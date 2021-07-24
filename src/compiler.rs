// analog to wren_compiler.c from wren_c.

use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::ops::Range;
use std::rc::Rc;
use std::str;

use crate::vm::{
    new_handle, wren_define_variable, DefinitionError, Module, ModuleLimitError, ObjClosure, ObjFn,
    SymbolTable, Value, WrenVM,
};

// Maximum times grammar is allowed to recurse through parse_precedence
// used to turn fuzz test cases (like 100x '[' in a row) into errors
// rather than stack overflows.
const MAX_GRAMMAR_NESTING: usize = 64;

// The maximum name of a method, not including the signature. This is an
// arbitrary but enforced maximum just so we know how long the method name
// strings need to be in the parser.
const MAX_METHOD_NAME: usize = 64;

// The maximum length of a method signature. Signatures look like:
//
//     foo        // Getter.
//     foo()      // No-argument method.
//     foo(_)     // One-argument method.
//     foo(_,_)   // Two-argument method.
//     init foo() // Constructor initializer.
//
// The maximum signature length takes into account the longest method name, the
// maximum number of parameters with separators between them, "init ", and "()".
// This is for buffer pre-allocation, rather than limit enforcement.
// const MAX_METHOD_SIGNATURE: usize = MAX_METHOD_NAME + (MAX_PARAMETERS * 2) + 6;

// The maximum length of an identifier.  This isn't needed in wren_rust, but
// is kept for consistency with wren_c for now.
pub(crate) const MAX_VARIABLE_NAME: usize = 64;

// This is written in bottom-up order, so the tokenization comes first, then
// parsing/code generation. This minimizes the number of explicit forward
// declarations needed.

// The maximum number of local (i.e. not module level) variables that can be
// declared in a single function, method, or chunk of top level code. This is
// the maximum number of variables in scope at one time, and spans block scopes.
const MAX_LOCALS: usize = 256;

// The maximum number of upvalues (i.e. variables from enclosing functions)
// that a function can close over.
// const MAX_UPVALUES: usize = 256;

// The maximum number of distinct constants that a function can contain.
const MAX_CONSTANTS: usize = 1 << 16;

// The maximum distance a Ops::Jump or Ops::JumpIfFalse instruction can move the
// instruction pointer.
// const MAX_JUMP: usize = 1 << 16;

// The maximum depth that interpolation can nest. For example, this string has
// three levels:
//
//      "outside %(one + "%(two + "%(three)")")"
const MAX_INTERPOLATION_NESTING: usize = 8;

// Token lifetimes should be tied to the Parser or InputManager.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    BeforeFile, // Error state, should never be encountered.
    LeftParen,
    RightParen,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftSquareBracket,
    RightSquareBracket,
    Minus,
    Plus,
    OpFactor(char),
    Num(f64),
    Hash,
    Pipe,
    PipePipe,
    Amp,
    AmpAmp,
    Bang,
    BangEquals,
    Dot,
    DotDot,
    DotDotDot,
    Comma,
    Caret,
    Colon,
    Tilde,
    Question,
    Class,
    GreaterThan,
    GreaterThanGreaterThan,
    LessThan,
    LessThanLessThan,
    GreaterThanEquals,
    LessThanEquals,
    Null,
    Var,
    While,
    Break,
    Continue,
    Return,
    This,
    Foreign,
    Static,
    Import,
    As,
    For,
    In,
    Is,
    If,
    Else,
    Construct,
    Equals,
    EqualsEquals,
    // A portion of a string literal preceding an interpolated expression. This
    // string:
    //
    //     "a %(b) c %(d) e"
    //
    // is tokenized to:
    //
    //     TOKEN_INTERPOLATION "a "
    //     TOKEN_NAME          b
    //     TOKEN_INTERPOLATION " c "
    //     TOKEN_NAME          d
    //     TOKEN_STRING        " e"
    Interpolation(String),
    Boolean(bool),
    Name(String),
    // Field and StaticField have separate Grammar rules, hence separate.
    Field(String),
    StaticField(String),
    String(String),
    Newline,
    EndOfFile,
}

#[derive(Debug)]
pub struct ParseToken {
    pub bytes_range: Range<usize>,
    pub token: Token,
    pub line: usize,
}

impl ParseToken {
    // FIXME: This should return a &str
    pub fn name(&self, input: &InputManager) -> String {
        // If we decoded it to parse it, we can decode it again w/o error.
        String::from_utf8(input.source[self.bytes_range.clone()].into()).unwrap()
    }

    fn before_file() -> ParseToken {
        ParseToken {
            bytes_range: Range::default(),
            token: Token::BeforeFile,
            line: 0,
        }
    }
}

// Is this really the tokenizer?
// Should have an input stream which it can pull (and cache) from
// And then the ability to pull more when needed and look-ahead when needed.
// Keeps track of when the current token starts, and knows how to start a new.
pub struct InputManager {
    source: Vec<u8>,
    offset: usize,
    // You never want to read this through parser.input.line_number
    // As the parser reads ahead a couple tokens.
    // Rather you probably want parser.previous.line
    line_number: usize,
    token_start_offset: usize,

    // Tracks the lexing state when tokenizing interpolated strings.
    //
    // Interpolated strings make the lexer not strictly regular: we don't know
    // whether a ")" should be treated as a RIGHT_PAREN token or as ending an
    // interpolated expression unless we know whether we are inside a string
    // interpolation and how many unmatched "(" there are. This is particularly
    // complex because interpolation can nest:
    //
    //     " %( " %( inner ) " ) "
    //
    // This tracks that state. The parser maintains a stack of ints, one for each
    // level of current interpolation nesting. Each value is the number of
    // unmatched "(" that are waiting to be closed.
    // FIXME: This belongs on Parser instead?
    parens: Vec<usize>,
}

impl InputManager {
    pub fn from_string(source: String) -> InputManager {
        InputManager::from_bytes(source.as_bytes().to_vec())
    }
    pub fn from_str(source: &str) -> InputManager {
        InputManager::from_bytes(source.as_bytes().to_vec())
    }
    pub fn from_bytes(source: Vec<u8>) -> InputManager {
        let start_offset = if source.starts_with(&[0xEF, 0xBB, 0xBF]) {
            3
        } else {
            0
        };

        InputManager {
            source: source,
            offset: start_offset,
            line_number: 1,
            token_start_offset: start_offset,
            parens: Vec::new(),
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.offset < self.source.len() {
            Some(self.source[self.offset])
        } else {
            None
        }
    }

    fn peek_is(&self, expected: u8) -> bool {
        match self.peek() {
            Some(c) => c == expected,
            None => false,
        }
    }

    fn peek_is_fn(&self, precicate: fn(u8) -> bool) -> bool {
        match self.peek() {
            Some(c) => precicate(c),
            None => false,
        }
    }

    fn peek_next(&self) -> Option<u8> {
        if self.offset + 1 < self.source.len() {
            Some(self.source[self.offset + 1])
        } else {
            None
        }
    }

    fn peek_next_is(&self, expected: u8) -> bool {
        match self.peek_next() {
            Some(c) => c == expected,
            None => false,
        }
    }

    fn peek_next_is_fn(&self, precicate: fn(u8) -> bool) -> bool {
        match self.peek_next() {
            Some(c) => precicate(c),
            None => false,
        }
    }

    fn next_checked(&mut self) -> Option<u8> {
        if self.offset < self.source.len() {
            let val = self.source[self.offset];
            if val == b'\n' {
                self.line_number += 1;
            }
            self.offset += 1;
            Some(val)
        } else {
            None
        }
    }

    // FIXME: Move to next_checked instead, or at least
    // rename this to next_unchecked.
    fn next(&mut self) -> u8 {
        let val = self.source[self.offset];
        if val == b'\n' {
            self.line_number += 1;
        }
        self.offset += 1;
        return val;
    }

    fn skip_while(&mut self, precicate: fn(u8) -> bool) {
        while let Some(val) = self.peek() {
            if !precicate(val) {
                break;
            }
            self.next();
        }
    }

    fn make_token(&self, token: Token) -> ParseToken {
        // Report Newline tokens as the line they are ending.
        let line_number = if token == Token::Newline {
            self.line_number - 1
        } else {
            self.line_number
        };
        ParseToken {
            line: line_number,
            token: token,
            bytes_range: self.token_start_offset..self.offset,
        }
    }

    fn is_at_end(&self) -> bool {
        self.offset >= self.source.len()
    }

    fn interpolation_allowed(&self) -> bool {
        self.parens.len() < MAX_INTERPOLATION_NESTING
    }

    fn count_open_parens(&mut self) {
        // If we are inside an interpolated expression, count the unmatched "(".
        if let Some(last) = self.parens.last_mut() {
            *last += 1;
        }
    }

    fn count_close_parens(&mut self) -> bool {
        // If we are inside an interpolated expression, count the ")".
        if let Some(last) = self.parens.last_mut() {
            *last -= 1;
            if *last != 0 {
                return false;
            }
        } else {
            return false;
        }
        // This is the final ")", so the interpolation expression has ended.
        // This ")" now begins the next section of the template string.
        self.parens.pop();
        return true;
    }
}

#[derive(Debug, Clone)]
pub enum LexError {
    UnexpectedChar(char),
    // Perhaps there is a better way to wrap these errors?
    SliceDecoderError(std::str::Utf8Error),
    StringDecoderError(std::string::FromUtf8Error),
    IntegerParsingError(std::num::ParseIntError),
    FloatParsingError(std::num::ParseFloatError),
    UnterminatedString,
    UnterminatedBlockComment,
    UnterminatedRawString,
    Error(String),
}

impl LexError {
    // Rename to error_str?
    pub(crate) fn from_str(msg: &str) -> LexError {
        LexError::Error(msg.into())
    }

    // Rename to error_string?
    pub(crate) fn from_string(msg: String) -> LexError {
        LexError::Error(msg)
    }
}

impl From<std::string::FromUtf8Error> for LexError {
    fn from(err: std::string::FromUtf8Error) -> LexError {
        LexError::StringDecoderError(err)
    }
}

impl From<std::str::Utf8Error> for LexError {
    fn from(err: std::str::Utf8Error) -> LexError {
        LexError::SliceDecoderError(err)
    }
}

impl From<std::num::ParseIntError> for LexError {
    fn from(err: std::num::ParseIntError) -> LexError {
        LexError::IntegerParsingError(err)
    }
}

impl From<std::num::ParseFloatError> for LexError {
    fn from(err: std::num::ParseFloatError) -> LexError {
        LexError::FloatParsingError(err)
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::SliceDecoderError(e) => write!(f, "Slice decoding {}", e),
            LexError::StringDecoderError(e) => write!(f, "String decoding {}", e),
            LexError::IntegerParsingError(e) => write!(f, "Integer parsing {}", e),
            LexError::FloatParsingError(e) => write!(f, "Float parsing {}", e),
            LexError::UnterminatedString => write!(f, "Unterminated string."),
            LexError::UnterminatedRawString => write!(f, "Unterminated raw string."),
            LexError::UnterminatedBlockComment => write!(f, "Unterminated block comment."),
            LexError::UnexpectedChar(c) => write!(f, "Unexpected char '{}'", c),
            LexError::Error(s) => write!(f, "{}", s),
        }
    }
}

impl error::Error for LexError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            LexError::StringDecoderError(e) => Some(e),
            LexError::SliceDecoderError(e) => Some(e),
            LexError::IntegerParsingError(e) => Some(e),
            LexError::FloatParsingError(e) => Some(e),
            LexError::UnterminatedString => None,
            LexError::UnterminatedRawString => None,
            LexError::UnterminatedBlockComment => None,
            LexError::UnexpectedChar(_) => None,
            LexError::Error(_) => None,
        }
    }
}

fn match_char(input: &mut InputManager, c: u8) -> bool {
    if let Some(next) = input.peek() {
        if next == c {
            input.next();
            return true;
        }
    }
    false
}

fn skip_line_comment(input: &mut InputManager) {
    while let Some(next) = input.peek() {
        if next == b'\n' {
            return;
        }
        input.next();
    }
}

fn skip_block_comment(input: &mut InputManager) -> Result<(), LexError> {
    let mut nesting = 1;
    while nesting > 0 {
        match input.peek() {
            Some(b'/') if input.peek_next() == Some(b'*') => {
                input.next();
                input.next();
                nesting += 1;
            }
            Some(b'*') if input.peek_next() == Some(b'/') => {
                input.next();
                input.next();
                nesting -= 1;
            }
            Some(_) => {
                input.next();
            }
            None => return Err(LexError::UnterminatedBlockComment),
        }
    }
    Ok(())
}

// Takes an InputManager (from which it gets source)
// Produces tokens one at a time.  Keeps reference to current and next
// as well as can look-ahead (and cache in a buffer if needed)?
// Knows if it's at the end of the file.
// struct Tokenizer {}

fn is_whitespace(maybe_b: Option<u8>) -> bool {
    if let Some(b) = maybe_b {
        return b == b' ' || b == b'\t' || b == b'\r';
    }
    return false;
}

fn two_char_token(
    input: &mut InputManager,
    second_byte: u8,
    two_token: Token,
    one_token: Token,
) -> ParseToken {
    let token = match input.peek() {
        Some(byte) if byte == second_byte => {
            input.next();
            two_token
        }
        _ => one_token,
    };
    input.make_token(token)
}

// Probably belongs on the InputManager/Tokenizer?
fn next_token(input: &mut InputManager) -> Result<ParseToken, LexError> {
    while !input.is_at_end() {
        // Reset token start at top of loop to avoid counting leading whitespace
        input.token_start_offset = input.offset;
        let c = input.next();
        match c {
            b'0' => {
                let value = if input.peek_is(b'x') {
                    read_hex_number(input)?
                } else {
                    read_number(input)?
                };
                return Ok(input.make_token(Token::Num(value)));
            }
            b'1'..=b'9' => {
                let value = read_number(input)?;
                return Ok(input.make_token(Token::Num(value)));
            }
            b'.' => {
                if match_char(input, b'.') {
                    let matched_third_dot = match_char(input, b'.');
                    if matched_third_dot {
                        return Ok(input.make_token(Token::DotDotDot));
                    } else {
                        return Ok(input.make_token(Token::DotDot));
                    }
                }
                return Ok(input.make_token(Token::Dot));
            }
            b',' => return Ok(input.make_token(Token::Comma)),
            b'!' => return Ok(two_char_token(input, b'=', Token::BangEquals, Token::Bang)),
            b'|' => return Ok(two_char_token(input, b'|', Token::PipePipe, Token::Pipe)),
            b'&' => return Ok(two_char_token(input, b'&', Token::AmpAmp, Token::Amp)),
            b'+' => return Ok(input.make_token(Token::Plus)),
            b'-' => return Ok(input.make_token(Token::Minus)),
            b'^' => return Ok(input.make_token(Token::Caret)),
            b'~' => return Ok(input.make_token(Token::Tilde)),
            b':' => return Ok(input.make_token(Token::Colon)),
            b'?' => return Ok(input.make_token(Token::Question)),
            b'*' | b'%' => return Ok(input.make_token(Token::OpFactor(c.into()))),
            b'<' => {
                return Ok(if match_char(input, b'<') {
                    input.make_token(Token::LessThanLessThan)
                } else {
                    two_char_token(input, b'=', Token::LessThanEquals, Token::LessThan)
                });
            }
            b'>' => {
                return Ok(if match_char(input, b'>') {
                    input.make_token(Token::GreaterThanGreaterThan)
                } else {
                    two_char_token(input, b'=', Token::GreaterThanEquals, Token::GreaterThan)
                });
            }
            b'(' => {
                input.count_open_parens();
                return Ok(input.make_token(Token::LeftParen));
            }
            b')' => {
                if input.count_close_parens() {
                    return read_string(input);
                }
                return Ok(input.make_token(Token::RightParen));
            }
            b'{' => return Ok(input.make_token(Token::LeftCurlyBrace)),
            b'}' => return Ok(input.make_token(Token::RightCurlyBrace)),
            b'[' => return Ok(input.make_token(Token::LeftSquareBracket)),
            b']' => return Ok(input.make_token(Token::RightSquareBracket)),
            b' ' | b'\t' | b'\r' => {
                while is_whitespace(input.peek()) {
                    input.next();
                }
            }
            b'/' => {
                if match_char(input, b'/') {
                    skip_line_comment(input);
                    continue;
                }
                if match_char(input, b'*') {
                    skip_block_comment(input)?;
                    continue;
                }
                return Ok(input.make_token(Token::OpFactor('/')));
            }
            b'#' => {
                // Ignore shebang on the first line.
                if input.line_number == 1 && input.peek_is(b'!') && input.peek_next_is(b'/') {
                    skip_line_comment(input);
                    break;
                }
                // Otherwise we treat it as a token
                return Ok(input.make_token(Token::Hash));
            }
            b'=' => {
                return Ok(two_char_token(
                    input,
                    b'=',
                    Token::EqualsEquals,
                    Token::Equals,
                ))
            }
            b'"' => {
                if input.peek_is(b'"') && input.peek_next_is(b'"') {
                    return read_raw_string(input);
                }
                return read_string(input);
            }
            // Note this does not have _ like "is_name" does.
            b'a'..=b'z' | b'A'..=b'Z' => {
                let name = read_name(c, input)?;
                return Ok(input.make_token(keyword_token(&name).unwrap_or(Token::Name(name))));
            }
            // Starting with _ is a Field, rather than Name.
            b'_' => {
                let is_static_field = input.peek_is(b'_');
                let name = read_name(c, input)?;
                return Ok(input.make_token(if is_static_field {
                    Token::StaticField(name)
                } else {
                    Token::Field(name)
                }));
            }
            b'\n' => return Ok(input.make_token(Token::Newline)),
            _ => {
                return Err(LexError::UnexpectedChar(c as char));
            }
        }
    }
    return Ok(input.make_token(Token::EndOfFile));
}

fn make_number(input: &InputManager, is_hex: bool) -> Result<f64, LexError> {
    let name = str::from_utf8(&input.source[input.token_start_offset..input.offset])?;

    let result = if is_hex {
        let without_prefix = name.trim_start_matches("0x");
        u32::from_str_radix(without_prefix, 16).map_err(|e| LexError::IntegerParsingError(e))?
            as f64
    } else {
        name.parse::<f64>()
            .map_err(|e| LexError::FloatParsingError(e))?
    };
    Ok(result)
}

// Reads the next character, which should be a hex digit (0-9, a-f, or A-F) and
// returns its numeric value. If the character isn't a hex digit, returns -1.
fn read_hex_digit(input: &mut InputManager) -> Option<u8> {
    // FIXME: Can't this just be input.next() and all the other
    // input.next() become "b"?
    match input.peek() {
        Some(b) => match b {
            b'0'..=b'9' => Some(input.next() - b'0'),
            b'a'..=b'f' => Some(input.next() - b'a' + 10),
            b'A'..=b'F' => Some(input.next() - b'A' + 10),
            _ => None,
        },
        _ => None,
    }
}

// Finishes lexing a hexadecimal number literal.
fn read_hex_number(input: &mut InputManager) -> Result<f64, LexError> {
    // Skip past the `x` used to denote a hexadecimal literal.
    input.next();

    // Iterate over all the valid hexadecimal digits found.
    while let Some(_) = read_hex_digit(input) {}

    make_number(input, true)
}

// Knows how to advance to the end of something that looks like a number
// and then turn that into a token.
// Belongs on the Tokenizer/InputManager.
fn read_number(input: &mut InputManager) -> Result<f64, LexError> {
    fn is_digit(b: u8) -> bool {
        b.is_ascii_digit()
    }

    input.skip_while(is_digit);

    // See if it has a floating point. Make sure there is a digit after the "."
    // so we don't get confused by method calls on number literals.
    if input.peek_is(b'.') && input.peek_next_is_fn(is_digit) {
        input.next();
        input.skip_while(is_digit);
    }
    // See if the number is in scientific notation.
    if match_char(input, b'e') || match_char(input, b'E') {
        // Allow a single positive/negative exponent symbol.
        if !match_char(input, b'+') {
            match_char(input, b'-');
        }
        if !input.peek_is_fn(is_digit) {
            return Err(LexError::from_str("Unterminated scientific notation."));
        }
        input.skip_while(is_digit);
    }

    make_number(input, false)
}

fn keyword_token(name: &str) -> Option<Token> {
    // FIXME: Hack until TokenType is separate from Token?
    match name {
        "true" => Some(Token::Boolean(true)),
        "false" => Some(Token::Boolean(false)),
        "var" => Some(Token::Var),
        "while" => Some(Token::While),
        "break" => Some(Token::Break),
        "continue" => Some(Token::Continue),
        "return" => Some(Token::Return),
        "null" => Some(Token::Null),
        "class" => Some(Token::Class),
        "is" => Some(Token::Is),
        "for" => Some(Token::For),
        "in" => Some(Token::In),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "construct" => Some(Token::Construct),
        "foreign" => Some(Token::Foreign),
        "static" => Some(Token::Static),
        "import" => Some(Token::Import),
        "as" => Some(Token::As),
        "this" => Some(Token::This),
        _ => None,
    }
}

fn is_name(c: Option<u8>) -> bool {
    matches!(c, Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'_'))
}

fn is_digit(c: Option<u8>) -> bool {
    matches!(c, Some(b'0'..=b'9'))
}

fn read_name(first_byte: u8, input: &mut InputManager) -> Result<String, LexError> {
    // This should be a string?
    let mut bytes = Vec::new();
    bytes.push(first_byte);
    while is_name(input.peek()) || is_digit(input.peek()) {
        bytes.push(input.next());
    }

    Ok(String::from_utf8(bytes)?)
}

// Reads [digits] hex digits in a string literal and returns their
// number value as a char.
// FIXME: This should probably return an Option and the caller
// can make the LexError in one place instead of 3.
fn read_escape(
    input: &mut InputManager,
    digits: usize,
    description: &str,
) -> Result<char, LexError> {
    let mut value = 0u32;
    for _ in 0..digits {
        match input.peek() {
            Some(b'"') | None => {
                return Err(LexError::Error(format!(
                    "Incomplete {} escape sequence.",
                    description
                )));
            }
            Some(_) => {
                let digit = read_hex_digit(input).ok_or_else(|| {
                    LexError::Error(format!("Invalid {} escape sequence.", description))
                })?;
                value = value * 16 + &digit.into();
            }
        }
    }
    Ok(char::from_u32(value)
        .ok_or_else(|| LexError::Error(format!("Invalid {} escape sequence.", description)))?)
}

fn read_raw_string(input: &mut InputManager) -> Result<ParseToken, LexError> {
    //consume the second and third "
    input.next();
    input.next();

    let mut string = String::new();

    let mut skip_start = Some(0);
    let mut first_newline = None;
    let mut skip_end = None;
    let mut last_newline = None;

    loop {
        // FIXME: We probably don't need to check all 3 every time?
        let c = input
            .next_checked()
            .ok_or_else(|| LexError::UnterminatedRawString)?;
        let c1 = input
            .peek()
            .ok_or_else(|| LexError::UnterminatedRawString)?;
        let c2 = input
            .peek_next()
            .ok_or_else(|| LexError::UnterminatedRawString)?;

        if c == b'\r' {
            continue;
        }

        let c_is_newline = c == b'\n';
        if c_is_newline {
            last_newline = Some(string.len());
            skip_end = last_newline;
            if first_newline.is_none() {
                first_newline = Some(string.len());
            }
        }

        // We found the end of the raw string.
        if c == b'"' && c1 == b'"' && c2 == b'"' {
            break;
        }

        let c_is_whitespace = c == b' ' || c == b'\t';
        if !c_is_newline && !c_is_whitespace {
            skip_end = None;
        }

        // If we haven't seen a newline or other character yet,
        // and still seeing whitespace, count the characters
        // as skippable till we know otherwise
        let skippable = skip_start.is_some() && c_is_whitespace && first_newline.is_none();
        if skippable {
            skip_start = Some(string.len() + 1);
        }

        // We've counted leading whitespace till we hit something else,
        // but it's not a newline, so we reset skipStart since we need these characters
        if first_newline.is_none() && !c_is_whitespace && !c_is_newline {
            skip_start = None;
        }

        string.push(char::from_u32(c.into()).unwrap());
    }

    //consume the second and third "
    input.next();
    input.next();

    let mut offset = 0;
    let mut count = string.len();

    if first_newline.is_some() && skip_start == first_newline {
        offset = first_newline.unwrap() + 1;
    }
    if last_newline.is_some() && skip_end == last_newline {
        count = last_newline.unwrap();
    }
    if offset > count {
        count = 0;
    } else {
        count -= offset;
    }

    Ok(input.make_token(Token::String(string[offset..offset + count].into())))
}

fn read_string(input: &mut InputManager) -> Result<ParseToken, LexError> {
    // FIXME: Is there not a simpler way to do this?
    fn push_char(bytes: &mut Vec<u8>, c: char) {
        let mut new_bytes = [0u8; 4];
        let string = c.encode_utf8(&mut new_bytes);
        bytes.extend_from_slice(string.as_bytes());
    }

    let mut bytes = Vec::new();
    let mut is_interpolation = false;
    loop {
        if input.is_at_end() {
            return Err(LexError::UnterminatedString);
        }
        let next = input.next();
        if next == b'"' {
            break;
        }
        // Ignore carriage returns on windows.
        // FIXME: Needs a test.
        // if next == b'\r' {
        //     continue;
        // }

        if next == b'%' {
            if input.interpolation_allowed() {
                // TODO: Allow format string.
                if input.is_at_end() || input.next() != b'(' {
                    return Err(LexError::from_str("Expect '(' after '%%'."));
                }

                input.parens.push(1);

                is_interpolation = true;
                break;
            }

            return Err(LexError::from_string(format!(
                "Interpolation may only nest {} levels deep.",
                MAX_INTERPOLATION_NESTING
            )));
        }

        if next == b'\\' {
            // FIXME: This should be part of the match instead.
            // input next() should return Option.
            if input.is_at_end() {
                return Err(LexError::UnterminatedString);
            }
            match input.next() {
                b'"' => bytes.push(b'"'),
                b'\\' => bytes.push(b'\\'),
                b'%' => bytes.push(b'%'),
                b'0' => bytes.push(b'\0'),
                b'a' => bytes.push(b'\x07'),
                b'b' => bytes.push(b'\x08'),
                b'e' => bytes.push(b'\x1b'),
                b'f' => bytes.push(b'\x0c'),
                b'n' => bytes.push(b'\n'),
                b'r' => bytes.push(b'\r'),
                b't' => bytes.push(b'\t'),
                b'u' => push_char(&mut bytes, read_escape(input, 4, "Unicode")?),
                b'U' => push_char(&mut bytes, read_escape(input, 8, "Unicode")?),
                b'v' => bytes.push(b'\x0b'),
                b'x' => push_char(&mut bytes, read_escape(input, 2, "byte")?),

                b => {
                    return Err(LexError::Error(format!(
                        "Invalid escape character '{}'.",
                        b
                    )))
                }
            }
        } else {
            bytes.push(next);
        }
    }
    let string = String::from_utf8(bytes)?;

    Ok(input.make_token(if is_interpolation {
        Token::Interpolation(string)
    } else {
        Token::String(string)
    }))
}

pub fn lex(input: &mut InputManager) -> Result<Vec<ParseToken>, WrenError> {
    let input_manager = input;
    let mut tokens = Vec::new();
    loop {
        let token = next_token(input_manager).map_err(|e| WrenError {
            module: "dummy".into(),
            line: input_manager.line_number,
            error: ParserError::Lexer(e),
        })?;
        let is_eof = token.token == Token::EndOfFile;
        tokens.push(token);
        if is_eof {
            break;
        }
    }
    Ok(tokens)
}

// NOTE: Keep in sync with Precendence::one_higher!
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
enum Precedence {
    None = 0, // Newlines, EOF, etc.
    Lowest,
    Assignment,   // =
    Conditional,  // ?:
    LogicalOr,    // ||
    LogicalAnd,   // &&
    Equality,     // == !=
    Is,           // is
    Comparison,   // < > <= >=
    BitwiseOr,    // |
    BitwiseXor,   // ^
    BitwiseAnd,   // &
    BitwiseShift, // << >>
    Range,        // .. ...
    Term,         // + -
    Factor,       // * / %
    Unary,        // unary - ! ~
    Call,         // . () []
    Primary,
}

impl Precedence {
    // There are crates to convert from enums to numbers
    // but doing this manually to avoid any dependencies.
    fn one_higher(self) -> Precedence {
        match self {
            Precedence::None => Precedence::Lowest,
            Precedence::Lowest => Precedence::Assignment,
            Precedence::Assignment => Precedence::Conditional,
            Precedence::Conditional => Precedence::LogicalOr,
            Precedence::LogicalOr => Precedence::LogicalAnd,
            Precedence::LogicalAnd => Precedence::Equality,
            Precedence::Equality => Precedence::Is,
            Precedence::Is => Precedence::Comparison,
            Precedence::Comparison => Precedence::BitwiseOr,
            Precedence::BitwiseOr => Precedence::BitwiseXor,
            Precedence::BitwiseXor => Precedence::BitwiseAnd,
            Precedence::BitwiseAnd => Precedence::BitwiseShift,
            Precedence::BitwiseShift => Precedence::Range,
            Precedence::Range => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => panic!("Primary is the highest precedence!"),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Ops {
    Constant(usize),
    Boolean(bool), // Unclear if needed, could be constant?
    Null,          // Unclear if needed, could be constant?
    Call(Signature, usize),
    LoadField(usize),
    StoreField(usize),
    Load(Variable),
    Store(Variable),
    ClassPlaceholder,
    Class(usize),
    ForeignClass,
    Closure(usize, Vec<Upvalue>),
    Construct,
    ForeignConstruct,
    Method(bool, usize), // METHOD_STATIC and METHOD_INSTANCE from wren_c

    ImportModule(String),
    ImportVariable(String),

    // If the top of the stack is false, jump [arg] forward. Otherwise, pop and
    // continue.
    And(u16),
    AndPlaceholder,

    // If the top of the stack is non-false, jump [arg] forward. Otherwise, pop
    // and continue.
    Or(u16),
    OrPlaceholder,

    JumpIfFalse(u16), // Pop stack, if truthy, Jump forward relative offset.
    JumpIfFalsePlaceholder,
    Jump(u16), // Jump forward relative offset.
    JumpPlaceholder,

    Loop(u16), // Jump backwards relative offset.
    Pop,
    Return,
    EndModule,
    End,
}

#[derive(Debug)]
struct Local {
    name: String,
    depth: ScopeDepth,
}

#[derive(Copy, Clone, PartialEq, Debug, Eq)]
enum ScopeDepth {
    Module,
    Local(usize),
}

impl Ord for ScopeDepth {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (ScopeDepth::Module, ScopeDepth::Module) => Ordering::Equal,
            (ScopeDepth::Module, ScopeDepth::Local(_)) => Ordering::Less,
            (ScopeDepth::Local(_), ScopeDepth::Module) => Ordering::Greater,
            (ScopeDepth::Local(this), ScopeDepth::Local(that)) => this.cmp(that),
        }
    }
}

impl PartialOrd for ScopeDepth {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl ScopeDepth {
    fn one_deeper(&self) -> ScopeDepth {
        match self {
            ScopeDepth::Local(depth) => ScopeDepth::Local(depth + 1),
            ScopeDepth::Module => ScopeDepth::Local(0),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Upvalue {
    // True if this upvalue is capturing a local variable from the enclosing
    // function. False if it's capturing an upvalue.
    is_local: bool,

    // The index of the local or upvalue being captured in the enclosing function.
    index: usize,
}

// Bookkeeping information for compiling a class definition.
#[derive(Default)]
struct ClassInfo {
    // The name of the class.
    // name: String,
    //   // Attributes for the class itself
    //   ObjMap* classAttributes;
    //   // Attributes for methods in this class
    //   ObjMap* methodAttributes;

    // // Symbol table for the fields of the class.
    fields: SymbolTable,

    // // Symbols for the methods defined by the class. Used to detect duplicate
    // // method definitions.
    // methods: Vec<usize>,
    // static_methods: Vec<usize>,

    // True if the class being compiled is a foreign class.
    is_foreign_class: bool,

    // // True if the current method being compiled is static.
    in_static_method: bool,
    // The signature of the method being compiled.
    //   Signature* signature;
}

impl ClassInfo {
    fn new(_name: &str, is_foreign_class: bool) -> ClassInfo {
        ClassInfo {
            // name: name.into(),
            fields: SymbolTable::default(),
            // methods: Vec::new(),
            // static_methods: Vec::new(),
            is_foreign_class: is_foreign_class,
            in_static_method: false,
        }
    }
}

#[derive(Default)]
pub(crate) struct FnDebug {
    // The name of the function.
    pub(crate) name: String,
    // An array of line numbers. There is one element in this array for each
    // bytecode in the function's bytecode array. The value of that element is
    // the line in the source code that generated that instruction.
    source_lines: Vec<usize>,
    pub(crate) from_core_module: bool,
}

impl FnDebug {
    pub(crate) fn line_for_pc(&self, pc: usize) -> usize {
        self.source_lines[pc]
    }
}

// wren_c uses both a hash and a list when building constants
// to allow both fast lookup (when there are lots of constants)
// as well as fast copy from the compiler into the final function.
// wren_c allows up to 65k constants.
pub(crate) struct ConstantsBuilder {
    hash: HashMap<Value, usize>,

    // public for deconstruction in end_compiler.
    pub(crate) list: Vec<Value>,
}

impl ConstantsBuilder {
    fn new() -> ConstantsBuilder {
        ConstantsBuilder {
            hash: HashMap::new(),
            list: Vec::new(),
        }
    }

    fn lookup(&self, value: &Value) -> Option<&usize> {
        self.hash.get(value)
    }

    fn len(&self) -> usize {
        // self.list.len() and self.hash.len() should both be the same.
        self.list.len()
    }

    // This makes no attempt to check if it's already in the map
    // callers are expected to lookup first.
    fn add(&mut self, value: Value) -> usize {
        let index = self.list.len();
        self.list.push(value.clone());
        self.hash.insert(value, index);
        index
    }
}

impl fmt::Debug for ConstantsBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.list)
    }
}

// Only lives for the function (or module top) compile.
// Keep a stack of compilers as we recruse the tree.
pub(crate) struct Compiler {
    // Public for deconstruction by ObjFn::new()
    pub(crate) constants: ConstantsBuilder,
    locals: Vec<Local>, // A fixed size array in wren_c
    pub(crate) code: Vec<Ops>,
    scope_depth: ScopeDepth,
    loops: Vec<LoopOffsets>, // wren_c uses stack-allocated objects instead.

    upvalues: Vec<Upvalue>,
    parent: Option<Box<Compiler>>,

    enclosing_class: Option<RefCell<ClassInfo>>,

    // Unclear if this will be used?
    is_initializer: bool,
    // wren_c has a numSlots / maxSlots optimization for recording from
    // the compiler how much stack space a given function will need
    // This allows wren_c to pre-grow the Fiber's (shared) stack ahead
    // of fn execution and not have to check with each push/pop if it
    // needs to grow more.
    pub(crate) fn_debug: FnDebug,
}

impl Compiler {
    fn _with_parent_and_arg0_name(parent: Option<Box<Compiler>>, arg0_name: &str) -> Compiler {
        Compiler {
            constants: ConstantsBuilder::new(),
            locals: vec![Local {
                name: arg0_name.into(),
                // wren_c has a funny hack here by setting depth to -1 (module)
                // it makes a local which is never popped.  I suspect this can
                // be done differently.  It can't be 0 at the top level or the
                // first time we create a compiler we'll end up popping the
                // implicit arg0 closure for the module scope.
                // However "this" really is a local.  It's owned by the callee
                // and should be popped by the callee.
                depth: ScopeDepth::Module,
            }],
            // locals: Vec::new(),
            code: Vec::new(),
            scope_depth: match parent {
                None => ScopeDepth::Module,
                Some(_) => ScopeDepth::Local(0),
            },
            loops: Vec::new(),
            // num_slots: 0,
            enclosing_class: None,
            parent: parent,
            upvalues: Vec::new(),
            is_initializer: false, // Should this be tracked here?
            fn_debug: FnDebug::default(),
        }
    }

    fn block(parent: Option<Box<Compiler>>) -> Compiler {
        Compiler::_with_parent_and_arg0_name(parent, "")
    }

    // Parent isn't actually optional.
    fn method(parent: Option<Box<Compiler>>) -> Compiler {
        Compiler::_with_parent_and_arg0_name(parent, "this")
    }

    fn nested_local_scope_count(&self) -> usize {
        match self.scope_depth {
            ScopeDepth::Module => panic!("No local scopes."),
            ScopeDepth::Local(i) => i,
        }
    }

    fn emit_op_for_line(&mut self, op: Ops, line: usize) -> usize {
        self.code.push(op);
        self.fn_debug.source_lines.push(line);
        assert_eq!(self.code.len(), self.fn_debug.source_lines.len());
        self.code.len() - 1
    }

    // Adds an upvalue to [compiler]'s function with the given properties. Does not
    // add one if an upvalue for that variable is already in the list. Returns the
    // index of the upvalue.
    // fn add_upvalue(&mut self, is_local: bool, index: usize) -> usize {
    //     // Look for an existing one.
    //     for (i, upvalue) in self.upvalues.iter().enumerate() {
    //         if upvalue.is_local == is_local && upvalue.index == index {
    //             return i;
    //         }
    //     }

    //     // If we got here, it's a new upvalue.
    //     self.upvalues.push(Upvalue {
    //         is_local: is_local,
    //         index: index,
    //     });
    //     self.upvalues.len() - 1
    // }
}

fn emit_constant(ctx: &mut ParseContext, value: Value) -> Result<(), WrenError> {
    let index = ensure_constant(ctx, value)?;
    emit(ctx, Ops::Constant(index));
    Ok(())
}

fn emit(ctx: &mut ParseContext, op: Ops) -> usize {
    let line = ctx.parser.previous.line;
    ctx.compiler_mut().emit_op_for_line(op, line)
}

fn emit_loop(ctx: &mut ParseContext, offsets: &LoopOffsets) {
    // Emit a loop instruction which jumps to start of current loop.
    // Measures from *after* start and doesn't include this Loop, so +2.
    let backwards_by = ctx
        .compiler()
        .offset_to_current_pc_from_after(offsets.start)
        + 2;

    emit(ctx, Ops::Loop(backwards_by));
}

fn push_scope(ctx: &mut ParseContext) {
    ctx.compiler_mut().scope_depth = ctx.compiler().scope_depth.one_deeper()
}
fn pop_scope(ctx: &mut ParseContext) {
    let popped = emit_pops_for_locals(ctx, ctx.compiler().scope_depth);
    let locals_len = ctx.compiler().locals.len();
    ctx.compiler_mut().locals.truncate(locals_len - popped);
    // self.num_slots -= popped;

    ctx.compiler_mut().scope_depth = match ctx.compiler_mut().scope_depth {
        ScopeDepth::Module => panic!("Can't pop from module scope!"),
        ScopeDepth::Local(i) => {
            if i == 0 {
                ScopeDepth::Module
            } else {
                ScopeDepth::Local(i - 1)
            }
        }
    }
}

impl fmt::Debug for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
            .field(&self.constants)
            .field(&self.code)
            .finish()
    }
}

// Adds [constant] to the constant pool and returns its index.
// wren_c calls this addConstant
fn ensure_constant(ctx: &mut ParseContext, constant: Value) -> Result<usize, WrenError> {
    if let Some(index) = ctx.compiler().constants.lookup(&constant) {
        return Ok(*index);
    } else if ctx.compiler().constants.len() < MAX_CONSTANTS {
        Ok(ctx.compiler_mut().constants.add(constant))
    } else {
        Err(ctx.error_string(format!(
            "A function may only contain {} unique constants.",
            MAX_CONSTANTS
        )))
    }
}

// Takes an InputManager.  Knows how to use a Tokenizer to break it up into
// tokens one at a time.  Turns a stream of tokens into a tree of objects.
// Module / Function / Closure are likely the eventual objects?
// Parser has the lifetime of a given compilation of a module.
struct Parser {
    input: InputManager,
    previous: ParseToken,
    current: ParseToken,
    next: ParseToken,
}

struct ParseContext<'a> {
    parser: Parser,
    _compiler: Option<Box<Compiler>>,
    vm: &'a mut WrenVM,
    // We have the module during parse-time in order to be able to
    // make ObjFn objects (which hold a pointer to the module) as
    // well as be able to look-up and define variables on the module.
    // IIRC, we only ever define functions on the module during parse time
    // all other variables are placeholders.
    module: Handle<Module>,
    nesting: usize,
}

impl<'a> ParseContext<'a> {
    fn module_name(&self) -> String {
        self.module.borrow().name.clone()
    }

    fn in_core_module(&self) -> bool {
        self.module.borrow().name.eq(crate::vm::CORE_MODULE_NAME)
    }

    fn parse_error(&self, error: ParserError) -> WrenError {
        WrenError {
            line: self.parser.previous.line,
            module: self.module_name(),
            error: error,
        }
    }

    fn grammar_error(&self, label: &str, message: String) -> WrenError {
        WrenError {
            line: self.parser.previous.line,
            module: self.module_name(),
            error: ParserError::Grammar(format!("{}: {}", label, message)),
        }
    }

    fn error_str(&self, msg: &str) -> WrenError {
        self.error_string(msg.into())
    }

    fn label_for_token(&self, token: &Token) -> String {
        if token == &Token::Newline {
            "Error at newline".into()
        } else if token == &Token::EndOfFile {
            "Error at end of file".into()
        } else {
            let name = previous_token_name(self);
            // Match wren_c's variable name limits.
            if name.len() <= MAX_VARIABLE_NAME {
                format!("Error at '{}'", name)
            } else {
                format!("Error at '{:.*}...'", MAX_VARIABLE_NAME, name)
            }
        }
    }

    // Outputs a compile or syntax error.
    fn error_string(&self, msg: String) -> WrenError {
        let token = &self.parser.previous.token;
        let label = self.label_for_token(token);
        self.grammar_error(&label, msg)
    }

    fn compiler(&self) -> &Compiler {
        self._compiler.as_ref().unwrap().as_ref()
    }

    fn compiler_mut(&mut self) -> &mut Compiler {
        self._compiler.as_mut().unwrap().as_mut()
    }

    fn have_compiler(&self) -> bool {
        self._compiler.is_some()
    }

    fn inside_class_definition(&self) -> bool {
        let mut maybe_compiler = &self._compiler;
        while let Some(compiler) = maybe_compiler {
            if compiler.enclosing_class.is_some() {
                return true;
            }
            maybe_compiler = &compiler.parent;
        }
        return false;
    }

    fn call_with_enclosing_class<T, F>(&mut self, f: F) -> Result<T, WrenError>
    where
        F: Fn(&ParseContext, &Option<RefCell<ClassInfo>>) -> Result<T, WrenError>,
    {
        let mut maybe_compiler = &self._compiler;
        while let Some(compiler) = maybe_compiler {
            if compiler.enclosing_class.is_some() {
                return f(self, &compiler.enclosing_class);
            }
            maybe_compiler = &compiler.parent;
        }
        f(self, &None)
    }
}

// Following the Pratt parser example:
// https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type PrefixParslet = fn(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError>;
type InfixParslet = fn(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError>;
type MethodSignatureParslet =
    fn(ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError>;

fn literal(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    // TODO: Pass in Token instead of needing to use "previous"?
    let value = match &ctx.parser.previous.token {
        Token::Num(n) => Value::Num(*n),
        Token::String(s) => Value::from_str(s),
        Token::Interpolation(s) => Value::from_str(s),
        _ => {
            let name = previous_token_name(ctx);
            return Err(ctx.error_string(format!("Invalid literal {}", name)));
        }
    };
    emit_constant(ctx, value)?;
    Ok(())
}

// A string literal that contains interpolated expressions.
//
// Interpolation is syntactic sugar for calling ".join()" on a list. So the
// string:
//
//     "a %(b + c) d"
//
// is compiled roughly like:
//
//     ["a ", b + c, " d"].join()
fn string_interpolation(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    // Instantiate a new list.
    load_core_variable(ctx, "List");
    call_method(ctx, 0, "new()");

    loop {
        // The opening string part.
        literal(ctx, false)?;
        call_method(ctx, 1, "addCore_(_)");

        // The interpolated expression.
        ignore_newlines(ctx)?;
        expression(ctx)?;
        call_method(ctx, 1, "addCore_(_)");

        ignore_newlines(ctx)?;
        if !match_current_interpolation(ctx)? {
            break;
        }
    }

    // The trailing string part.
    consume_string(ctx, "Expect end of string interpolation.")?;
    literal(ctx, false)?;
    call_method(ctx, 1, "addCore_(_)");

    // The list of interpolated parts.
    call_method(ctx, 0, "join()");
    Ok(())
}

fn conditional(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    // Ignore newline after '?'.
    ignore_newlines(ctx)?;

    // Jump to the else branch if the condition is false.
    let if_jump = emit(ctx, Ops::JumpIfFalsePlaceholder);

    // Compile the then branch.
    parse_precendence(ctx, Precedence::Conditional)?;

    consume_expecting(
        ctx,
        Token::Colon,
        "Expect ':' after then branch of conditional operator.",
    )?;
    ignore_newlines(ctx)?;

    // Jump over the else branch when the if branch is taken.
    let else_jump = emit(ctx, Ops::JumpPlaceholder);

    // Compile the else branch.
    ctx.compiler_mut().patch_jump(if_jump);

    parse_precendence(ctx, Precedence::Assignment)?;

    // Patch the jump over the else.
    ctx.compiler_mut().patch_jump(else_jump);
    Ok(())
}

#[derive(Debug, PartialEq)]
pub(crate) enum SignatureType {
    Getter,
    Method,
    Subscript,
    SubscriptSetter,
    Setter,
    Initializer,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Signature {
    pub(crate) bare_name: String,
    // Not sure either of these are needed once the full name is compiled
    // It's possible this struct can go away.
    pub(crate) call_type: SignatureType,
    pub(crate) arity: u8,
}

impl Signature {
    // This is called signatureToString in wren_c
    // This is intentionally not pub(crate), the vm
    // should never call this, but rather lookup the
    // name from the symbol.
    fn full_name(&self) -> String {
        // FIXME: wren_c has special handling for SubscriptSetter.
        fn args(arity: u8) -> String {
            (0..arity).map(|_| "_").collect::<Vec<&str>>().join(",")
        }
        match self.call_type {
            SignatureType::Getter => self.bare_name.clone(),
            SignatureType::Setter => format!("{}={}", self.bare_name, args(self.arity)),
            SignatureType::Method => format!("{}({})", self.bare_name, args(self.arity)),
            SignatureType::Initializer => format!("init {}({})", self.bare_name, args(self.arity)),
            SignatureType::Subscript => format!("{}[{}]", self.bare_name, args(self.arity)), // name should always be empty
            SignatureType::SubscriptSetter => {
                format!("{}[{}]=(_)", self.bare_name, args(self.arity - 1))
            } // name should always be empty.
        }
    }
    fn from_bare_name(call_type: SignatureType, bare_name: &str, arity: u8) -> Signature {
        Signature {
            bare_name: bare_name.into(),
            call_type: call_type,
            arity: arity,
        }
    }
}

fn infix_op(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    let rule = ctx.parser.previous.token.grammar_rule();
    let name = previous_token_name(ctx);
    ignore_newlines(ctx)?;
    // Compile the right-hand side.
    parse_precendence(ctx, rule.precedence.one_higher())?;

    // Call the operator method on the left-hand side.
    // This could use signature_from_token, but since this is only called
    // for tokens with known name lengths wren_c skips it.
    let signature = Signature::from_bare_name(SignatureType::Method, &name, 1);
    call_signature(ctx, signature);
    Ok(())
}

// Compiles a method signature for an infix operator.
fn infix_signature(ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError> {
    // Add the RHS parameter.
    signature.call_type = SignatureType::Method;
    signature.arity = 1;

    // Parse the parameter name.
    consume_expecting(ctx, Token::LeftParen, "Expect '(' after operator name.")?;
    declare_named_variable(ctx)?;
    consume_expecting(ctx, Token::RightParen, "Expect ')' after parameter name.")?;
    Ok(())
}

// Compiles a method signature for an unary operator (i.e. "!").
fn unary_signature(_ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError> {
    // Do nothing. The name is already complete.
    signature.call_type = SignatureType::Getter;
    Ok(())
}

// Compiles a method signature for an operator that can either be unary or
// infix (i.e. "-").
fn mixed_signature(ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError> {
    signature.call_type = SignatureType::Getter;

    // If there is a parameter, it's an infix operator, otherwise it's unary.
    if match_current(ctx, Token::LeftParen)? {
        // Add the RHS parameter.
        signature.call_type = SignatureType::Method;
        signature.arity = 1;

        // Parse the parameter name.
        declare_named_variable(ctx)?;
        consume_expecting(ctx, Token::RightParen, "Expect ')' after parameter name.")?;
    }
    Ok(())
}

// Compiles a method call with [signature] using [instruction].
fn call_signature(ctx: &mut ParseContext, signature: Signature) {
    let symbol = signature_symbol(ctx, &signature);
    emit(ctx, Ops::Call(signature, symbol));
    // FIXME: Handle superclass calls.
}

// Compiles a method call with [numArgs] for a method with [name] with [length].
fn call_method(ctx: &mut ParseContext, arity: u8, full_name: &str) {
    let symbol = ctx.vm.methods.ensure_symbol(full_name);
    let signature = Signature {
        call_type: SignatureType::Method,
        bare_name: full_name.into(), // FIXME: Wrong.
        arity: arity,
    };
    emit(ctx, Ops::Call(signature, symbol));
}

// A list literal.
fn list(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    // Instantiate a new list.
    load_core_variable(ctx, "List");
    call_method(ctx, 0, "new()");
    // Compile the list elements. Each one compiles to a ".add()" call.
    loop {
        ignore_newlines(ctx)?;

        // Stop if we hit the end of the list.
        if peek_expecting(ctx, Token::RightSquareBracket) {
            break;
        }

        // The element.
        expression(ctx)?;
        call_method(ctx, 1, "addCore_(_)");
        if !match_current(ctx, Token::Comma)? {
            break;
        }
    }

    // Allow newlines before the closing ']'.
    ignore_newlines(ctx)?;
    consume_expecting(
        ctx,
        Token::RightSquareBracket,
        "Expect ']' after list elements.",
    )?;
    Ok(())
}

// A map literal.
fn map(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    // Instantiate a new map.
    load_core_variable(ctx, "Map");
    call_method(ctx, 0, "new()");

    // Compile the map elements. Each one is compiled to just invoke the
    // subscript setter on the map.
    loop {
        ignore_newlines(ctx)?;

        // Stop if we hit the end of the map.
        if peek_expecting(ctx, Token::RightCurlyBrace) {
            break;
        }

        // The key.
        parse_precendence(ctx, Precedence::Unary)?;
        consume_expecting(ctx, Token::Colon, "Expect ':' after map key.")?;
        ignore_newlines(ctx)?;

        // The value.
        expression(ctx)?;
        call_method(ctx, 2, "addCore_(_,_)");

        if !match_current(ctx, Token::Comma)? {
            break;
        }
    }

    // Allow newlines before the closing '}'.
    ignore_newlines(ctx)?;
    consume_expecting(ctx, Token::RightCurlyBrace, "Expect '}' after map entries.")
}

fn unary_op(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    let name = previous_token_name(ctx);

    ignore_newlines(ctx)?;
    // Compile the argument.
    parse_precendence(ctx, Precedence::Unary.one_higher())?;
    // Call the operator method on the left-hand side.
    call_method(ctx, 0, &name);
    Ok(())
}

// Returns a signature with [type] whose name is from the last consumed token.
fn signature_from_token(
    ctx: &ParseContext,
    call_type: SignatureType,
) -> Result<Signature, WrenError> {
    let name = previous_token_name(ctx);

    // wren_c measures in bytes so we do too.
    if name.len() > MAX_METHOD_NAME {
        Err(ctx.error_string(format!(
            "Method names cannot be longer than {} characters.",
            MAX_METHOD_NAME
        )))
    } else {
        Ok(Signature::from_bare_name(call_type, &name, 0))
    }
}

// This isn't needed, just here for ease of code porting.
fn signature_symbol(ctx: &mut ParseContext, signature: &Signature) -> usize {
    // Signature.full_name() is "signatureToString"
    ctx.vm.methods.ensure_symbol(&signature.full_name())
}

fn finish_arguments_list(ctx: &mut ParseContext) -> Result<u8, WrenError> {
    let mut arg_count = 0;
    loop {
        ignore_newlines(ctx)?;
        arg_count += 1;
        validate_num_parameters(ctx, arg_count)?;
        expression(ctx)?;
        let found_comma = match_current(ctx, Token::Comma)?;
        if !found_comma {
            break;
        }
    }
    // Allow a newline before the closing delimiter.
    ignore_newlines(ctx)?;
    Ok(arg_count)
}

// The VM can only handle a certain number of parameters, so check that we
// haven't exceeded that and give a usable error.
fn validate_num_parameters(ctx: &mut ParseContext, num_args: u8) -> Result<(), WrenError> {
    // wren_c checks == here and continues after the error.
    let max: u8 = crate::vm::MAX_PARAMETERS as u8;
    if num_args > max {
        return Err(ctx.error_string(format!("Methods cannot have more than {} parameters.", max)));
    }
    Ok(())
}

// Parses the rest of a comma-separated parameter list after the opening
// delimeter. Updates `arity` in [signature] with the number of parameters.
fn finish_parameter_list(
    ctx: &mut ParseContext,
    signature: &mut Signature,
) -> Result<(), WrenError> {
    loop {
        ignore_newlines(ctx)?;
        signature.arity += 1;
        validate_num_parameters(ctx, signature.arity)?;

        // Define a local variable in the method for the parameter.
        declare_named_variable(ctx)?;
        if !match_current(ctx, Token::Comma)? {
            break;
        }
    }
    Ok(())
}

type Handle<T> = Rc<RefCell<T>>;

// Finishes [compiler], which is compiling a function, method, or chunk of top
// level code. If there is a parent compiler, then this emits code in the
// parent compiler to load the resulting function.
fn end_compiler(
    ctx: &mut ParseContext,
    ending: Box<Compiler>,
    arity: u8,
    name: String,
) -> Result<Handle<ObjFn>, WrenError> {
    let mut compiler = ending;
    // Mark the end of the bytecode. Since it may contain multiple early returns,
    // we can't rely on CODE_RETURN to tell us we're at the end.
    // NOTE: emit() would use wrong compiler, emit manually:
    let line = ctx.parser.previous.line;
    compiler.emit_op_for_line(Ops::End, line);

    compiler.fn_debug.name = name;
    compiler.fn_debug.from_core_module = ctx.in_core_module();

    // We're done with the compiler, create an ObjFn from it.
    let upvalues = std::mem::take(&mut compiler.upvalues);
    let fn_obj = new_handle(ObjFn::new(ctx.vm, ctx.module.clone(), compiler, arity));

    // If this was not the top-compiler, load the compile function into
    // the parent code.
    if ctx.have_compiler() {
        // Wrap the function in a closure. We do this even if it has no upvalues so
        // that the VM can uniformly assume all called objects are closures. This
        // makes creating a function a little slower, but makes invoking them
        // faster. Given that functions are invoked more often than they are
        // created, this is a win.
        let constant = ensure_constant(ctx, Value::Fn(fn_obj.clone()))?;
        emit(ctx, Ops::Closure(constant, upvalues));
    }

    Ok(fn_obj)
}

// Parses a method or function body, after the initial "{" has been consumed.
//
// If [Compiler->isInitializer] is `true`, this is the body of a constructor
// initializer. In that case, this adds the code to ensure it returns `this`.
fn finish_body(ctx: &mut ParseContext) -> Result<(), WrenError> {
    let is_expression_body = finish_block(ctx)?;

    if ctx.compiler().is_initializer {
        // If the initializer body evaluates to a value, discard it.
        if is_expression_body {
            emit(ctx, Ops::Pop);
        }

        // The receiver is always stored in the first local slot.
        emit(
            ctx,
            Ops::Load(Variable {
                scope: Scope::Local,
                index: 0,
            }),
        );
    } else if !is_expression_body {
        // Implicitly return null in statement bodies.
        emit(ctx, Ops::Null);
    }

    emit(ctx, Ops::Return);
    Ok(())
}

struct PushCompiler<'a, 'b> {
    ctx: &'a mut ParseContext<'b>,
    did_pop: bool,
}

impl<'a, 'b> PushCompiler<'a, 'b> {
    fn push_root(ctx: &'a mut ParseContext<'b>) -> PushCompiler<'a, 'b> {
        let current = ctx._compiler.take();
        match current {
            None => (),
            Some(_) => panic!(),
        }
        // Maybe Compiler::root that doesn't take a parent?
        ctx._compiler = Some(Box::new(Compiler::block(current)));
        PushCompiler {
            ctx: ctx,
            did_pop: false,
        }
    }

    fn push_block(ctx: &'a mut ParseContext<'b>) -> PushCompiler<'a, 'b> {
        let current = ctx._compiler.take();
        match current {
            None => panic!(),
            Some(_) => (),
        }
        let compiler = Compiler::block(current);
        ctx._compiler = Some(Box::new(compiler));
        PushCompiler {
            ctx: ctx,
            did_pop: false,
        }
    }

    fn push_method(ctx: &'a mut ParseContext<'b>) -> PushCompiler<'a, 'b> {
        let current = ctx._compiler.take();
        match current {
            None => panic!(),
            Some(_) => (),
        }
        let compiler = Compiler::method(current);
        ctx._compiler = Some(Box::new(compiler));
        PushCompiler {
            ctx: ctx,
            did_pop: false,
        }
    }

    fn pop(&mut self) -> Box<Compiler> {
        assert_eq!(self.did_pop, false);
        let mut compiler = self.ctx._compiler.take().unwrap();
        self.ctx._compiler = compiler.parent.take();
        self.did_pop = true;
        compiler
    }
}

impl<'a, 'b> Drop for PushCompiler<'a, 'b> {
    fn drop(&mut self) {
        if !self.did_pop {
            // Pop this compiler off the stack.
            self.pop();
        }
    }
}

// // Compiles an (optional) argument list for a method call with [methodSignature]
// // and then calls it.
fn method_call(ctx: &mut ParseContext, signature: Signature) -> Result<(), WrenError> {
    let mut called = Signature {
        bare_name: signature.bare_name,
        call_type: SignatureType::Getter,
        arity: 0,
    };
    if match_current(ctx, Token::LeftParen)? {
        ignore_newlines(ctx)?;
        called.call_type = SignatureType::Method;
        if ctx.parser.current.token != Token::RightParen {
            called.arity = finish_arguments_list(ctx)?;
        }
        consume_expecting(ctx, Token::RightParen, "Expect ')' after arguments.")?;
    }

    // Parse the block argument, if any.
    if match_current(ctx, Token::LeftCurlyBrace)? {
        // Include the block argument in the arity.
        called.call_type = SignatureType::Method;
        called.arity += 1;

        let mut scope = PushCompiler::push_block(ctx);

        // Make a dummy signature to track the arity.
        let mut fn_signature = Signature::from_bare_name(SignatureType::Method, "", 0);

        // Parse the parameter list, if any.
        if match_current(scope.ctx, Token::Pipe)? {
            finish_parameter_list(scope.ctx, &mut fn_signature)?;
            consume_expecting(
                scope.ctx,
                Token::Pipe,
                "Expect '|' after function parameters.",
            )?;
        }

        finish_body(scope.ctx)?;

        // Name the function based on the method its passed to.
        let name = called.full_name() + " block argument";
        let compiler = scope.pop();
        end_compiler(scope.ctx, compiler, fn_signature.arity, name)?;
    }

    // Handle SIG_INITIALIZER?
    // if signature.call_type == SignatureType::Initializer {
    //     if called.call_type != SignatureType::Method {
    //         return Err(ctx.grammar_error("A superclass constructor must have an argument list."));
    //     }
    //     called.call_type = SignatureType::Initializer;
    // }

    call_signature(ctx, called);

    Ok(())
}

fn call(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError> {
    ignore_newlines(ctx)?;
    consume_expecting_name(ctx, "Expect method name after '.'.")?;
    named_call(ctx, can_assign)?;
    Ok(())
}

fn or(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    ignore_newlines(ctx)?;

    // Skip the right argument if the left is true.
    let jump = emit(ctx, Ops::OrPlaceholder);
    parse_precendence(ctx, Precedence::LogicalOr)?;
    ctx.compiler_mut().patch_jump(jump);
    Ok(())
}

fn and(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    ignore_newlines(ctx)?;

    // Skip the right argument if the left is true.
    let jump = emit(ctx, Ops::AndPlaceholder);
    parse_precendence(ctx, Precedence::LogicalAnd)?;
    ctx.compiler_mut().patch_jump(jump);
    Ok(())
}

fn this(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    if !ctx.inside_class_definition() {
        // wren_c uses a more direct error call which skips the
        // label telling us that the error occured at "this".
        Err(ctx.error_str("Cannot use 'this' outside of a method."))
    } else {
        load_this(ctx)
    }
}

// Subscript or "array indexing" operator like `foo[bar]`.
fn subscript(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError> {
    let mut arity = finish_arguments_list(ctx)?;
    // Isn't arity always 1 here?
    consume_expecting(
        ctx,
        Token::RightSquareBracket,
        "Expect ']' after arguments.",
    )?;
    allow_line_before_dot(ctx)?;

    let mut call_type = SignatureType::Subscript;
    if can_assign && match_current(ctx, Token::Equals)? {
        call_type = SignatureType::SubscriptSetter;
        // Add one for the implicit RHS arg.
        arity += 1;
        // Compile the assigned value.
        validate_num_parameters(ctx, arity)?;
        expression(ctx)?;
    }
    // Name is always empty for Subscripts, I think?
    let signature = Signature::from_bare_name(call_type, "", arity);
    call_signature(ctx, signature);
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Local,
    Upvalue,
    Module,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub scope: Scope,
    pub index: usize,
}

impl Variable {
    // symbols are passed around as to match wren_c limits
    // but stored (for now) as usize for easy use on the VM size
    // for vector lookups.  Not sure if this is right.
    fn module(symbol: u16) -> Variable {
        Variable {
            scope: Scope::Module,
            index: symbol as usize,
        }
    }

    fn upvalue(symbol: u16) -> Variable {
        Variable {
            scope: Scope::Upvalue,
            index: symbol as usize,
        }
    }

    fn local(symbol: u16) -> Variable {
        Variable {
            scope: Scope::Local,
            index: symbol as usize,
        }
    }
}

fn allow_line_before_dot(ctx: &mut ParseContext) -> Result<(), WrenError> {
    if ctx.parser.current.token == Token::Newline && ctx.parser.next.token == Token::Dot {
        consume(ctx)?;
    }
    Ok(())
}

fn field(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError> {
    let name = previous_token_name(ctx);

    let field_lookup = |ctx: &ParseContext, maybe_class_info: &Option<RefCell<ClassInfo>>| {
        match maybe_class_info {
            None => Err(ctx.error_str("Cannot reference a field outside of a class definition.")),
            Some(class_info) => {
                if class_info.borrow().is_foreign_class {
                    Err(ctx.error_str("Cannot define fields in a foreign class."))
                } else if class_info.borrow().in_static_method {
                    Err(ctx.error_str("Cannot use an instance field in a static method."))
                } else {
                    Ok(class_info.borrow_mut().fields.ensure_symbol(&name))
                }
            }
        }
    };

    let symbol = ctx.call_with_enclosing_class(field_lookup)?;
    // FIXME: Handle field limits.

    // If there's an "=" after a field name, it's an assignment.
    let is_assignment = can_assign && match_current(ctx, Token::Equals)?;
    if is_assignment {
        // Compile the right-hand side.
        expression(ctx)?;
    }

    // FIXME: Could invent a LOAD_FIELD_THIS as an optimization.
    load_this(ctx)?;
    if is_assignment {
        emit(ctx, Ops::StoreField(symbol));
    } else {
        emit(ctx, Ops::LoadField(symbol));
    }
    allow_line_before_dot(ctx)?;
    Ok(())
}

// fn static_field(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError> {
//     // Look up the name in the scope chain.
//     let name = ctx
//         .parser
//         .previous
//         .name(&ctx.parser.input)
//         .map_err(|e| ctx.parse_error(e.into()))?;

//     let class_compiler = getEnclosingClassCompiler(compiler);
//     if class_compiler.is_none() {
//         return Err(ctx.grammar_error("Cannot use a static field outside of a class definition."));
//     }

//     // If this is the first time we've seen this static field, implicitly
//     // define it as a variable in the scope surrounding the class definition.
//     if resolve_local(class_compiler, &name).is_none() {
//         let symbol = declare_variable(class_compiler, name)?;

//         // Implicitly initialize it to null.
//         emitOp(class_compiler, Ops::Null);
//         define_variable(class_compiler, symbol);
//     }

//     // It definitely exists now, so resolve it properly. This is different from
//     // the above resolveLocal() call because we may have already closed over it
//     // as an upvalue.
//     let variable = resolve_name(ctx, &name).unwrap();
//     bare_name(ctx, can_assign, variable);
//     Ok(())
// }

// Compiles a read or assignment to [variable].
fn bare_name(
    ctx: &mut ParseContext,
    can_assign: bool,
    variable: Variable,
) -> Result<(), WrenError> {
    // If there's an "=" after a bare name, it's a variable assignment.
    if can_assign && match_current(ctx, Token::Equals)? {
        // Compile the right-hand side.
        expression(ctx)?;
        emit(ctx, Ops::Store(variable));
    } else {
        emit(ctx, Ops::Load(variable));

        allow_line_before_dot(ctx)?;
    }
    Ok(())
}

// Attempts to look up [name] in the functions enclosing the one being compiled
// by [compiler]. If found, it adds an upvalue for it to this compiler's list
// of upvalues (unless it's already in there) and returns its index. If not
// found, returns -1.
//
// If the name is found outside of the immediately enclosing function, this
// will flatten the closure and add upvalues to all of the intermediate
// functions so that it gets walked down to this one.
//
// If it reaches a method boundary, this stops and returns -1 since methods do
// not close over local variables.
fn find_upvalue(_ctx: &ParseContext, _name: &str) -> Option<u16> {
    // We can't support this until we support having a stack of compilers.
    None
}

fn resolve_non_module(ctx: &ParseContext, name: &str) -> Option<Variable> {
    if let Some(index) = ctx.compiler().locals.iter().position(|l| l.name.eq(name)) {
        return Some(Variable::local(index as u16));
    }
    if let Some(index) = find_upvalue(ctx, name) {
        return Some(Variable::upvalue(index));
    }

    None
}

// wren_c just checks if the first byte is lowercase.
// this exists to be shared with vm.rs.
pub(crate) fn wren_is_local_name(name: &str) -> bool {
    matches!(name.bytes().nth(0).unwrap(), b'a'..=b'z')
}

// Loads the receiver of the currently enclosing method. Correctly handles
// functions defined inside methods.
fn load_this(ctx: &mut ParseContext) -> Result<(), WrenError> {
    let var = resolve_non_module(ctx, "this");
    match var {
        None => Err(ctx.error_str("Could not resolve 'this'"))?,
        Some(var) => {
            emit(ctx, Ops::Load(var));
            Ok(())
        }
    }
}

// Compiles a call whose name is the previously consumed token. This includes
// getters, method calls with arguments, and setter calls.
fn named_call(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError> {
    // Get the token for the method name.
    let mut signature = signature_from_token(ctx, SignatureType::Getter)?;

    if can_assign && match_current(ctx, Token::Equals)? {
        ignore_newlines(ctx)?;

        // Build the setter signature.
        signature.call_type = SignatureType::Setter;
        signature.arity = 1;

        // Compile the assigned value.
        expression(ctx)?;
        call_signature(ctx, signature);
    } else {
        method_call(ctx, signature)?;
        allow_line_before_dot(ctx)?;
    }
    Ok(())
}

fn name(ctx: &mut ParseContext, can_assign: bool) -> Result<(), WrenError> {
    // This needs to be much more complicated to handle module
    // lookups as well as setters.

    let name = previous_token_name(ctx);
    if let Some(variable) = resolve_non_module(ctx, &name) {
        bare_name(ctx, can_assign, variable)?;
        return Ok(());
    }

    // If we're inside a method and the name is lowercase, treat it as a method
    // on this.
    if wren_is_local_name(&name) && ctx.inside_class_definition() {
        load_this(ctx)?;
        return named_call(ctx, can_assign);
    }

    // Otherwise if in module scope handle module case:

    let maybe_index = ctx.module.borrow().lookup_symbol(&name);
    let symbol = match maybe_index {
        None => {
            // Implicitly define a module-level variable in
            // the hopes that we get a real definition later.
            let line_value = Value::from_usize(ctx.parser.previous.line);
            match ctx.module.borrow_mut().define_variable(&name, line_value) {
                Ok(index) => index,
                Err(ModuleLimitError::TooManyVariables) => {
                    return Err(ctx.error_str("Too many module variables defined."));
                }
            }
        }
        Some(index) => index,
    };

    bare_name(ctx, can_assign, Variable::module(symbol))
}

fn expression(ctx: &mut ParseContext) -> Result<(), WrenError> {
    parse_precendence(ctx, Precedence::Lowest)
}

// Bookkeeping information for the current loop being compiled.
#[derive(Copy, Clone)]
struct LoopOffsets {
    // Index of the instruction that the loop should jump back to.
    start: usize,
    // Index of the argument for the CODE_JUMP_IF instruction used to exit the
    // loop. Stored so we can patch it once we know where the loop ends.
    exit_jump: usize,
    // Index of the first instruction of the body of the loop.
    body: usize,
    // Depth of the scope(s) that need to be exited if a break is hit inside the
    // loop.
    scope_depth: ScopeDepth,
}

impl Compiler {
    fn start_loop(&mut self) -> u8 {
        self.loops.push(LoopOffsets {
            start: self.code.len(), // Unclear why wren_c has code - 1 here?
            scope_depth: self.scope_depth,
            body: 0,
            exit_jump: 0,
        });
        (self.loops.len() - 1) as u8
    }
}

fn loop_body(ctx: &mut ParseContext) -> Result<(), WrenError> {
    ctx.compiler_mut().loops.last_mut().unwrap().body = ctx.compiler().code.len();
    statement(ctx)
}

fn end_loop(ctx: &mut ParseContext) {
    let offsets = ctx
        .compiler_mut()
        .loops
        .pop()
        .expect("end_loop called with no loop!");
    emit_loop(ctx, &offsets);
    // Load up the exitLoop instruction and patch it to the current code offset.
    ctx.compiler_mut().patch_jump(offsets.exit_jump);

    // FIXME: Break this out into a method on compiler?
    // Find any break placeholders and make them real jumps.
    for i in offsets.body..ctx.compiler().code.len() {
        match ctx.compiler_mut().code[i] {
            Ops::JumpIfFalsePlaceholder | Ops::JumpPlaceholder => ctx.compiler_mut().patch_jump(i),
            _ => (),
        }
    }
}

impl<'a, 'b> Compiler {
    fn offset_to_current_pc_from_after(&self, offset: usize) -> u16 {
        // We are commonly passed in the instruction offset
        // we're about to fix.  However our output is used
        // as the offset from *after* that instruction to the
        // end of the current PC.  Hence after = offset + 1
        let after = offset + 1;
        (self.code.len() - after) as u16
    }

    fn patch_jump(&mut self, index: usize) {
        let offset_forward = self.offset_to_current_pc_from_after(index);
        self.code[index] = match self.code[index] {
            Ops::JumpIfFalsePlaceholder => Ops::JumpIfFalse(offset_forward),
            Ops::JumpPlaceholder => Ops::Jump(offset_forward),
            Ops::OrPlaceholder => Ops::Or(offset_forward),
            Ops::AndPlaceholder => Ops::And(offset_forward),
            _ => panic!("Token to patch is not a jump! {:?}", self.code[index]),
        }
    }
}

// Look up [name] in the current scope to see what variable it refers to.
// Returns the variable either in module scope, local scope, or the enclosing
// function's upvalue list.
// fn resolve_name(ctx: &mut ParseContext, name: &str) -> Option<Variable> {
//     let maybe_variable = resolve_non_module(ctx, name);
//     if maybe_variable.is_some() {
//         return maybe_variable;
//     }

//     if let Some(index) = ctx.vm.module.lookup_symbol(name) {
//         return Some(Variable {
//             scope: Scope::Module,
//             index: index as usize,
//         });
//     }
//     None
// }

fn load_local(ctx: &mut ParseContext, index: u16) {
    emit(ctx, Ops::Load(Variable::local(index)));
}

fn test_exit_loop(ctx: &mut ParseContext) {
    ctx.compiler_mut().loops.last_mut().unwrap().exit_jump = emit(ctx, Ops::JumpIfFalsePlaceholder);
}

fn for_hidden_variable_scope(ctx: &mut ParseContext) -> Result<(), WrenError> {
    consume_expecting(ctx, Token::LeftParen, "Expect '(' after 'for'.")?;
    consume_expecting_name(ctx, "Expect for loop variable name.")?;

    // Remember the name of the loop variable.
    let name = previous_token_name(ctx);

    consume_expecting(ctx, Token::In, "Expect 'in' after loop variable.")?;
    ignore_newlines(ctx)?;

    // Evaluate the sequence expression and store it in a hidden local variable.
    // The space in the variable name ensures it won't collide with a user-defined
    // variable.
    expression(ctx)?;

    // Verify that there is space to hidden local variables.
    // Note that we expect only two addLocal calls next to each other in the
    // following code.
    if ctx.compiler().locals.len() + 2 > MAX_LOCALS {
        return Err(ctx.error_string(format!("Cannot declare more than {} variables in one scope. (Not enough space for for-loops internal variables)",
          MAX_LOCALS)));
    }

    let seq_slot = add_local(ctx, "seq ".into());

    // Create another hidden local for the iterator object.
    null(ctx, false)?;
    let iter_slot = add_local(ctx, "iter ".into());

    consume_expecting(ctx, Token::RightParen, "Expect ')' after loop expression.")?;

    ctx.compiler_mut().start_loop();

    // Advance the iterator by calling the ".iterate" method on the sequence.
    load_local(ctx, seq_slot);
    load_local(ctx, iter_slot);

    // Update and test the iterator.
    call_method(ctx, 1, "iterate(_)");
    emit(ctx, Ops::Store(Variable::local(iter_slot)));
    test_exit_loop(ctx);

    // Get the current value in the sequence by calling ".iteratorValue".
    load_local(ctx, seq_slot);
    load_local(ctx, iter_slot);
    call_method(ctx, 1, "iteratorValue(_)");

    // Bind the loop variable in its own scope, ensures we get a fresh variable
    // each iteration so that closures for it don't all see the same one.
    {
        let scope = ScopePusher::push_block(ctx);
        add_local(scope.ctx, name);
        loop_body(scope.ctx)?;
    }
    end_loop(ctx);
    Ok(())
}

fn for_statement(ctx: &mut ParseContext) -> Result<(), WrenError> {
    // A for statement like:
    //
    //     for (i in sequence.expression) {
    //       System.print(i)
    //     }
    //
    // Is compiled to bytecode almost as if the source looked like this:
    //
    //     {
    //       var seq_ = sequence.expression
    //       var iter_
    //       while (iter_ = seq_.iterate(iter_)) {
    //         var i = seq_.iteratorValue(iter_)
    //         System.print(i)
    //       }
    //     }
    //
    // It's not exactly this, because the synthetic variables `seq_` and `iter_`
    // actually get names that aren't valid Wren identfiers, but that's the basic
    // idea.
    //
    // The important parts are:
    // - The sequence expression is only evaluated once.
    // - The .iterate() method is used to advance the iterator and determine if
    //   it should exit the loop.
    // - The .iteratorValue() method is used to get the value at the current
    //   iterator position.

    // Create a scope for the hidden local variables used for the iterator.
    let scope = ScopePusher::push_block(ctx);
    for_hidden_variable_scope(scope.ctx)
}

fn if_statement(ctx: &mut ParseContext) -> Result<(), WrenError> {
    // Compile the condition.
    consume_expecting(ctx, Token::LeftParen, "Expect '(' after 'if'.")?;
    expression(ctx)?;
    consume_expecting(ctx, Token::RightParen, "Expect ')' after if condition.")?;
    // Jump to the else branch if the condition is false.
    let if_jump = emit(ctx, Ops::JumpIfFalsePlaceholder);
    // Compile the then branch.
    statement(ctx)?;
    // Compile the else branch if there is one.
    if match_current(ctx, Token::Else)? {
        // Jump over the else branch when the if branch is taken.
        let else_jump = emit(ctx, Ops::JumpPlaceholder);
        ctx.compiler_mut().patch_jump(if_jump);
        statement(ctx)?;
        // Patch the jump over the else.
        ctx.compiler_mut().patch_jump(else_jump);
    } else {
        ctx.compiler_mut().patch_jump(if_jump);
    }
    Ok(())
}

fn while_statement(ctx: &mut ParseContext) -> Result<(), WrenError> {
    ctx.compiler_mut().start_loop();

    // Compile the condition.
    consume_expecting(ctx, Token::LeftParen, "Expect '(' after 'while'.")?;
    expression(ctx)?;
    consume_expecting(ctx, Token::RightParen, "Expect ')' after while condition.")?;

    test_exit_loop(ctx);
    loop_body(ctx)?;
    end_loop(ctx);
    Ok(())
}

// Consumes the current token. Emits an error if it is not a newline. Then
// discards any duplicate newlines following it.
fn consume_at_least_one_line(ctx: &mut ParseContext, msg: &str) -> Result<(), WrenError> {
    consume_expecting(ctx, Token::Newline, msg)?;
    ignore_newlines(ctx)
}

// Parses a block body, after the initial "{" has been consumed.
//
// Returns true if it was a expression body, false if it was a statement body.
// (More precisely, returns true if a value was left on the stack. An empty
// block returns false.)
fn finish_block(ctx: &mut ParseContext) -> Result<bool, WrenError> {
    // Empty blocks do nothing. (Is this required or an optimization?)
    if match_current(ctx, Token::RightCurlyBrace)? {
        return Ok(false);
    }

    // This is a bit magical of Wren...
    // If there's no line after the "{", it's a single-expression body.
    if !match_at_least_one_line(ctx)? {
        expression(ctx)?;
        consume_expecting(ctx, Token::RightCurlyBrace, "Expect '}' at end of block.")?;
        return Ok(true);
    }

    // Empty blocks (with just a newline inside) do nothing.
    if match_current(ctx, Token::RightCurlyBrace)? {
        return Ok(false);
    }

    // Compile the definition list.
    loop {
        definition(ctx)?;
        consume_at_least_one_line(ctx, "Expect newline after statement.")?;
        if ctx.parser.current.token == Token::RightCurlyBrace
            || ctx.parser.current.token == Token::EndOfFile
        {
            break;
        }
    }

    consume_expecting(ctx, Token::RightCurlyBrace, "Expect '}' at end of block.")?;
    return Ok(false);
}

struct ScopePusher<'a, 'b> {
    ctx: &'a mut ParseContext<'b>,
    did_pop: bool,
    did_set_class: bool,
}

impl<'a, 'b> ScopePusher<'a, 'b> {
    fn push_class(ctx: &'a mut ParseContext<'b>, class_info: ClassInfo) -> ScopePusher<'a, 'b> {
        push_scope(ctx);
        ctx.compiler_mut().enclosing_class = Some(RefCell::new(class_info));
        ScopePusher {
            ctx: ctx,
            did_pop: false,
            did_set_class: true,
        }
    }

    fn push_block(ctx: &'a mut ParseContext<'b>) -> ScopePusher<'a, 'b> {
        push_scope(ctx);
        ScopePusher {
            ctx: ctx,
            did_pop: false,
            did_set_class: false,
        }
    }

    fn pop(&mut self) {
        assert_eq!(self.did_pop, false);
        if self.did_set_class {
            self.ctx.compiler_mut().enclosing_class = None;
        }
        pop_scope(self.ctx);
        self.did_pop = true;
    }
}

impl<'a, 'b> Drop for ScopePusher<'a, 'b> {
    fn drop(&mut self) {
        if !self.did_pop {
            // Pop the scope
            self.pop();
        }
    }
}

// Generates code to pop local variables at [depth] or greater. Does *not*
// actually undeclare variables or pop any scopes, though. This is called
// directly when compiling "break" statements to ditch the local variables
// before jumping out of the loop even though they are still in scope *past*
// the break instruction.
//
// Returns the number of local variables for which pops were issued.
// wren_c names this "discardLocals"
fn emit_pops_for_locals(ctx: &mut ParseContext, scope_depth: ScopeDepth) -> usize {
    let target_depth = match scope_depth {
        ScopeDepth::Module => panic!("Can't discard locals at module level."),
        ScopeDepth::Local(i) => i,
    };

    // Note this *does not change* Compiler.locals, only issues pops.
    // Locals remain in compiler until pop_scope().
    fn emit_pops(ctx: &mut ParseContext, target_depth: usize) -> usize {
        let mut pops_emitted = 0;
        for index in (0..ctx.compiler().locals.len()).rev() {
            // Scope the borrow of ctx for local.
            {
                let local = &ctx.compiler().locals[index];
                let local_depth = match local.depth {
                    ScopeDepth::Module => return pops_emitted, // Stop at the magical "this" (see hack note above in Compiler constructor).
                    ScopeDepth::Local(i) => i,
                };
                if local_depth < target_depth {
                    break;
                }
            }
            // FIXME: Handle upvalues.
            emit(ctx, Ops::Pop);
            pops_emitted += 1;
        }
        pops_emitted
    }

    emit_pops(ctx, target_depth)
}

// Attempts to look up the name in the local variables of [compiler]. If found,
// returns its index, otherwise returns -1.
// fn resolve_local(ctx: &mut ParseContext, name: &str) -> Option<usize> {
//     // Look it up in the local scopes. Look in reverse order so that the most
//     // nested variable is found first and shadows outer ones.
//     for (i, local) in ctx.compiler().locals.iter().enumerate().rev() {
//         if local.name.eq(name) {
//             return Some(i);
//         }
//     }
//     None
// }

// Break, continue, if, for, while, blocks, etc.
// Unlike expression, does not leave something on the stack.
fn statement(ctx: &mut ParseContext) -> Result<(), WrenError> {
    if match_current(ctx, Token::Break)? {
        let offsets = *ctx
            .compiler()
            .loops
            .last()
            .ok_or_else(|| ctx.error_str("Cannot use 'break' outside of a loop."))?;

        // Since we will be jumping out of the scope, make sure any locals in it
        // are discarded first.
        emit_pops_for_locals(ctx, offsets.scope_depth.one_deeper());

        // Emit a placeholder instruction for the jump to the end of the body.
        // We'll fix these up with real Jumps at the end of compiling this loop.
        emit(ctx, Ops::JumpPlaceholder); // Break placeholder
    } else if match_current(ctx, Token::Continue)? {
        let offsets = *ctx
            .compiler()
            .loops
            .last()
            .ok_or_else(|| ctx.error_str("Cannot use 'continue' outside of a loop."))?;

        // Since we will be jumping out of the scope, make sure any locals in it
        // are discarded first.
        emit_pops_for_locals(ctx, offsets.scope_depth.one_deeper());

        emit_loop(ctx, &offsets);
    } else if match_current(ctx, Token::For)? {
        for_statement(ctx)?;
    } else if match_current(ctx, Token::If)? {
        if_statement(ctx)?;
    } else if match_current(ctx, Token::Return)? {
        // Compile the return value.
        if peek_expecting(ctx, Token::Newline) {
            // If there's no expression after return, initializers should
            // return 'this' and regular methods should return null
            if ctx.compiler().is_initializer {
                emit(ctx, Ops::Load(Variable::local(0)));
            } else {
                emit(ctx, Ops::Null);
            }
        } else {
            if ctx.compiler().is_initializer {
                return Err(ctx.error_str("A constructor cannot return a value."));
            }
            expression(ctx)?;
        }
        emit(ctx, Ops::Return);
    } else if match_current(ctx, Token::While)? {
        while_statement(ctx)?;
    } else if match_current(ctx, Token::LeftCurlyBrace)? {
        // Block statement.
        let scope = ScopePusher::push_block(ctx);
        if finish_block(scope.ctx)? {
            // Block was an expression, so discard it.
            emit(scope.ctx, Ops::Pop);
        }
    } else {
        expression(ctx)?;
        emit(ctx, Ops::Pop);
    }
    Ok(())
}

// Creates a matching constructor method for an initializer with [signature]
// and [initializerSymbol].
//
// Construction is a two-stage process in Wren that involves two separate
// methods. There is a static method that allocates a new instance of the class.
// It then invokes an initializer method on the new instance, forwarding all of
// the constructor arguments to it.
//
// The allocator method always has a fixed implementation:
//
//     CODE_CONSTRUCT - Replace the class in slot 0 with a new instance of it.
//     CODE_CALL      - Invoke the initializer on the new instance.
//
// This creates that method and calls the initializer with [initializerSymbol].
fn create_constructor(
    ctx: &mut ParseContext,
    signature: Signature,
    initializer_symbol: usize, // Isn't this the same as signature?
) -> Result<(), WrenError> {
    let is_foreign_class = ctx
        .compiler()
        .enclosing_class
        .as_ref()
        .unwrap()
        .borrow()
        .is_foreign_class;

    let mut scope = PushCompiler::push_method(ctx);
    // Allocate the instance.
    if is_foreign_class {
        emit(scope.ctx, Ops::ForeignConstruct);
    } else {
        emit(scope.ctx, Ops::Construct);
    }

    // Run its initializer.
    emit(scope.ctx, Ops::Call(signature, initializer_symbol));

    // Return the instance.
    emit(scope.ctx, Ops::Return);

    let compiler = scope.pop();
    // FIXME: Where does this closure get put?
    end_compiler(scope.ctx, compiler, 0, "".into())?;
    Ok(())
}

// Loads the enclosing class onto the stack and then binds the function already
// on the stack as a method on that class.
fn define_method(
    ctx: &mut ParseContext,
    class_variable: Variable,
    is_static: bool,
    method_symbol: usize,
) {
    // Load the class. We have to do this for each method because we can't
    // keep the class on top of the stack. If there are static fields, they
    // will be locals above the initial variable slot for the class on the
    // stack. To skip past those, we just load the class each time right before
    // defining a method.
    emit(ctx, Ops::Load(class_variable));

    // Define the method.
    emit(ctx, Ops::Method(is_static, method_symbol));
}

// Declares a method in the enclosing class with [signature].
//
// Reports an error if a method with that signature is already declared.
// Returns the symbol for the method.
fn declare_method(ctx: &mut ParseContext, signature: &Signature) -> Result<usize, WrenError> {
    let symbol = signature_symbol(ctx, signature);
    // FIXME: Ensure this method isn't a duplicate.
    Ok(symbol)
}

// Create a new local variable with [name]. Assumes the current scope is local
// and the name is unique.
fn add_local(ctx: &mut ParseContext, name: String) -> u16 {
    let local = Local {
        name: name,
        depth: ScopeDepth::Local(ctx.compiler().nested_local_scope_count()),
    };
    ctx.compiler_mut().locals.push(local);
    (ctx.compiler().locals.len() - 1) as u16
}

fn declare_variable(ctx: &mut ParseContext, name: String) -> Result<u16, WrenError> {
    // Enable in a separate change.
    if name.len() > MAX_VARIABLE_NAME {
        return Err(ctx.error_string(format!(
            "Variable name cannot be longer than {} characters.",
            MAX_VARIABLE_NAME
        )));
    }

    // Top-level module scope.
    if ctx.compiler().scope_depth == ScopeDepth::Module {
        // Error handling missing.
        // Error handling should occur inside wren_define_variable, no?
        let result = wren_define_variable(&mut ctx.module.borrow_mut(), &name, Value::Null);
        let symbol = result.map_err(|e| {
            let msg = match e {
                DefinitionError::LocalUsedBeforeDefinition(line) => format!(
                    "Variable '{}' referenced before this definition (first use at line {}).",
                    name, line
                ),
                DefinitionError::TooManyVariables => "Too many module variables defined.".into(),
                DefinitionError::VariableAlreadyDefined => {
                    "Module variable is already defined.".into()
                }
            };
            ctx.error_string(msg)
        })?;

        return Ok(symbol);
    }

    // See if there is already a variable with this name declared in the current
    // scope. (Outer scopes are OK: those get shadowed.)
    for local in ctx.compiler().locals.iter().rev() {
        // Once we escape this scope and hit an outer one, we can stop.
        if local.depth < ctx.compiler().scope_depth {
            break;
        }
        if local.name.eq(&name) {
            // Wren still returns, even after emitting the error!
            return Err(ctx.error_str("Variable is already declared in this scope."));
        }
    }

    if ctx.compiler().locals.len() >= MAX_LOCALS {
        return Err(ctx.error_string(format!(
            "Cannot declare more than {} variables in one scope.",
            MAX_LOCALS,
        )));
    }

    Ok(add_local(ctx, name))
}

// Parses a name token and declares a variable in the current scope with that
// name. Returns its slot.
fn declare_named_variable(ctx: &mut ParseContext) -> Result<u16, WrenError> {
    consume_expecting_name(ctx, "Expect variable name.")?;
    let name = previous_token_name(ctx);
    declare_variable(ctx, name)
}

// Compiles an "import" statement.
//
// An import compiles to a series of instructions. Given:
//
//     import "foo" for Bar, Baz
//
// We compile a single IMPORT_MODULE "foo" instruction to load the module
// itself. When that finishes executing the imported module, it leaves the
// ObjModule in vm->lastModule. Then, for Bar and Baz, we:
//
// * Declare a variable in the current scope with that name.
// * Emit an IMPORT_VARIABLE instruction to load the variable's value from the
//   other module.
// * Compile the code to store that value in the variable in this scope.
fn import(ctx: &mut ParseContext) -> Result<(), WrenError> {
    ignore_newlines(ctx)?;
    let import_name = consume_string(ctx, "Expect a string after 'import'.")?;

    // Load the module.
    emit(ctx, Ops::ImportModule(import_name));

    // Discard the unused result value from calling the module body's closure.
    emit(ctx, Ops::Pop);

    // The for clause is optional.
    if !match_current(ctx, Token::For)? {
        return Ok(());
    }

    // Compile the comma-separated list of variables to import.
    loop {
        ignore_newlines(ctx)?;

        consume_expecting_name(ctx, "Expect variable name.")?;

        // We need to hold onto the source variable,
        // in order to reference it in the import later
        let variable_name = previous_token_name(ctx);

        // Store the symbol we care about for the variable
        let slot = if match_current(ctx, Token::As)? {
            //import "module" for Source as Dest
            //Use 'Dest' as the name by declaring a new variable for it.
            //This parses a name after the 'as' and defines it.
            declare_named_variable(ctx)
        } else {
            //import "module" for Source
            //Uses 'Source' as the name directly
            declare_variable(ctx, variable_name.clone())
        }?;

        // Load the variable from the other module.
        emit(ctx, Ops::ImportVariable(variable_name));

        // Store the result in the variable here.
        define_variable(ctx, slot);
        if !match_current(ctx, Token::Comma)? {
            break;
        }
    }
    Ok(())
}

fn previous_token_name(ctx: &ParseContext) -> String {
    ctx.parser.previous.name(&ctx.parser.input)
}

fn variable_definition(ctx: &mut ParseContext) -> Result<(), WrenError> {
    // Grab its name, but don't declare it yet. A (local) variable shouldn't be
    // in scope in its own initializer.
    consume_expecting_name(ctx, "Expect variable name.")?;
    let name = previous_token_name(ctx);

    // Compile the initializer.
    if match_current(ctx, Token::Equals)? {
        ignore_newlines(ctx)?;
        expression(ctx)?;
    } else {
        // Default initialize it to null.
        emit(ctx, Ops::Null);
    }

    // Now put it in scope.
    let symbol = declare_variable(ctx, name)?;
    define_variable(ctx, symbol);
    Ok(())
}

// FIXME: This is probably not needed?  All it really adds is an assert?
fn load_core_variable(ctx: &mut ParseContext, name: &str) {
    let symbol = match ctx.module.borrow().lookup_symbol(name) {
        Some(s) => s,
        None => panic!("Failed to find core variable {}", name),
    };
    emit(ctx, Ops::Load(Variable::module(symbol)));
}

fn define_variable(ctx: &mut ParseContext, symbol: u16) {
    match ctx.compiler().scope_depth {
        ScopeDepth::Local(_) => {
            // Store the variable. If it's a local, the result of the initializer is
            // in the correct slot on the stack already so we're done.
            return;
        }
        ScopeDepth::Module => {
            // It's a module-level variable, so store the value in the module slot and
            // then discard the temporary for the initializer.
            emit(ctx, Ops::Store(Variable::module(symbol)));
            emit(ctx, Ops::Pop);
        }
    }
}

fn scope_for_definitions(ctx: &ParseContext) -> Scope {
    match ctx.compiler().scope_depth {
        ScopeDepth::Local(_) => Scope::Local,
        ScopeDepth::Module => Scope::Module,
    }
}

// Compiles an optional setter parameter in a method [signature].
//
// Returns `true` if it was a setter.
fn maybe_setter(ctx: &mut ParseContext, signature: &mut Signature) -> Result<bool, WrenError> {
    // See if it's a setter.
    if !match_current(ctx, Token::Equals)? {
        return Ok(false);
    }

    // It's a setter.
    signature.call_type = if signature.call_type == SignatureType::Subscript {
        SignatureType::SubscriptSetter
    } else {
        SignatureType::Setter
    };

    // Parse the value parameter.
    consume_expecting(ctx, Token::LeftParen, "Expect '(' after '='.")?;
    declare_named_variable(ctx)?;
    consume_expecting(ctx, Token::RightParen, "Expect ')' after parameter name.")?;

    signature.arity += 1;

    Ok(true)
}

// Compiles a method signature for a subscript operator.
fn subscript_signature(ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError> {
    signature.call_type = SignatureType::Subscript;
    // signature is "[" since that was the matching token, clear it.
    signature.bare_name = "".into();

    // Parse the parameters inside the subscript.
    finish_parameter_list(ctx, signature)?;
    consume_expecting(
        ctx,
        Token::RightSquareBracket,
        "Expect ']' after parameters.",
    )?;

    maybe_setter(ctx, signature)?;
    Ok(())
}

// Parses an optional parenthesized parameter list. Updates `type` and `arity`
// in [signature] to match what was parsed.
fn parameter_list(ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError> {
    // The parameter list is optional.
    if !match_current(ctx, Token::LeftParen)? {
        return Ok(());
    }

    signature.call_type = SignatureType::Method;

    // Allow new line before an empty argument list
    ignore_newlines(ctx)?;

    // Allow an empty parameter list.
    if match_current(ctx, Token::RightParen)? {
        return Ok(());
    }

    finish_parameter_list(ctx, signature)?;
    consume_expecting(ctx, Token::RightParen, "Expect ')' after parameters.")
}
// Compiles a method signature for a named method or setter.
fn named_signature(ctx: &mut ParseContext, signature: &mut Signature) -> Result<(), WrenError> {
    signature.call_type = SignatureType::Getter;

    // If it's a setter, it can't also have a parameter list.
    if maybe_setter(ctx, signature)? {
        return Ok(());
    }

    // Regular named method with an optional parameter list.
    parameter_list(ctx, signature)
}

// Compiles a method signature for a constructor.
fn constructor_signature(
    ctx: &mut ParseContext,
    signature: &mut Signature,
) -> Result<(), WrenError> {
    consume_expecting_name(ctx, "Expect constructor name after 'construct'.")?;
    // HACK(from wren_c) Capture the name.
    signature.bare_name = signature_from_token(ctx, SignatureType::Initializer)?.bare_name;
    signature.call_type = SignatureType::Initializer;

    if match_current(ctx, Token::Equals)? {
        return Err(ctx.error_str("A constructor cannot be a setter."));
    }

    // Isn't this just consume_expecting_msg?
    if !match_current(ctx, Token::LeftParen)? {
        return Err(ctx.error_str("A constructor cannot be a getter."));
    }
    // Allow an empty parameter list.
    if match_current(ctx, Token::RightParen)? {
        return Ok(());
    }
    finish_parameter_list(ctx, signature)?;

    consume_expecting(ctx, Token::RightParen, "Expect ')' after parameters.")
}

// Compiles a method definition inside a class body.
//
// Returns `true` if it compiled successfully, or `false` if the method couldn't
// be parsed.
fn method(ctx: &mut ParseContext, class_variable: &Variable) -> Result<bool, WrenError> {
    // Parse any attributes before the method and store them
    //   if (matchAttribute(compiler))
    //   {
    //     return method(compiler, classVariable);
    //   }

    // wren_c TODO: What about foreign constructors?
    let is_foreign = match_current(ctx, Token::Foreign)?;
    let is_static = match_current(ctx, Token::Static)?;
    ctx.compiler_mut()
        .enclosing_class
        .as_ref()
        .unwrap()
        .borrow_mut()
        .in_static_method = is_static;

    let maybe_signature_fn = ctx.parser.current.token.grammar_rule().signature;
    consume(ctx)?;

    let signature_fn = maybe_signature_fn.ok_or(ctx.error_str("Expect method definition."))?;

    // Build the method signature.
    let mut signature = signature_from_token(ctx, SignatureType::Getter)?;
    //   compiler->enclosingClass->signature = &signature;

    let method_symbol;
    {
        let mut scope = PushCompiler::push_method(ctx);

        // Compile the method signature.
        signature_fn(scope.ctx, &mut signature)?;

        scope.ctx.compiler_mut().is_initializer = signature.call_type == SignatureType::Initializer;

        if is_static && signature.call_type == SignatureType::Initializer {
            scope.ctx.error_str("A constructor cannot be static.");
        }

        // Include the full signature in debug messages in stack traces.
        //   signatureToString(&signature, fullSignature, &length);

        // Copy any attributes the compiler collected into the enclosing class
        //   copyMethodAttributes(compiler, isForeign, isStatic, fullSignature, length);

        // Check for duplicate methods. Doesn't matter that it's already been
        // defined, error will discard bytecode anyway.
        // Check if the method table already contains this symbol
        method_symbol = declare_method(scope.ctx, &signature)?;

        // FIXME: handle foreign.

        if !is_foreign {
            consume_expecting(
                scope.ctx,
                Token::LeftCurlyBrace,
                "Expect '{' to begin method body.",
            )?;
            finish_body(scope.ctx)?;
            let compiler = scope.pop();
            // FIXME: Where does the closure go?
            end_compiler(scope.ctx, compiler, signature.arity, signature.full_name())?;
        }
    }

    // Outside the scope block so we can emit to the outer compiler:
    if is_foreign {
        emit_constant(ctx, Value::from_string(signature.full_name()))?;
    }

    // Define the method. For a constructor, this defines the instance
    // initializer method.
    define_method(ctx, class_variable.clone(), is_static, method_symbol);

    if signature.call_type == SignatureType::Initializer {
        // Also define a matching constructor method on the metaclass.
        signature.call_type = SignatureType::Method;
        let constructor_symbol = signature_symbol(ctx, &signature);

        create_constructor(ctx, signature, method_symbol)?;
        define_method(ctx, class_variable.clone(), true, constructor_symbol);
    }

    Ok(true)
}

// TODO: is_foreign should probably be an enum.
fn class_definition(ctx: &mut ParseContext, is_foreign: bool) -> Result<(), WrenError> {
    // Create a variable to store the class in.

    let class_symbol = declare_named_variable(ctx)?;
    let class_variable = Variable {
        scope: scope_for_definitions(ctx),
        index: class_symbol as usize,
    };
    // FIXME: Should declare_named_variable return the name too?
    let name = previous_token_name(ctx);

    // FIXME: Clone shouldn't be needed.
    let class_name = Value::String(Rc::new(name.clone()));

    // Make a string constant for the name.
    emit_constant(ctx, class_name)?;
    // FIXME: Handle superclasses, TOKEN_IS

    // Load the superclass (if there is one).
    if match_current(ctx, Token::Is)? {
        parse_precendence(ctx, Precedence::Call)?;
    } else {
        // Implicitly inherit from Object.
        load_core_variable(ctx, "Object");
    }

    // Store a placeholder for the number of fields argument. We don't know the
    // count until we've compiled all the methods to see which fields are used.
    let class_instruction = if !is_foreign {
        emit(ctx, Ops::ClassPlaceholder)
    } else {
        emit(ctx, Ops::ForeignClass);
        usize::MAX // Unused
    };

    // Store it in its name. (in the module case)
    define_variable(ctx, class_symbol);

    // Push a local variable scope. Static fields in a class body are hoisted out
    // into local variables declared in this scope. Methods that use them will
    // have upvalues referencing them.
    {
        let mut scope = ScopePusher::push_class(ctx, ClassInfo::new(&name, is_foreign));
        // FIXME: Handle attributes.

        //   // Set up a symbol table for the class's fields. We'll initially compile
        //   // them to slots starting at zero. When the method is bound to the class, the
        //   // bytecode will be adjusted by [wrenBindMethod] to take inherited fields
        //   // into account.
        //   wrenSymbolTableInit(&classInfo.fields);

        //   // Set up symbol buffers to track duplicate static and instance methods.
        //   wrenIntBufferInit(&classInfo.methods);
        //   wrenIntBufferInit(&classInfo.staticMethods);

        // Compile the method definitions.
        consume_expecting(
            scope.ctx,
            Token::LeftCurlyBrace,
            "Expect '{' after class declaration.",
        )?;
        match_at_least_one_line(scope.ctx)?;

        while !match_current(scope.ctx, Token::RightCurlyBrace)? {
            if !method(scope.ctx, &class_variable)? {
                break;
            }

            // Don't require a newline after the last definition.
            if match_current(scope.ctx, Token::RightCurlyBrace)? {
                break;
            }

            consume_at_least_one_line(scope.ctx, "Expect newline after definition in class.")?;
        }

        //   // If any attributes are present,
        //   // instantiate a ClassAttributes instance for the class
        //   // and send it over to CODE_END_CLASS
        //   bool hasAttr = classInfo.classAttributes != NULL ||
        //                  classInfo.methodAttributes != NULL;
        //   if(hasAttr) {
        //     emitClassAttributes(compiler, &classInfo);
        //     loadVariable(compiler, classVariable);
        //     // At the moment, we don't have other uses for CODE_END_CLASS,
        //     // so we put it inside this condition. Later, we can always
        //     // emit it and use it as needed.
        //     emitOp(compiler, CODE_END_CLASS);
        //   }

        // FIXME::Clear symbol tables for tracking field and method names.
        if !is_foreign {
            let num_fields = scope
                .ctx
                .compiler()
                .enclosing_class
                .as_ref()
                .unwrap()
                .borrow()
                .fields
                .count();
            scope.ctx.compiler_mut().code[class_instruction] = Ops::Class(num_fields);
        }
        scope.pop(); // Making explicit what leaving the block is about to do.
    }
    Ok(())
}

// Class definitions, imports, etc.
fn definition(ctx: &mut ParseContext) -> Result<(), WrenError> {
    // We don't handle class definitions, etc. yet.

    if match_current(ctx, Token::Class)? {
        return class_definition(ctx, false);
    } else if match_current(ctx, Token::Foreign)? {
        consume_expecting(ctx, Token::Class, "Expect 'class' after 'foreign'.")?;
        return class_definition(ctx, true);
    }

    // Fall through to the "statement" case.
    if match_current(ctx, Token::Import)? {
        import(ctx)
    } else if match_current(ctx, Token::Var)? {
        variable_definition(ctx)
    } else {
        statement(ctx)
    }
}

fn grouping(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    expression(ctx)?;
    consume_expecting(ctx, Token::RightParen, "Expect ')' after expression.")?;
    Ok(())
}

fn boolean(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    if let Token::Boolean(b) = ctx.parser.previous.token {
        emit(ctx, Ops::Boolean(b));
    } else {
        panic!("boolean called w/o boolean token");
    }
    Ok(())
}

fn null(ctx: &mut ParseContext, _can_assign: bool) -> Result<(), WrenError> {
    emit(ctx, Ops::Null);
    Ok(())
}

// How many states are needed here?
// Could this just be an enum?
// enum Grammar {
//     Prefix(PrefixParslet),
//     Infix(InfixParslet),
//     Unused,
// }
struct GrammarRule {
    prefix: Option<PrefixParslet>,
    infix: Option<InfixParslet>,
    precedence: Precedence,
    signature: Option<MethodSignatureParslet>,
}

impl GrammarRule {
    fn prefix(prefix_parselet: PrefixParslet) -> GrammarRule {
        GrammarRule {
            prefix: Some(prefix_parselet),
            infix: None,
            precedence: Precedence::None,
            signature: None,
        }
    }

    fn infix_operator(precedence: Precedence) -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: Some(infix_op),
            precedence: precedence,
            signature: Some(infix_signature),
        }
    }

    fn prefix_operator() -> GrammarRule {
        GrammarRule {
            prefix: Some(unary_op),
            infix: None,
            precedence: Precedence::None,
            signature: Some(unary_signature),
        }
    }
    fn infix(precedence: Precedence, infix_parselet: InfixParslet) -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: Some(infix_parselet),
            precedence: precedence,
            signature: None,
        }
    }

    fn unused() -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
            signature: None,
        }
    }
}

// Parslets belong in some sort of grouped parslet object per token?
impl Token {
    fn grammar_rule(&self) -> GrammarRule {
        // This should switch on TokenType, not Token itself.
        match self {
            Token::BeforeFile => unimplemented!(),
            Token::LeftParen => GrammarRule::prefix(grouping),
            Token::RightParen => GrammarRule::unused(),
            // Unclear if subscriptSignature is needed?
            Token::LeftCurlyBrace => GrammarRule::prefix(map),
            Token::RightCurlyBrace => GrammarRule::unused(),
            Token::RightSquareBracket => GrammarRule::unused(),
            Token::LeftSquareBracket => GrammarRule {
                prefix: Some(list),
                infix: Some(subscript),
                precedence: Precedence::Call,
                signature: Some(subscript_signature),
            },
            Token::Minus => GrammarRule {
                prefix: Some(unary_op),
                infix: Some(infix_op),
                precedence: Precedence::Term,
                signature: Some(mixed_signature),
            },
            Token::Plus => GrammarRule::infix_operator(Precedence::Term),
            Token::Tilde => GrammarRule::prefix_operator(),
            Token::Colon => GrammarRule::unused(),
            Token::Question => GrammarRule::infix(Precedence::Assignment, conditional),
            Token::OpFactor(_) => GrammarRule::infix_operator(Precedence::Factor),
            Token::Bang => GrammarRule::prefix_operator(),
            Token::BangEquals => GrammarRule::infix_operator(Precedence::Equality),
            Token::Num(_) => GrammarRule::prefix(literal),
            Token::String(_) => GrammarRule::prefix(literal),
            Token::Dot => GrammarRule::infix(Precedence::Call, call),
            Token::DotDot => GrammarRule::infix_operator(Precedence::Range),
            Token::DotDotDot => GrammarRule::infix_operator(Precedence::Range),
            Token::Boolean(_) => GrammarRule::prefix(boolean),
            Token::Null => GrammarRule::prefix(null),
            Token::Hash => GrammarRule::unused(),
            Token::Var => GrammarRule::unused(),
            Token::Is => GrammarRule::infix_operator(Precedence::Is),
            Token::Caret => GrammarRule::infix_operator(Precedence::BitwiseXor),
            Token::Pipe => GrammarRule::infix_operator(Precedence::BitwiseOr),
            Token::PipePipe => GrammarRule::infix(Precedence::LogicalOr, or),
            Token::Amp => GrammarRule::infix_operator(Precedence::BitwiseAnd),
            Token::AmpAmp => GrammarRule::infix(Precedence::LogicalOr, and),
            Token::For => GrammarRule::unused(),
            Token::In => GrammarRule::unused(),
            Token::If => GrammarRule::unused(),
            Token::Else => GrammarRule::unused(),
            Token::Construct => GrammarRule {
                prefix: None,
                infix: None,
                signature: Some(constructor_signature),
                precedence: Precedence::None,
            },
            Token::Foreign => GrammarRule::unused(),
            Token::Static => GrammarRule::unused(),
            Token::Class => GrammarRule::unused(),
            Token::While => GrammarRule::unused(),
            Token::This => GrammarRule::prefix(this),
            Token::Return => GrammarRule::unused(),
            Token::Break => GrammarRule::unused(),
            Token::Continue => GrammarRule::unused(),
            Token::LessThanLessThan => GrammarRule::infix_operator(Precedence::BitwiseShift),
            Token::GreaterThanGreaterThan => GrammarRule::infix_operator(Precedence::BitwiseShift),
            Token::LessThan => GrammarRule::infix_operator(Precedence::Comparison),
            Token::GreaterThan => GrammarRule::infix_operator(Precedence::Comparison),
            Token::LessThanEquals => GrammarRule::infix_operator(Precedence::Comparison),
            Token::GreaterThanEquals => GrammarRule::infix_operator(Precedence::Comparison),
            Token::Comma => GrammarRule::unused(),
            Token::Import => GrammarRule::unused(),
            Token::As => GrammarRule::unused(),
            Token::Newline => GrammarRule::unused(),
            Token::EndOfFile => GrammarRule::unused(),
            Token::Equals => GrammarRule::unused(),
            Token::EqualsEquals => GrammarRule::infix_operator(Precedence::Equality),
            Token::Name(_) => GrammarRule {
                prefix: Some(name),
                infix: None,
                signature: Some(named_signature),
                precedence: Precedence::None,
            },
            Token::Field(_) => GrammarRule::prefix(field),
            // Token::StaticField(_) => GrammarRule::prefix(static_field),
            Token::StaticField(_) => GrammarRule::unused(), // FIXME: Wrong.
            Token::Interpolation(_) => GrammarRule::prefix(string_interpolation),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Lexer(LexError),
    Grammar(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Add Error: to Lexer messages to match
            // lexError from wren_c (needs refactor).
            ParserError::Lexer(msg) => write!(f, "Error: {}", msg),
            ParserError::Grammar(msg) => write!(f, "{}", msg),
        }
    }
}

impl error::Error for ParserError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            ParserError::Lexer(ref e) => Some(e),
            ParserError::Grammar(..) => None,
        }
    }
}

impl From<LexError> for ParserError {
    fn from(err: LexError) -> ParserError {
        ParserError::Lexer(err)
    }
}

// typedef void (*WrenErrorFn)(
//     WrenVM* vm, WrenErrorType type, const char* module, int line,
//     const char* message);

#[derive(Debug)]
pub struct WrenError {
    pub module: String,
    pub line: usize,
    pub error: ParserError, // Probably shouldn't be public?
}

// FIXME: This is a stub.
impl fmt::Display for WrenError {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}

// FIXME: This is a stub.
impl error::Error for WrenError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

// This is called nextToken in wren_c, and consume_expecting is consume.
fn consume(ctx: &mut ParseContext) -> Result<(), WrenError> {
    std::mem::swap(&mut ctx.parser.previous, &mut ctx.parser.current);
    std::mem::swap(&mut ctx.parser.current, &mut ctx.parser.next);
    ctx.parser.next = next_token(&mut ctx.parser.input).map_err(|e| ctx.parse_error(e.into()))?;
    Ok(())
}

fn peek_expecting(ctx: &ParseContext, expected: Token) -> bool {
    ctx.parser.current.token == expected
}

fn match_current(ctx: &mut ParseContext, token: Token) -> Result<bool, WrenError> {
    if ctx.parser.current.token == token {
        consume(ctx)?;
        return Ok(true);
    }
    Ok(false)
}

fn match_current_interpolation(ctx: &mut ParseContext) -> Result<bool, WrenError> {
    match ctx.parser.current.token {
        Token::Interpolation(_) => {
            consume(ctx)?;
            return Ok(true);
        }
        _ => Ok(false),
    }
}

fn match_at_least_one_line(ctx: &mut ParseContext) -> Result<bool, WrenError> {
    let mut saw_line = false;
    while Token::Newline == ctx.parser.current.token {
        consume(ctx)?;
        saw_line = true;
    }
    Ok(saw_line)
}

fn consume_expecting_name(ctx: &mut ParseContext, error_message: &str) -> Result<(), WrenError> {
    consume(ctx)?;
    match ctx.parser.previous.token {
        Token::Name(_) => Ok(()),
        _ => Err(ctx.error_str(error_message)),
    }
}

fn consume_string(ctx: &mut ParseContext, error_message: &str) -> Result<String, WrenError> {
    consume(ctx)?;
    match &ctx.parser.previous.token {
        Token::String(s) => Ok(s.clone()),
        _ => Err(ctx.error_str(error_message)),
    }
}

fn consume_expecting(
    ctx: &mut ParseContext,
    token: Token,
    error_message: &str,
) -> Result<(), WrenError> {
    consume(ctx)?;
    if ctx.parser.previous.token != token {
        return Err(ctx.error_str(error_message));
    }
    Ok(())
}

struct NestingScope<'a, 'b> {
    ctx: &'a mut ParseContext<'b>,
}

impl<'a, 'b> NestingScope<'a, 'b> {
    fn push(ctx: &'a mut ParseContext<'b>) -> NestingScope<'a, 'b> {
        ctx.nesting += 1;
        NestingScope { ctx: ctx }
    }
}

impl<'a, 'b> Drop for NestingScope<'a, 'b> {
    fn drop(&mut self) {
        self.ctx.nesting -= 1;
    }
}

fn parse_precendence(ctx: &mut ParseContext, precedence: Precedence) -> Result<(), WrenError> {
    if ctx.nesting > MAX_GRAMMAR_NESTING {
        return Err(ctx.error_string(format!(
            "Maximum grammar nesting ({}) exceeded.",
            MAX_GRAMMAR_NESTING
        )));
    }
    let scope = NestingScope::push(ctx);
    consume(scope.ctx)?;
    let prefix_parser = scope
        .ctx
        .parser
        .previous
        .token
        .grammar_rule()
        .prefix
        .ok_or_else(|| scope.ctx.error_str("Expected expression."))?;

    // Track if the precendence of the surrounding expression is low enough to
    // allow an assignment inside this one. We can't compile an assignment like
    // a normal expression because it requires us to handle the LHS specially --
    // it needs to be an lvalue, not an rvalue. So, for each of the kinds of
    // expressions that are valid lvalues -- names, subscripts, fields, etc. --
    // we pass in whether or not it appears in a context loose enough to allow
    // "=". If so, it will parse the "=" itself and handle it appropriately.
    let can_assign = precedence <= Precedence::Conditional;
    prefix_parser(scope.ctx, can_assign)?;

    while precedence <= scope.ctx.parser.current.token.grammar_rule().precedence {
        consume(scope.ctx)?;
        let infix_parser = scope
            .ctx
            .parser
            .previous
            .token
            .grammar_rule()
            .infix
            .expect("Invalid token");
        infix_parser(scope.ctx, can_assign)?;
    }
    Ok(())
}

fn ignore_newlines(ctx: &mut ParseContext) -> Result<(), WrenError> {
    match_at_least_one_line(ctx)?;
    Ok(())
}

pub(crate) fn compile_in_module(
    vm: &mut WrenVM,
    module_name: &str,
    input: InputManager,
) -> Result<Handle<ObjClosure>, WrenError> {
    // When compiling, we create a module and register it.
    let module = vm.lookup_or_register_empty_module(module_name);
    wren_compile(vm, input, module)
}

pub(crate) fn wren_compile_source(
    vm: &mut WrenVM,
    module_name: &str,
    source: String,
) -> Result<Handle<ObjClosure>, WrenError> {
    let input = InputManager::from_string(source);
    compile_in_module(vm, module_name, input)
}

pub(crate) fn wren_compile<'a>(
    vm: &'a mut WrenVM,
    input: InputManager,
    module: Handle<Module>,
) -> Result<Handle<ObjClosure>, WrenError> {
    let num_existing_variables = module.borrow().variable_count();

    // Init the parser & compiler
    let mut parse_context = ParseContext {
        parser: Parser {
            input: input,
            previous: ParseToken::before_file(),
            current: ParseToken::before_file(),
            next: ParseToken::before_file(),
        },
        _compiler: None,
        module: module,
        vm: vm,
        nesting: 0,
    };

    let mut scope = PushCompiler::push_root(&mut parse_context);

    consume(scope.ctx)?; // Fill next
    consume(scope.ctx)?; // Move next -> current

    ignore_newlines(scope.ctx)?;
    loop {
        let found_eof = match_current(scope.ctx, Token::EndOfFile)?;
        if found_eof {
            break;
        }
        definition(scope.ctx)?;

        let found_newline = match_at_least_one_line(scope.ctx)?;
        // If there is no newline we must be EOF?
        if !found_newline {
            consume_expecting(scope.ctx, Token::EndOfFile, "Expect end of file.")?;
            break;
        }
    }
    emit(scope.ctx, Ops::EndModule);
    emit(scope.ctx, Ops::Return);

    let handle_undefined = |name: &str, line_number: usize| -> Result<(), WrenError> {
        Err(WrenError {
            line: line_number,
            module: scope.ctx.module_name(),
            // FIXME: Manual generation of label here is hacky.
            error: ParserError::Grammar(format!(
                "Error at '{}': {}",
                name, "Variable is used but not defined."
            )),
        })
    };

    scope
        .ctx
        .module
        .borrow()
        .check_for_undefined_variables(num_existing_variables, handle_undefined)?;

    let compiler = scope.pop();
    // wren_c uses (script) :shrug:
    let fn_obj = end_compiler(scope.ctx, compiler, 0, "<script>".into())?;
    let closure = new_handle(ObjClosure::new(scope.ctx.vm, fn_obj));
    Ok(closure)
}
