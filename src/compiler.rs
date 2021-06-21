use std::error;
use std::fmt;
use std::ops::Range;
use std::rc::Rc;
use std::str;

use num_traits::FromPrimitive;

use crate::vm::{Closure, Function, ModuleError, Value, WrenVM};

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
    Class,
    LessThan,
    GreaterThan,
    Null,
    Var,
    While,
    Break,
    For,
    In,
    Is,
    If,
    Else,
    Equals,
    EqualsEquals,
    Boolean(bool),
    Name(String),
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
    pub fn name(&self, input: &InputManager) -> Result<String, LexError> {
        // There must be a nicer way to do this into-error on a single line?
        let name = String::from_utf8(input.source[self.bytes_range.clone()].into())?;
        Ok(name)
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
    line_number: usize,
    token_start_offset: usize,
}

impl InputManager {
    pub fn from_string(source: String) -> InputManager {
        InputManager::from_bytes(source.as_bytes().to_vec())
    }
    pub fn from_bytes(source: Vec<u8>) -> InputManager {
        InputManager {
            source: source,
            offset: 0,
            line_number: 1,
            token_start_offset: 0,
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
}

impl From<ModuleError> for ParserError {
    fn from(err: ModuleError) -> ParserError {
        ParserError::Module(err)
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
    UnterminatedScientificNotation,
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
            LexError::UnterminatedString => write!(f, "Unterminated String"),
            LexError::UnterminatedBlockComment => write!(f, "Unterminated Block Comment"),
            LexError::UnterminatedScientificNotation => {
                write!(f, "Unterminated Scientific Notation")
            }
            LexError::UnexpectedChar(c) => write!(f, "Unexpected char '{}'", c),
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
            LexError::UnterminatedBlockComment => None,
            LexError::UnterminatedScientificNotation => None,
            LexError::UnexpectedChar(_) => None,
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
            b'0'..=b'9' => {
                let num = read_number(input)?;
                return Ok(input.make_token(Token::Num(num)));
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
            b'*' | b'%' => return Ok(input.make_token(Token::OpFactor(c.into()))),
            b'<' => return Ok(input.make_token(Token::LessThan)),
            b'>' => return Ok(input.make_token(Token::GreaterThan)),
            b'(' => return Ok(input.make_token(Token::LeftParen)),
            b')' => return Ok(input.make_token(Token::RightParen)),
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
            b'"' => return read_string(input),
            // TODO: How can we share code with is_name above?
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return Ok(read_name(c, input)?);
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
    let name = str::from_utf8(&input.source[input.token_start_offset..input.offset])
        .map_err(|e| LexError::SliceDecoderError(e))?;

    let result = if is_hex {
        i32::from_str_radix(name, 16).map_err(|e| LexError::IntegerParsingError(e))? as f64
    } else {
        name.parse::<f64>()
            .map_err(|e| LexError::FloatParsingError(e))?
    };
    Ok(result)
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
    if input.peek_is(b'e') || input.peek_is(b'E') {
        // Allow a single positive/negative exponent symbol.
        if !match_char(input, b'+') {
            match_char(input, b'-');
        }
        if !input.peek_is_fn(is_digit) {
            return Err(LexError::UnterminatedScientificNotation);
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
        "null" => Some(Token::Null),
        "class" => Some(Token::Class),
        "is" => Some(Token::Is),
        "for" => Some(Token::For),
        "in" => Some(Token::In),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        _ => None,
    }
}

fn is_name(c: Option<u8>) -> bool {
    matches!(c, Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'_'))
}

fn is_digit(c: Option<u8>) -> bool {
    matches!(c, Some(b'0'..=b'9'))
}

fn read_name(first_byte: u8, input: &mut InputManager) -> Result<ParseToken, LexError> {
    // This should be a string?
    let mut bytes = Vec::new();
    bytes.push(first_byte);
    while is_name(input.peek()) || is_digit(input.peek()) {
        bytes.push(input.next());
    }

    let name = String::from_utf8(bytes)?;
    Ok(input.make_token(keyword_token(&name).unwrap_or(Token::Name(name))))
}

fn read_string(input: &mut InputManager) -> Result<ParseToken, LexError> {
    let mut bytes = Vec::new();
    loop {
        if input.is_at_end() {
            return Err(LexError::UnterminatedString);
        }
        let next = input.next();
        if next == b'"' {
            break;
        }
        bytes.push(next);
    }
    let string = String::from_utf8(bytes)?;

    Ok(input.make_token(Token::String(string)))
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

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, FromPrimitive)]
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
    fn one_higher(self) -> Precedence {
        FromPrimitive::from_u8(self as u8 + 1).unwrap()
    }
}

#[derive(Debug, PartialEq)]
pub enum Ops {
    Constant(usize),
    Boolean(bool), // Unclear if needed, could be constant?
    Null,          // Unclear if needed, could be constant?
    Call(Signature),
    Load(Variable),
    Store(Variable),
    ClassPlaceholder,
    Class(usize),

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
    End,
}

#[derive(Debug)]
struct Local {
    name: String,
    // depth: usize,
}

#[derive(Copy, Clone, PartialEq)]
enum ScopeDepth {
    Module,
    Local(usize),
}

// Only lives for the function (or module top) compile.
// Keep a stack of compilers as we recruse the tree.
pub struct Compiler {
    constants: Vec<Value>,
    locals: Vec<Local>, // A fixed size array in wren_c
    code: Vec<Ops>,
    scope_depth: ScopeDepth,
    loops: Vec<LoopOffsets>, // wren_c uses stack-allocated objects instead.

                             // The current number of slots (locals and temporaries) in use.
                             //
                             // We use this and maxSlots to track the maximum number of additional slots
                             // a function may need while executing. When the function is called, the
                             // fiber will check to ensure its stack has enough room to cover that worst
                             // case and grow the stack if needed.
                             //
                             // This value here doesn't include parameters to the function. Since those
                             // are already pushed onto the stack by the caller and tracked there, we
                             // don't need to double count them here.
                             // num_slots: usize,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            constants: Vec::new(),
            locals: Vec::new(),
            code: Vec::new(),
            scope_depth: ScopeDepth::Module,
            loops: Vec::new(),
            // num_slots: 0,
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.constants.len();
        self.constants.push(value);
        self.code.push(Ops::Constant(index));
    }

    fn emit_boolean(&mut self, value: bool) {
        self.code.push(Ops::Boolean(value));
    }

    fn emit_call(&mut self, signature: Signature) {
        self.code.push(Ops::Call(signature));
    }

    fn emit(&mut self, op: Ops) -> usize {
        self.code.push(op);
        self.code.len() - 1
    }

    fn emit_loop(&mut self, backwards_by: u16) {
        self.code.push(Ops::Loop(backwards_by))
    }

    fn emit_store(&mut self, variable: Variable) {
        self.code.push(Ops::Store(variable))
    }

    fn emit_load(&mut self, variable: Variable) {
        self.code.push(Ops::Load(variable))
    }

    fn push_scope(&mut self) {
        self.scope_depth = match self.scope_depth {
            ScopeDepth::Module => ScopeDepth::Local(0),
            ScopeDepth::Local(i) => ScopeDepth::Local(i + 1),
        }
    }
    fn pop_scope(&mut self) {
        // let popped = discard_locals(self, self.scope_depth);
        // self.num_slots -= popped;

        self.scope_depth = match self.scope_depth {
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

    // fn nested_local_scope_count(&self) -> usize {
    //     match self.scope_depth {
    //         ScopeDepth::Module => panic!("No local scopes."),
    //         ScopeDepth::Local(i) => i,
    //     }
    // }
}

impl fmt::Debug for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
            .field(&self.constants)
            .field(&self.code)
            .finish()
    }
}

// Takes an InputManager.  Knows how to use a Tokenizer to break it up into
// tokens one at a time.  Turns a stream of tokens into a tree of objects.
// Module / Function / Closure are likely the eventual objects?
// Parser has the lifetime of a given compilation of a module.
struct Parser<'a> {
    input: InputManager,
    vm: &'a mut WrenVM,
    previous: ParseToken,
    current: ParseToken,
    next: ParseToken,
    compiler: Compiler,
}

// Following the Pratt parser example:
// https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type PrefixParslet = fn(parser: &mut Parser, can_assign: bool) -> Result<(), WrenError>;
type InfixParslet = fn(parser: &mut Parser, can_assign: bool) -> Result<(), WrenError>;

fn literal(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    // TODO: Pass in Token instead of needing to use "previous"?
    match &parser.previous.token {
        Token::Num(n) => parser.compiler.emit_constant(Value::Num(*n)),
        Token::String(s) => parser
            .compiler
            .emit_constant(Value::String(Rc::new(s.clone()))),
        _ => panic!("invalid literal"),
    }
    Ok(())
}

#[derive(Debug, PartialEq)]
enum SignatureType {
    Getter,
    Method,
    Subscript,
    SubscriptSetter,
    // Setter,
    // Initializer,
}

#[derive(Debug, PartialEq)]
pub struct Signature {
    // full_name includes arity in the string.
    pub full_name: String,
    // Not sure either of these are needed once the full name is compiled
    // It's possible this struct can go away.
    call_type: SignatureType,
    pub arity: u8,
}

impl Signature {
    fn full_name(call_type: &SignatureType, name: &str, arity: u8) -> String {
        let args = (0..arity).map(|_| "_").collect::<Vec<&str>>().join(",");
        match call_type {
            SignatureType::Getter => name.into(),
            // SignatureType::Setter => format!("{}={}", name, args),
            SignatureType::Method => format!("{}({})", name, args),
            SignatureType::Subscript => format!("{}[{}]", name, args), // name should always be empty
            SignatureType::SubscriptSetter => format!("{}[{}]=({})", name, args, args), // name should always be empty.
                                                                                        // SignatureType::Initializer => format!("init {}({})", name, args),
        }
    }
    fn from_bare_name(call_type: SignatureType, name: &str, arity: u8) -> Signature {
        Signature {
            full_name: Signature::full_name(&call_type, name, arity),
            call_type: call_type,
            arity: arity,
        }
    }
}

fn infix_op(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    let rule = parser.previous.token.grammar_rule();
    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;
    ignore_newlines(parser)?;
    // Compile the right-hand side.
    parser.parse_precendence(rule.precedence.one_higher())?;

    // Call the operator method on the left-hand side.
    let signature = Signature::from_bare_name(SignatureType::Method, &name, 1);
    parser.compiler.emit_call(signature);
    Ok(())
}

// Compiles a method call with [numArgs] for a method with [name] with [length].
fn call_method(parser: &mut Parser, arity: u8, full_name: &str) {
    parser.vm.methods.ensure_method(full_name);
    let signature = Signature {
        call_type: SignatureType::Method,
        full_name: full_name.into(),
        arity: arity,
    };
    parser.compiler.emit_call(signature);
}

fn unary_op(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;

    ignore_newlines(parser)?;
    // Compile the argument.
    parser.parse_precendence(Precedence::Unary.one_higher())?;
    // Call the operator method on the left-hand side.
    call_method(parser, 0, &name);
    Ok(())
}

fn finish_arguments_list(parser: &mut Parser) -> Result<u8, WrenError> {
    let mut arg_count = 0;
    loop {
        ignore_newlines(parser)?;
        // TODO: Check and throw an error if too many parameters.
        arg_count += 1;
        expression(parser)?;
        let found_comma = parser.match_current(Token::Comma)?;
        if !found_comma {
            break;
        }
    }
    // Allow a newline before the closing delimiter.
    ignore_newlines(parser)?;
    Ok(arg_count)
}

// // Compiles an (optional) argument list for a method call with [methodSignature]
// // and then calls it.
fn method_call(parser: &mut Parser) -> Result<(), WrenError> {
    // Grab name from previous token.
    let name = match &parser.previous.token {
        Token::Name(n) => Ok(n.clone()),
        _ => Err(parser.parse_error(ParserError::Grammar(
            "named_call previous token not name".into(),
        ))),
    }?;
    let mut call_type = SignatureType::Getter;
    let mut arity = 0;

    let found_left_paren = parser.match_current(Token::LeftParen)?;
    if found_left_paren {
        ignore_newlines(parser)?;
        call_type = SignatureType::Method;
        if parser.current.token != Token::RightParen {
            arity = finish_arguments_list(parser)?;
        }
        parser.consume_expecting(Token::RightParen)?;
    }

    let signature = Signature::from_bare_name(call_type, &name, arity);
    parser.compiler.emit_call(signature);

    Ok(())
}

fn call(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    ignore_newlines(parser)?;
    parser.consume_expecting_name()?;
    method_call(parser)?; // namedCall in the original
    Ok(())
}

fn or_(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    ignore_newlines(parser)?;

    // Skip the right argument if the left is true.
    let jump = parser.compiler.emit(Ops::OrPlaceholder);
    parser.parse_precendence(Precedence::LogicalOr)?;
    parser.compiler.patch_jump(jump);
    Ok(())
}

fn and_(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    ignore_newlines(parser)?;

    // Skip the right argument if the left is true.
    let jump = parser.compiler.emit(Ops::AndPlaceholder);
    parser.parse_precendence(Precedence::LogicalAnd)?;
    parser.compiler.patch_jump(jump);
    Ok(())
}

// Subscript or "array indexing" operator like `foo[bar]`.
fn subscript(parser: &mut Parser, can_assign: bool) -> Result<(), WrenError> {
    let mut arity = finish_arguments_list(parser)?;
    // Isn't arity always 1 here?
    parser.consume_expecting(Token::RightSquareBracket)?;
    allow_line_before_dot(parser)?;

    let mut call_type = SignatureType::Subscript;
    if can_assign && parser.match_current(Token::Equals)? {
        call_type = SignatureType::SubscriptSetter;
        arity += 1; // Why? I guess the value being set?
                    // Compile the assigned value.
                    // Validate # of parameters.
        expression(parser)?;
    }
    // Name is always empty for Subscripts, I think?
    let signature = Signature::from_bare_name(call_type, "", arity);
    parser.compiler.emit_call(signature);
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Local,
    // Upvalue,
    Module,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub scope: Scope,
    pub index: usize,
}

fn allow_line_before_dot(parser: &mut Parser) -> Result<(), WrenError> {
    if parser.current.token == Token::Newline && parser.next.token == Token::Dot {
        parser.consume()?;
    }
    Ok(())
}

// Compiles a read or assignment to [variable].
fn bare_name(parser: &mut Parser, can_assign: bool, variable: Variable) -> Result<(), WrenError> {
    // If there's an "=" after a bare name, it's a variable assignment.
    if can_assign && parser.match_current(Token::Equals)? {
        // Compile the right-hand side.
        expression(parser)?;
        parser.compiler.emit_store(variable.clone());
    }

    parser.compiler.emit_load(variable);

    allow_line_before_dot(parser)?;
    Ok(())
}

fn resolve_non_module(parser: &Parser, name: &str) -> Option<Variable> {
    if let Some(index) = parser.compiler.locals.iter().position(|l| l.name.eq(name)) {
        return Some(Variable {
            scope: Scope::Local,
            index: index,
        });
    }

    None
}

fn name(parser: &mut Parser, can_assign: bool) -> Result<(), WrenError> {
    // This needs to be much more complicated to handle module
    // lookups as well as setters.

    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;
    if let Some(variable) = resolve_non_module(parser, &name) {
        bare_name(parser, can_assign, variable)?;
        return Ok(());
    }

    // Otherwise if in module scope handle module case:

    let maybe_index = parser.vm.module.lookup_symbol(&name);
    match maybe_index {
        Some(index) => {
            parser.compiler.emit_load(Variable {
                scope: Scope::Module,
                index: index,
            });
        }
        None => Err(parser.parse_error(ParserError::Grammar(format!(
            "Undeclared variable '{}'",
            name
        ))))?,
    }

    // variable.scope = SCOPE_MODULE;
    // variable.index = wrenSymbolTableFind(&compiler->parser->module->variableNames,
    //                                      token->start, token->length);

    // Otherwise define a variable and hope it's filled in later (by what?)
    //     // Implicitly define a module-level variable in
    // // the hopes that we get a real definition later.
    // variable.index = wrenDeclareVariable(compiler->parser->vm,
    //     compiler->parser->module,
    //     token->start, token->length,
    //     token->line);
    // loadVariable(compiler, variable);
    Ok(())
}

fn expression(parser: &mut Parser) -> Result<(), WrenError> {
    parser.parse_precendence(Precedence::Lowest)
}

// Bookkeeping information for the current loop being compiled.
#[derive(Default, Copy, Clone)]
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
    //    scope_depth: usize,
}

impl Compiler {
    fn start_loop(&mut self) -> u8 {
        self.loops.push(LoopOffsets {
            start: self.code.len(), // Unclear why wren_c has code - 1 here?
            //scope_depth: self.scope_depth,
            body: 0,
            exit_jump: 0,
        });
        (self.loops.len() - 1) as u8
    }
}

fn loop_body(parser: &mut Parser) -> Result<(), WrenError> {
    parser.compiler.loops.last_mut().unwrap().body = parser.compiler.code.len();
    statement(parser)
}

impl Compiler {
    fn offset_to_current_pc_from_after(&self, offset: usize) -> u16 {
        // We are commonly passed in the instruction offset
        // we're about to fix.  However our output is used
        // as the offset from *after* that instruction to the
        // end of the current PC.  Hence after = offset + 1
        let after = offset + 1;
        (self.code.len() - after) as u16
    }

    fn end_loop(&mut self) {
        let offsets = self.loops.pop().expect("end_loop called with no loop!");
        // Emit a loop instruction which jumps to start of current loop.
        // Measures from *after* start and doesn't include this Loop, so +2.
        self.emit_loop(self.offset_to_current_pc_from_after(offsets.start) + 2);
        // Load up the exitLoop instruction and patch it to the current code offset.
        self.patch_jump(offsets.exit_jump);

        // Find any break placeholders and make them real jumps.
        for i in offsets.body..self.code.len() {
            if self.code[i] == Ops::JumpPlaceholder || self.code[i] == Ops::JumpIfFalsePlaceholder {
                self.patch_jump(i)
            }
        }
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

fn load_local(parser: &mut Parser, index: usize) {
    parser.compiler.emit_load(Variable {
        scope: Scope::Local,
        index: index,
    })
}

fn test_exit_loop(compiler: &mut Compiler) {
    compiler.loops.last_mut().unwrap().exit_jump = compiler.emit(Ops::JumpIfFalsePlaceholder);
}

fn for_hidden_variable_scope(parser: &mut Parser) -> Result<(), WrenError> {
    parser.consume_expecting(Token::LeftParen)?;
    parser.consume_expecting_name()?;

    // Remember the name of the loop variable.
    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;

    parser.consume_expecting(Token::In)?;
    ignore_newlines(parser)?;

    // Evaluate the sequence expression and store it in a hidden local variable.
    // The space in the variable name ensures it won't collide with a user-defined
    // variable.
    expression(parser)?;

    // FIXME: Ensure there is enough local space.

    let seq_slot = add_local(parser, "seq ".into());

    // Create another hidden local for the iterator object.
    null(parser, false)?;
    let iter_slot = add_local(parser, "iter ".into());

    parser.consume_expecting(Token::RightParen)?;

    parser.compiler.start_loop();

    // Advance the iterator by calling the ".iterate" method on the sequence.
    load_local(parser, seq_slot);
    load_local(parser, iter_slot);

    // Update and test the iterator.
    call_method(parser, 1, "iterate(_)");
    parser.compiler.emit_store(Variable {
        scope: Scope::Local,
        index: iter_slot,
    });
    test_exit_loop(&mut parser.compiler);

    // Get the current value in the sequence by calling ".iteratorValue".
    load_local(parser, seq_slot);
    load_local(parser, iter_slot);
    call_method(parser, 1, "iteratorValue(_)");

    // Bind the loop variable in its own scope. This ensures we get a fresh
    // variable each iteration so that closures for it don't all see the same one.
    parser.compiler.push_scope();
    add_local(parser, name);

    loop_body(parser)?;

    // Loop variable.
    parser.compiler.pop_scope();

    parser.compiler.end_loop();
    Ok(())
}

fn for_statement(parser: &mut Parser) -> Result<(), WrenError> {
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
    auto_scope(parser, for_hidden_variable_scope)
}

fn if_statement(parser: &mut Parser) -> Result<(), WrenError> {
    // Compile the condition.
    parser.consume_expecting(Token::LeftParen)?;
    expression(parser)?;
    parser.consume_expecting(Token::RightParen)?;
    // Jump to the else branch if the condition is false.
    let if_jump = parser.compiler.emit(Ops::JumpIfFalsePlaceholder);
    // Compile the then branch.
    statement(parser)?;
    // Compile the else branch if there is one.
    if parser.match_current(Token::Else)? {
        // Jump over the else branch when the if branch is taken.
        let else_jump = parser.compiler.emit(Ops::JumpPlaceholder);
        parser.compiler.patch_jump(if_jump);
        statement(parser)?;
        // Patch the jump over the else.
        parser.compiler.patch_jump(else_jump);
    } else {
        parser.compiler.patch_jump(if_jump);
    }
    Ok(())
}

fn while_statement(parser: &mut Parser) -> Result<(), WrenError> {
    parser.compiler.start_loop();

    // Compile the condition.
    parser.consume_expecting(Token::LeftParen)?;
    expression(parser)?;
    parser.consume_expecting(Token::RightParen)?;

    test_exit_loop(&mut parser.compiler);
    loop_body(parser)?;
    parser.compiler.end_loop();
    Ok(())
}

// Consumes the current token. Emits an error if it is not a newline. Then
// discards any duplicate newlines following it.
fn consume_at_least_one_line(parser: &mut Parser) -> Result<(), WrenError> {
    parser.consume_expecting(Token::Newline)?;
    ignore_newlines(parser)
}

// Parses a block body, after the initial "{" has been consumed.
//
// Returns true if it was a expression body, false if it was a statement body.
// (More precisely, returns true if a value was left on the stack. An empty
// block returns false.)
fn finish_block(parser: &mut Parser) -> Result<bool, WrenError> {
    // Empty blocks do nothing. (Is this required or an optimization?)
    if parser.match_current(Token::RightCurlyBrace)? {
        return Ok(false);
    }

    // This is a bit magical of Wren...
    // If there's no line after the "{", it's a single-expression body.
    if !parser.match_at_least_one_line()? {
        expression(parser)?;
        parser.consume_expecting(Token::RightCurlyBrace)?;
        return Ok(true);
    }

    // Empty blocks (with just a newline inside) do nothing.
    if parser.match_current(Token::RightCurlyBrace)? {
        return Ok(false);
    }

    // Compile the definition list.
    loop {
        definition(parser)?;
        consume_at_least_one_line(parser)?;
        if parser.current.token == Token::RightCurlyBrace
            || parser.current.token == Token::EndOfFile
        {
            break;
        }
    }

    parser.consume_expecting(Token::RightCurlyBrace)?;
    return Ok(false);
}

fn auto_scope(
    parser: &mut Parser,
    f: fn(&mut Parser) -> Result<(), WrenError>,
) -> Result<(), WrenError> {
    parser.compiler.push_scope();
    let v = f(parser);
    parser.compiler.pop_scope();
    return v;
}

// Generates code to discard local variables at [depth] or greater. Does *not*
// actually undeclare variables or pop any scopes, though. This is called
// directly when compiling "break" statements to ditch the local variables
// before jumping out of the loop even though they are still in scope *past*
// the break instruction.
//
// Returns the number of local variables that were eliminated.
// fn discard_locals(compiler: &mut Compiler, scope_depth: ScopeDepth) -> usize {
//     let depth = match scope_depth {
//         ScopeDepth::Module => panic!("Can't discard locals at module level."),
//         ScopeDepth::Local(i) => i,
//     };

//     let starting_locals_len = compiler.locals.len();
//     while let Some(local) = compiler.locals.last() {
//         if local.depth < depth {
//             break;
//         }
//         // FIXME: Handle upvalues.
//         compiler.emit(Ops::Pop);
//         compiler.locals.pop();
//     }

//     return starting_locals_len - compiler.locals.len();
// }

// Break, continue, if, for, while, blocks, etc.
// Unlike expression, does not leave something on the stack.
fn statement(parser: &mut Parser) -> Result<(), WrenError> {
    // TODO: Many more statements to implement!

    if parser.match_current(Token::Break)? {
        if parser.compiler.loops.is_empty() {
            return Err(parser.parse_error(ParserError::Grammar(
                "Cannot use 'break' outside of a loop.".into(),
            )));
        }

        // Since we will be jumping out of the scope, make sure any locals in it
        // are discarded first.
        //   discardLocals(compiler, compiler->loop->scopeDepth + 1);

        // Emit a placeholder instruction for the jump to the end of the body.
        // We'll fix these up with real Jumps at the end of compiling this loop.
        parser.compiler.emit(Ops::JumpPlaceholder); // Break placeholder
    } else if parser.match_current(Token::If)? {
        if_statement(parser)?;
    } else if parser.match_current(Token::For)? {
        for_statement(parser)?;
    } else if parser.match_current(Token::While)? {
        while_statement(parser)?;
    } else if parser.match_current(Token::LeftCurlyBrace)? {
        // Block statement.
        fn finish(p: &mut Parser) -> Result<(), WrenError> {
            if finish_block(p)? {
                // Block was an expression, so discard it.
                p.compiler.emit(Ops::Pop);
            }
            Ok(())
        }
        auto_scope(parser, finish)?;
    } else {
        expression(parser)?;
        parser.compiler.emit(Ops::Pop);
    }
    Ok(())
}

// Create a new local variable with [name]. Assumes the current scope is local
// and the name is unique.
fn add_local(parser: &mut Parser, name: String) -> usize {
    parser.compiler.locals.push(Local {
        name: name,
        // depth: parser.compiler.nested_local_scope_count(),
    });
    parser.compiler.locals.len() - 1
}

fn declare_variable(parser: &mut Parser, name: String) -> Result<usize, WrenError> {
    // TODO: Check variable name max length.

    // Top-level module scope.
    // if parser.compiler.scope_depth == ScopeDepth::Module {
    //     // Error handling missing.
    //     // Error handling should occur inside wren_define_variable, no?
    //     let result = wren_define_variable(&mut parser.vm.module, &name, Value::Null);
    //     let symbol = result.map_err(|e| parser.parse_error(ParserError::Module(e)))?;
    //     return Ok(symbol);
    // }

    // TODO: Check to see if another local with the same name exists
    // TODO: Enforce max number of local variables.
    Ok(add_local(parser, name))
}

// Parses a name token and declares a variable in the current scope with that
// name. Returns its slot.
fn declare_named_variable(parser: &mut Parser) -> Result<usize, WrenError> {
    parser.consume_expecting_name()?;
    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;
    declare_variable(parser, name)
}

fn variable_definition(parser: &mut Parser) -> Result<(), WrenError> {
    // Grab its name, but don't declare it yet. A (local) variable shouldn't be
    // in scope in its own initializer.
    parser.consume_expecting_name()?;
    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;

    // Compile the initializer.
    if parser.match_current(Token::Equals)? {
        ignore_newlines(parser)?;
        expression(parser)?;
    } else {
        // Default initialize it to null.
        parser.compiler.emit(Ops::Null);
    }

    // Now put it in scope.
    declare_variable(parser, name)?;
    // TODO: Add define_variable in non-local case.
    Ok(())
}

// FIXME: This is probably not needed?  All it really adds is an assert?
fn load_core_variable(parser: &mut Parser, name: &str) {
    let symbol = parser.vm.module.lookup_symbol(name).unwrap();
    parser.compiler.emit_load(Variable {
        scope: Scope::Module,
        index: symbol,
    });
}

fn define_variable(parser: &mut Parser, variable: Variable) {
    match variable.scope {
        Scope::Local => {
            // Store the variable. If it's a local, the result of the initializer is
            // in the correct slot on the stack already so we're done.
            return;
        }
        Scope::Module => {
            // It's a module-level variable, so store the value in the module slot and
            // then discard the temporary for the initializer.
            parser.compiler.emit_load(variable);
            parser.compiler.emit(Ops::Pop);
        }
    }
}

// TODO: is_foreign should probably be an enum.
fn class_definition(parser: &mut Parser, _is_foreign: bool) -> Result<(), WrenError> {
    // Create a variable to store the class in.
    let class_variable = Variable {
        scope: Scope::Module, // Local not supported yet.
        index: declare_named_variable(parser)?,
    };

    // FIXME: Should declare_named_variable return the name too?
    let name = parser
        .previous
        .name(&parser.input)
        .map_err(|e| parser.parse_error(e.into()))?;

    let class_name = Value::String(Rc::new(name));

    // Make a string constant for the name.
    parser.compiler.emit_constant(class_name);
    // FIXME: Handle superclasses, TOKEN_IS

    // Implicitly inherit from Object.
    load_core_variable(parser, "Object");

    // Store a placeholder for the number of fields argument. We don't know the
    // count until we've compiled all the methods to see which fields are used.
    let class_instruction = parser.compiler.emit(Ops::ClassPlaceholder);

    // Store it in its name. (in the module case)
    define_variable(parser, class_variable);

    // Push a local variable scope. Static fields in a class body are hoisted out
    // into local variables declared in this scope. Methods that use them will
    // have upvalues referencing them.

    fn finish_class(parser: &mut Parser) -> Result<(), WrenError> {
        //   ClassInfo classInfo;
        //   classInfo.isForeign = isForeign;
        //   classInfo.name = className;

        //   // Allocate attribute maps if necessary.
        //   // A method will allocate the methods one if needed
        //   classInfo.classAttributes = compiler->attributes->count > 0
        //         ? wrenNewMap(compiler->parser->vm)
        //         : NULL;
        //   classInfo.methodAttributes = NULL;
        //   // Copy any existing attributes into the class
        //   copyAttributes(compiler, classInfo.classAttributes);

        //   // Set up a symbol table for the class's fields. We'll initially compile
        //   // them to slots starting at zero. When the method is bound to the class, the
        //   // bytecode will be adjusted by [wrenBindMethod] to take inherited fields
        //   // into account.
        //   wrenSymbolTableInit(&classInfo.fields);

        //   // Set up symbol buffers to track duplicate static and instance methods.
        //   wrenIntBufferInit(&classInfo.methods);
        //   wrenIntBufferInit(&classInfo.staticMethods);
        //   compiler->enclosingClass = &classInfo;

        // Compile the method definitions.
        parser.consume_expecting(Token::LeftCurlyBrace)?;
        parser.match_at_least_one_line()?;

        while !parser.match_current(Token::RightCurlyBrace)? {
            // FIXME: No method suport yet!
            // if (!method(compiler, classVariable)) break;

            // Don't require a newline after the last definition.
            if parser.match_current(Token::RightCurlyBrace)? {
                break;
            }

            consume_at_least_one_line(parser)?;
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
        Ok(())
    }
    auto_scope(parser, finish_class)?;

    let num_fields = 0; // FIXME
    parser.compiler.code[class_instruction] = Ops::Class(num_fields);
    Ok(())
}

// Class definitions, imports, etc.
fn definition(parser: &mut Parser) -> Result<(), WrenError> {
    // We don't handle class definitions, etc. yet.

    if parser.match_current(Token::Class)? {
        return class_definition(parser, false);
    }

    // Fall through to the "statement" case.
    if parser.match_current(Token::Var)? {
        variable_definition(parser)
    } else {
        statement(parser)
    }
}

fn grouping(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    expression(parser)?;
    parser.consume_expecting(Token::RightParen)?;
    Ok(())
}

fn boolean(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    if let Token::Boolean(b) = parser.previous.token {
        parser.compiler.emit_boolean(b);
    } else {
        panic!("boolean called w/o boolean token");
    }
    Ok(())
}

fn null(parser: &mut Parser, _can_assign: bool) -> Result<(), WrenError> {
    parser.compiler.emit(Ops::Null);
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
    // Eventually signature_function for generating correct method names
    // from operator tokens.
    precedence: Precedence,
}

impl GrammarRule {
    fn prefix(prefix_parselet: PrefixParslet) -> GrammarRule {
        GrammarRule {
            prefix: Some(prefix_parselet),
            infix: None,
            precedence: Precedence::None,
        }
    }

    fn infix_operator(precedence: Precedence) -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: Some(infix_op),
            precedence: precedence,
        }
    }

    fn prefix_operator() -> GrammarRule {
        GrammarRule {
            prefix: Some(unary_op),
            infix: None,
            precedence: Precedence::None,
        }
    }
    fn infix(precedence: Precedence, infix_parselet: InfixParslet) -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: Some(infix_parselet),
            precedence: precedence,
        }
    }

    fn unused() -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }
    }
}

// Parslets belong in some sort of grouped parslet object per token?
impl Token {
    fn error_message_name(&self) -> &'static str {
        match self {
            Token::BeforeFile => unimplemented!(),
            Token::LeftParen => "left paren",
            Token::RightParen => "right paren",
            Token::LeftCurlyBrace => "left curly brace",
            Token::RightCurlyBrace => "right curly brace",
            Token::LeftSquareBracket => "left square bracket",
            Token::RightSquareBracket => "right square bracket",
            Token::Plus => "plus sign",
            Token::Minus => "minus sign",
            Token::Hash => "hash (attributes)",
            Token::Pipe => "bitwise or",
            Token::PipePipe => "logical or",
            Token::Amp => "bitwise and",
            Token::AmpAmp => "logical and",
            Token::Bang => "bang / unary not",
            Token::BangEquals => "not equals",
            Token::OpFactor(_) => "operator * or / or %",
            Token::Num(_) => "number literal",
            Token::String(_) => "string literal",
            Token::Dot => "dot (call)",
            Token::DotDot => "dot dot (inclusive range)",
            Token::DotDotDot => "dot dot dot (exclusive range)",
            Token::Boolean(_) => "boolean literal",
            Token::Name(_) => "name",
            Token::Comma => "comma",
            Token::For => "for",
            Token::In => "in",
            Token::Null => "null",
            Token::Class => "class",
            Token::LessThan => "less than",
            Token::GreaterThan => "greater than",
            Token::Newline => "newline",
            Token::EndOfFile => "end of file",
            Token::Var => "var",
            Token::Is => "is",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::Break => "break",
            Token::Equals => "equal sign",
            Token::EqualsEquals => "==",
        }
    }

    fn grammar_rule(&self) -> GrammarRule {
        // This should switch on TokenType, not Token itself.
        match self {
            Token::BeforeFile => unimplemented!(),
            Token::LeftParen => GrammarRule::prefix(grouping),
            Token::RightParen => GrammarRule::unused(),
            // Unclear if subscriptSignature is needed?
            Token::LeftCurlyBrace => GrammarRule::unused(), // WRONG, should be prefix(map)
            Token::RightCurlyBrace => GrammarRule::unused(),
            Token::RightSquareBracket => GrammarRule::unused(),
            Token::LeftSquareBracket => GrammarRule {
                prefix: None, // FIXME: prefix: Some(list),
                infix: Some(subscript),
                precedence: Precedence::Call,
            },
            Token::Minus => GrammarRule {
                prefix: Some(unary_op),
                infix: Some(infix_op),
                precedence: Precedence::Term,
            },
            Token::Plus => GrammarRule::infix_operator(Precedence::Term),
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
            Token::Pipe => GrammarRule::infix_operator(Precedence::BitwiseOr),
            Token::PipePipe => GrammarRule::infix(Precedence::LogicalOr, or_),
            Token::Amp => GrammarRule::infix_operator(Precedence::BitwiseAnd),
            Token::AmpAmp => GrammarRule::infix(Precedence::LogicalOr, and_),
            Token::For => GrammarRule::unused(),
            Token::In => GrammarRule::unused(),
            Token::If => GrammarRule::unused(),
            Token::Else => GrammarRule::unused(),
            Token::Class => GrammarRule::unused(),
            Token::While => GrammarRule::unused(),
            Token::Break => GrammarRule::unused(),
            Token::LessThan => GrammarRule::infix_operator(Precedence::Comparison),
            Token::GreaterThan => GrammarRule::infix_operator(Precedence::Comparison),
            Token::Comma => GrammarRule::unused(),
            Token::Newline => GrammarRule::unused(),
            Token::EndOfFile => GrammarRule::unused(),
            Token::Equals => GrammarRule::unused(),
            Token::EqualsEquals => GrammarRule::infix_operator(Precedence::Equality),
            Token::Name(_) => GrammarRule::prefix(name), // TODO: Also wrong.
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Lexer(LexError),
    Grammar(String),
    Module(ModuleError),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParserError::Lexer(..) => write!(f, "Lex failure"),
            ParserError::Grammar(..) => write!(f, "Grammer failure"),
            ParserError::Module(..) => write!(f, "Module failure"),
        }
    }
}

impl error::Error for ParserError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            ParserError::Lexer(ref e) => Some(e),
            ParserError::Grammar(..) => None,
            ParserError::Module(..) => None,
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

impl<'a> Parser<'a> {
    // This is just called nextToken(Compiler) in wren_c
    fn consume(&mut self) -> Result<(), WrenError> {
        std::mem::swap(&mut self.previous, &mut self.current);
        std::mem::swap(&mut self.current, &mut self.next);
        self.next = next_token(&mut self.input).map_err(|e| self.parse_error(e.into()))?;
        Ok(())
    }

    fn match_current(&mut self, token: Token) -> Result<bool, WrenError> {
        if self.current.token == token {
            self.consume()?;
            return Ok(true);
        }
        Ok(false)
    }

    fn match_at_least_one_line(&mut self) -> Result<bool, WrenError> {
        let mut saw_line = false;
        while Token::Newline == self.current.token {
            self.consume()?;
            saw_line = true;
        }
        Ok(saw_line)
    }

    // Hack until we split TokenType and Token.
    fn consume_expecting_name(&mut self) -> Result<(), WrenError> {
        self.consume()?;
        match self.previous.token {
            Token::Name(_) => Ok(()),
            _ => Err(self.parse_error(ParserError::Grammar("Expected name".into()))),
        }
    }

    fn parse_error(&self, error: ParserError) -> WrenError {
        WrenError {
            line: self.input.line_number,
            module: self.vm.module.name.clone(),
            error: error,
        }
    }

    fn consume_expecting(&mut self, token: Token) -> Result<(), WrenError> {
        self.consume()?;
        let name_for_error = token.error_message_name(); // Can we avoid this?
        if self.previous.token != token {
            return Err(self.parse_error(ParserError::Grammar(format!(
                "Expected {}, found: {:?}",
                name_for_error, self.previous.token
            ))));
        }
        Ok(())
    }

    fn parse_precendence(&mut self, precedence: Precedence) -> Result<(), WrenError> {
        self.consume()?;
        let prefix_parser = self
            .previous
            .token
            .grammar_rule()
            .prefix
            .ok_or(self.parse_error(ParserError::Grammar(format!(
                "Expected Expression: {:?}",
                self.previous
            ))))?;

        // Track if the precendence of the surrounding expression is low enough to
        // allow an assignment inside this one. We can't compile an assignment like
        // a normal expression because it requires us to handle the LHS specially --
        // it needs to be an lvalue, not an rvalue. So, for each of the kinds of
        // expressions that are valid lvalues -- names, subscripts, fields, etc. --
        // we pass in whether or not it appears in a context loose enough to allow
        // "=". If so, it will parse the "=" itself and handle it appropriately.
        let can_assign = precedence <= Precedence::Conditional;
        prefix_parser(self, can_assign)?;

        while precedence <= self.current.token.grammar_rule().precedence {
            self.consume()?;
            let infix_parser = self
                .previous
                .token
                .grammar_rule()
                .infix
                .expect("Invalid token");
            infix_parser(self, can_assign)?;
        }
        Ok(())
    }
}

fn ignore_newlines(parser: &mut Parser) -> Result<(), WrenError> {
    parser.match_at_least_one_line()?;
    Ok(())
}

pub(crate) fn compile<'a>(
    vm: &'a mut WrenVM,
    input: InputManager,
    _module_name: &str,
) -> Result<Closure, WrenError> {
    // TODO: We should create one per module_name instead.

    // When compiling, we create a module and register it.
    // We automatically import Core into all modules.
    // Implicitly import the core module.
    // ObjModule* coreModule = getModule(vm, NULL_VAL);
    // for (int i = 0; i < coreModule->variables.count; i++)
    // {
    //   wrenDefineVariable(vm, module,
    //                      coreModule->variableNames.data[i]->value,
    //                      coreModule->variableNames.data[i]->length,
    //                      coreModule->variables.data[i], NULL);
    // }

    // If we init the module we need to copy over the core stuff.
    // vm.module = Module::with_name(module_name);

    // Init the parser & compiler
    let mut parser = Parser {
        input: input,
        previous: ParseToken::before_file(),
        current: ParseToken::before_file(),
        next: ParseToken::before_file(),
        compiler: Compiler::new(),
        vm: vm,
    };
    parser.consume()?; // Fill next
    parser.consume()?; // Move next -> current

    // Make a new compiler!

    ignore_newlines(&mut parser)?;
    loop {
        let found_eof = parser.match_current(Token::EndOfFile)?;
        if found_eof {
            break;
        }
        definition(&mut parser)?;

        let found_newline = parser.match_at_least_one_line()?;
        // If there is no newline we must be EOF?
        if !found_newline {
            parser.consume_expecting(Token::EndOfFile)?;
            break;
        }
    }
    // parser.emit_end_module();
    // parser.emit_return();

    // FIXME: Check for undefined implicit variables and throw errors.

    // FIXME: Missing lots of "endCompiler" cleanup here.
    parser.compiler.emit(Ops::End);
    Ok(Closure {
        function: Function {
            code: parser.compiler.code,
            constants: parser.compiler.constants,
        },
    })
}
