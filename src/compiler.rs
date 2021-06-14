use std::error;
use std::fmt;
use std::ops::Range;
use std::rc::Rc;
use std::str;

use num_traits::FromPrimitive;

use crate::vm::{Closure, Function, Module, Value, WrenVM};

// Token lifetimes should be tied to the Parser or InputManager.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    BeforeFile, // Error state, should never be encountered.
    LeftParen,
    RightParen,
    OpTerm(char),
    OpFactor(char),
    Num(u64),
    Dot,
    Comma,
    Var,
    Equals,
    EqualsEquals,
    Boolean(bool),
    Name(String),
    String(String),
    Newline,
    EndOfFile, // Does this belong here or as an Err?
}

#[derive(Debug)]
pub struct ParseToken {
    bytes_range: Range<usize>,
    token: Token,
    line: usize,
}

impl ParseToken {
    fn name(&self, input: &InputManager) -> Result<String, LexError> {
        String::from_utf8(input.source[self.bytes_range.clone()].into())
            .map_err(|_| LexError::DecoderError)
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
        InputManager {
            source: source.as_bytes().to_vec(),
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

    fn next(&mut self) -> u8 {
        let val = self.source[self.offset];
        if val == b'\n' {
            self.line_number += 1;
        }
        self.offset += 1;
        return val;
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

#[derive(Debug, Clone)]
pub enum LexError {
    UnexpectedChar(char),
    DecoderError,
    UnterminatedString,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexError::DecoderError => write!(f, "Decoder Error"),
            LexError::UnterminatedString => write!(f, "Unterminated String"),
            LexError::UnexpectedChar(c) => write!(f, "Unexpected char '{}'", c),
        }
    }
}

impl error::Error for LexError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            LexError::DecoderError => None,
            LexError::UnterminatedString => None,
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
    loop {
        if let Some(next) = input.peek() {
            if next == b'\n' {
                break;
            }
            input.next();
        } else {
            break;
        }
    }
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

// Probably belongs on the InputManager/Tokenizer?
fn next_token(input: &mut InputManager) -> Result<ParseToken, LexError> {
    input.token_start_offset = input.offset;
    while !input.is_at_end() {
        let c = input.next();
        match c {
            b'0'..=b'9' => {
                let num = read_number(c, input)?;
                return Ok(input.make_token(Token::Num(num)));
            }
            b'.' => return Ok(input.make_token(Token::Dot)),
            b',' => return Ok(input.make_token(Token::Comma)),
            b'+' | b'-' => return Ok(input.make_token(Token::OpTerm(c.into()))),
            b'*' | b'%' => return Ok(input.make_token(Token::OpFactor(c.into()))),
            b'(' => return Ok(input.make_token(Token::LeftParen)),
            b')' => return Ok(input.make_token(Token::RightParen)),
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
                return Ok(input.make_token(Token::OpFactor('/')));
            }
            b'=' => {
                let token = match input.peek() {
                    Some(b'=') => {
                        input.next();
                        Token::EqualsEquals
                    }
                    _ => Token::Equals,
                };
                return Ok(input.make_token(token));
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

// Knows how to advance to the end of something that looks like a number
// and then turn that into a token.
// Belongs on the Tokenizer/InputManager.
fn read_number(c: u8, input: &mut InputManager) -> Result<u64, LexError> {
    fn from_ascii_digit(c: u8) -> u64 {
        assert!(c.is_ascii_digit());
        u64::from(c - b'0')
    }

    let mut number = from_ascii_digit(c);
    while let Some(byte) = input.peek() {
        if !byte.is_ascii_digit() {
            break;
        }
        let digit = from_ascii_digit(c);
        number = number * 10 + digit;
        input.next();
    }
    Ok(number)
}

fn keyword_token(name: &str) -> Option<Token> {
    // FIXME: Hack until TokenType is separate from Token?
    match name {
        "true" => Some(Token::Boolean(true)),
        "false" => Some(Token::Boolean(false)),
        "var" => Some(Token::Var),
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

    let name = String::from_utf8(bytes).map_err(|_| LexError::DecoderError)?;
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
    let string = String::from_utf8(bytes).expect("Decoder err");

    Ok(input.make_token(Token::String(string)))
}

pub fn lex(input: &mut InputManager) -> Result<Vec<ParseToken>, LexError> {
    let input_manager = input;
    let mut tokens = Vec::new();
    loop {
        let token = next_token(input_manager)?;
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
    Assignment,
    Conditional,
    Term,   // + -
    Factor, // * / %
    Call,
}

impl Precedence {
    fn one_higher(self) -> Precedence {
        FromPrimitive::from_u8(self as u8 + 1).unwrap()
    }
}

#[derive(Debug)]
pub enum Ops {
    Constant(usize),
    Boolean(bool), // Unclear if needed, could be constant?
    Null,          // Unclear if needed, could be constant?
    Call(Signature),
    Load(Variable),
    Store(Variable),
    Pop,
    End,
}

struct Local {
    name: String,
    //   depth: i8,
}

// Only lives for the function (or module top) compile.
// Keep a stack of compilers as we recruse the tree.
#[derive(Default)]
pub struct Compiler {
    constants: Vec<Value>,
    locals: Vec<Local>, // A fixed size array in wren_c
    code: Vec<Ops>,
}

impl Compiler {
    fn emit_constant(&mut self, value: Value) {
        let index = self.constants.len();
        self.constants.push(value);
        self.code.push(Ops::Constant(index));
    }

    fn emit_boolean(&mut self, value: bool) {
        self.code.push(Ops::Boolean(value));
    }

    fn emit_null(&mut self) {
        self.code.push(Ops::Null);
    }

    fn emit_call(&mut self, signature: Signature) {
        self.code.push(Ops::Call(signature));
    }

    fn emit_pop(&mut self) {
        self.code.push(Ops::Pop);
    }

    fn emit_end(&mut self) {
        self.code.push(Ops::End);
    }

    fn emit_store(&mut self, variable: Variable) {
        self.code.push(Ops::Store(variable))
    }

    fn emit_load(&mut self, variable: Variable) {
        self.code.push(Ops::Load(variable))
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

// Takes an InputManager.  Knows how to use a Tokenizer to break it up into
// tokens one at a time.  Turns a stream of tokens into a tree of objects.
// Module / Function / Closure are likely the eventual objects?
// Parser has the lifetime of a given compilation of a module.
struct Parser<'a> {
    input: InputManager,
    module: &'a Module,
    previous: ParseToken,
    current: ParseToken,
    next: ParseToken,
    compiler: Compiler,
}

// Following the Pratt parser example:
// https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type PrefixParslet = fn(parser: &mut Parser, can_assign: bool) -> Result<(), ParserError>;
type InfixParslet = fn(parser: &mut Parser) -> Result<(), ParserError>;

fn literal(parser: &mut Parser, _can_assign: bool) -> Result<(), ParserError> {
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

#[derive(Debug)]
enum SignatureType {
    Getter,
    Method,
}

#[derive(Debug)]
pub struct Signature {
    pub name: String,
    call_type: SignatureType,
    arity: u8,
}

fn infix_op(parser: &mut Parser) -> Result<(), ParserError> {
    let rule = parser.previous.token.grammar_rule();
    ignore_newlines(parser)?;
    // Compile the right-hand side.
    parser.parse_precendence(rule.precedence.one_higher())?;

    // Call the operator method on the left-hand side.
    let signature = Signature {
        name: rule.name.unwrap(),
        call_type: SignatureType::Method,
        arity: 1,
    };
    parser.compiler.emit_call(signature);
    Ok(())
}

fn finish_arguments_list(parser: &mut Parser) -> Result<u8, ParserError> {
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
fn method_call(parser: &mut Parser) -> Result<(), ParserError> {
    // Grab name from previous token.
    let name = match &parser.previous.token {
        Token::Name(n) => Ok(n),
        _ => Err(ParserError::Grammar(
            "named_call previous token not name".into(),
        )),
    }?;
    let mut signature = Signature {
        name: name.into(),
        call_type: SignatureType::Getter,
        arity: 0,
    };

    let found_left_paren = parser.match_current(Token::LeftParen)?;
    if found_left_paren {
        ignore_newlines(parser)?;
        signature.call_type = SignatureType::Method;
        if parser.current.token != Token::RightParen {
            signature.arity = finish_arguments_list(parser)?;
        }
        parser.consume_expecting(Token::RightParen)?;
    }

    parser.compiler.emit_call(signature);

    Ok(())
}

fn call(parser: &mut Parser) -> Result<(), ParserError> {
    ignore_newlines(parser)?;
    parser.consume_expecting_name()?;
    method_call(parser)?; // namedCall in the original
    Ok(())
}

#[derive(Debug, Clone)]
pub enum Scope {
    Local,
    // Upvalue,
    Module,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub scope: Scope,
    pub index: usize,
}

fn allow_line_before_dot(parser: &mut Parser) -> Result<(), ParserError> {
    if parser.current.token == Token::Newline && parser.next.token == Token::Dot {
        parser.consume()?;
    }
    Ok(())
}

// Compiles a read or assignment to [variable].
fn bare_name(parser: &mut Parser, can_assign: bool, variable: Variable) -> Result<(), ParserError> {
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

fn name(parser: &mut Parser, can_assign: bool) -> Result<(), ParserError> {
    // This needs to be much more complicated to handle module
    // lookups as well as setters.

    let name = parser.previous.name(&parser.input)?;
    if let Some(variable) = resolve_non_module(parser, &name) {
        bare_name(parser, can_assign, variable)?;
        return Ok(());
    }

    // Otherwise if in module scope handle module case:

    let maybe_index = parser.module.lookup_symbol(&name);
    match maybe_index {
        Some(index) => {
            parser.compiler.emit_load(Variable {
                scope: Scope::Module,
                index: index,
            });
        }
        None => Err(ParserError::Grammar(format!("Undeclared {}", name)))?,
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

fn expression(parser: &mut Parser) -> Result<(), ParserError> {
    parser.parse_precendence(Precedence::Lowest)
}

// Break, continue, if, for, while, blocks, etc.
// Unlike expression, does not leave something on the stack.
fn statement(parser: &mut Parser) -> Result<(), ParserError> {
    // No statements currently implemented
    // Fall through to expression case, but pop the stack after.
    expression(parser)?;
    parser.compiler.emit_pop();
    Ok(())
}

// Create a new local variable with [name]. Assumes the current scope is local
// and the name is unique.
fn add_local(parser: &mut Parser, name: &str) {
    parser.compiler.locals.push(Local { name: name.into() });
}

fn declare_variable(parser: &mut Parser, name: &str) -> Result<(), ParserError> {
    // TODO: Check variable name max length.
    // TODO: Handle top level scope?
    // TODO: Check to see if another local with the same name exists
    // TODO: Enforce max number of local variables.
    println!("declare_variable");
    add_local(parser, name);
    Ok(())
}

fn variable_definition(parser: &mut Parser) -> Result<(), ParserError> {
    // Grab its name, but don't declare it yet. A (local) variable shouldn't be
    // in scope in its own initializer.
    parser.consume_expecting_name()?;
    let name = parser.previous.name(&parser.input)?;

    // Compile the initializer.
    if parser.match_current(Token::Equals)? {
        ignore_newlines(parser)?;
        expression(parser)?;
    } else {
        // Default initialize it to null.
        parser.compiler.emit_null();
    }

    // Now put it in scope.
    declare_variable(parser, &name)?;
    // TODO: Add define_variable in non-local case.
    Ok(())
}

// Class definitions, imports, etc.
fn definition(parser: &mut Parser) -> Result<(), ParserError> {
    // We don't handle class definitions, etc. yet.
    // Fall through to the "statement" case.
    if parser.match_current(Token::Var)? {
        variable_definition(parser)
    } else {
        statement(parser)
    }
}

fn grouping(parser: &mut Parser, _can_assign: bool) -> Result<(), ParserError> {
    expression(parser)?;
    parser.consume_expecting(Token::RightParen)?;
    Ok(())
}

fn boolean(parser: &mut Parser, _can_assign: bool) -> Result<(), ParserError> {
    if let Token::Boolean(b) = parser.previous.token {
        parser.compiler.emit_boolean(b);
    } else {
        panic!("boolean called w/o boolean token");
    }
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
    name: Option<String>, // REMOVE
}

impl GrammarRule {
    fn prefix(prefix_parselet: PrefixParslet) -> GrammarRule {
        GrammarRule {
            prefix: Some(prefix_parselet),
            infix: None,
            precedence: Precedence::None,
            name: None,
        }
    }
    fn infix_operator(precedence: Precedence, name: &str) -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: Some(infix_op),
            precedence: precedence,
            name: Some(name.to_string()),
        }
    }

    fn infix(precedence: Precedence, infix_parselet: InfixParslet) -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: Some(infix_parselet),
            precedence: precedence,
            name: None,
        }
    }

    fn unused() -> GrammarRule {
        GrammarRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
            name: None,
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
            Token::OpTerm(_) => "operator + or -",
            Token::OpFactor(_) => "operator * or / or %",
            Token::Num(_) => "number literal",
            Token::String(_) => "string literal",
            Token::Dot => "dot",
            Token::Boolean(_) => "boolean literal",
            Token::Name(_) => "name",
            Token::Comma => "comma",
            Token::Newline => "newline",
            Token::EndOfFile => "end of file",
            Token::Var => "var",
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
            Token::OpTerm(c) => GrammarRule::infix_operator(Precedence::Term, &c.to_string()),
            Token::OpFactor(c) => GrammarRule::infix_operator(Precedence::Factor, &c.to_string()),
            Token::Num(_) => GrammarRule::prefix(literal),
            Token::String(_) => GrammarRule::prefix(literal),
            Token::Dot => GrammarRule::infix(Precedence::Call, call),
            Token::Boolean(_) => GrammarRule::prefix(boolean),
            Token::Var => GrammarRule::unused(),
            Token::Comma => GrammarRule::unused(),
            Token::Newline => GrammarRule::unused(),
            Token::EndOfFile => GrammarRule::unused(),
            Token::Equals => GrammarRule::unused(),
            Token::EqualsEquals => GrammarRule::unused(), // Wrong!
            Token::Name(_) => GrammarRule::prefix(name),  // TODO: Also wrong.
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Lexer(LexError),
    Grammar(String),
}

// Errors should have
// Line and offset
// A snippet from that line?
// Some sort of error-type?

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParserError::Lexer(..) => write!(f, "Lex failure"),
            ParserError::Grammar(..) => write!(f, "Grammer failure"),
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

impl<'a> Parser<'a> {
    // This is just called nextToken(Compiler) in wren_c
    fn consume(&mut self) -> Result<(), ParserError> {
        std::mem::swap(&mut self.previous, &mut self.current);
        std::mem::swap(&mut self.current, &mut self.next);
        self.next = next_token(&mut self.input).map_err(|e| ParserError::Lexer(e))?;
        Ok(())
    }

    fn match_current(&mut self, token: Token) -> Result<bool, ParserError> {
        if self.current.token == token {
            self.consume()?;
            return Ok(true);
        }
        Ok(false)
    }

    // Hack until we split TokenType and Token.
    fn match_line(&mut self) -> Result<bool, ParserError> {
        let mut saw_line = false;
        while Token::Newline == self.current.token {
            self.consume()?;
            saw_line = true;
        }
        Ok(saw_line)
    }

    // Hack until we split TokenType and Token.
    fn consume_expecting_name(&mut self) -> Result<(), ParserError> {
        self.consume()?;
        match self.previous.token {
            Token::Name(_) => Ok(()),
            _ => Err(ParserError::Grammar("Expected name".into())),
        }
    }

    fn consume_expecting(&mut self, token: Token) -> Result<(), ParserError> {
        self.consume()?;
        let name_for_error = token.error_message_name(); // Can we avoid this?
        if self.previous.token != token {
            return Err(ParserError::Grammar(format!("Expected {}", name_for_error)));
        }
        Ok(())
    }

    fn parse_precendence(&mut self, precedence: Precedence) -> Result<(), ParserError> {
        self.consume()?;
        let prefix_parser = self
            .previous
            .token
            .grammar_rule()
            .prefix
            .ok_or(ParserError::Grammar("Expected Expression".into()))?;

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
            infix_parser(self)?;
        }
        Ok(())
    }
}

fn ignore_newlines(parser: &mut Parser) -> Result<(), ParserError> {
    parser.match_line()?;
    Ok(())
}

pub fn compile<'a>(
    vm: &'a mut WrenVM,
    input: InputManager,
    _module_name: &str,
) -> Result<Closure, ParserError> {
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

    // Initailize the fake "core" module.
    vm.module.define_variable("System", Value::Num(1));

    // Init the parser & compiler
    let mut parser = Parser {
        input: input,
        previous: ParseToken::before_file(),
        current: ParseToken::before_file(),
        next: ParseToken::before_file(),
        compiler: Compiler::default(),
        module: &vm.module,
    };
    parser.consume()?; // Fill next
    parser.consume()?; // Move next -> current

    ignore_newlines(&mut parser)?;
    loop {
        let found_eof = parser.match_current(Token::EndOfFile)?;
        if found_eof {
            break;
        }
        definition(&mut parser)?;

        let found_newline = parser.match_current(Token::Newline)?;
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
    parser.compiler.emit_end();
    Ok(Closure {
        function: Function {
            code: parser.compiler.code,
            constants: parser.compiler.constants,
        },
    })
}
