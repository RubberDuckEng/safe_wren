use std::error;
use std::fmt;
use std::rc::Rc;
use std::str;

use num_traits::FromPrimitive;

use crate::vm::{Closure, Function, Module, Value, WrenVM};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    OpTerm(char),
    OpFactor(char),
    Num(u64),
    Dot,
    Comma,
    Boolean(bool),
    Name(String),
    String(String),
    Newline,
    EndOfFile, // Does this belong here or as an Err?
}

// Is this really the tokenizer?
// Should have an input stream which it can pull (and cache) from
// And then the ability to pull more when needed and look-ahead when needed.
// Keeps track of when the current token starts, and knows how to start a new.
pub struct InputManager {
    source: Vec<u8>,
    offset: usize,
}

impl InputManager {
    pub fn from_string(source: String) -> InputManager {
        InputManager {
            source: source.as_bytes().to_vec(),
            offset: 0,
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
        self.offset += 1;
        return val;
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

// Probably belongs on the InputManager/Tokenizer?
fn next_token(input: &mut InputManager) -> Result<Token, LexError> {
    while !input.is_at_end() {
        let c = input.next();
        match c {
            b'0'..=b'9' => {
                let n = read_number(c, input)?;
                return Ok(Token::Num(n));
            }
            b'.' => return Ok(Token::Dot),
            b',' => return Ok(Token::Comma),
            b'+' | b'-' => return Ok(Token::OpTerm(c.into())),
            b'*' | b'%' => return Ok(Token::OpFactor(c.into())),
            b'(' => return Ok(Token::LeftParen),
            b')' => return Ok(Token::RightParen),
            b' ' => {
                while input.peek().unwrap_or(b'\0') == b' ' {
                    input.next();
                }
            }
            b'/' => {
                if match_char(input, b'/') {
                    skip_line_comment(input);
                    continue;
                }
                return Ok(Token::OpFactor('/'));
            }
            b'"' => return read_string(input),
            // TODO: How can we share code with is_name above?
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return Ok(read_name(c, input)?);
            }
            b'\n' => return Ok(Token::Newline),
            _ => {
                return Err(LexError::UnexpectedChar(c as char));
            }
        }
    }
    return Ok(Token::EndOfFile);
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
    // FIXME: Hack until TokenType is separate from Token
    if name.eq("true") {
        return Some(Token::Boolean(true));
    }
    if name.eq("false") {
        return Some(Token::Boolean(false));
    }
    None
}

fn is_name(c: Option<u8>) -> bool {
    matches!(c, Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'_'))
}

fn is_digit(c: Option<u8>) -> bool {
    matches!(c, Some(b'0'..=b'9'))
}

fn read_name(first_byte: u8, input: &mut InputManager) -> Result<Token, LexError> {
    // This should be a string?
    let mut bytes = Vec::new();
    bytes.push(first_byte);
    while is_name(input.peek()) || is_digit(input.peek()) {
        bytes.push(input.next());
    }

    let name = String::from_utf8(bytes).map_err(|_| LexError::DecoderError)?;
    Ok(keyword_token(&name).unwrap_or(Token::Name(name)))
}

fn read_string(input: &mut InputManager) -> Result<Token, LexError> {
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

    Ok(Token::String(string))
}

pub fn lex(input: InputManager) -> Result<Vec<Token>, LexError> {
    let mut input_manager = input;
    let mut tokens = Vec::new();
    loop {
        let token = next_token(&mut input_manager)?;
        tokens.push(token);
        if tokens.last().unwrap() == &Token::EndOfFile {
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
    Boolean(bool), // Unclear if needed, could be Constant?
    Call(Signature),
    Load(Variable),
    Pop,
    End,
}

// Only lives for the function (or module top) compile.
// Keep a stack of compilers as we recruse the tree.
#[derive(Default)]
pub struct Compiler {
    constants: Vec<Value>,
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

    fn emit_call(&mut self, signature: Signature) {
        self.code.push(Ops::Call(signature));
    }

    fn emit_pop(&mut self) {
        self.code.push(Ops::Pop);
    }

    fn emit_end(&mut self) {
        self.code.push(Ops::End);
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
    previous: Option<Token>,
    current: Option<Token>,
    next: Option<Token>,
    compiler: Compiler,
}

// Following the Pratt parser example:
// https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type PrefixParslet = fn(parser: &mut Parser) -> Result<(), ParserError>;
type InfixParslet = fn(parser: &mut Parser) -> Result<(), ParserError>;

fn literal(parser: &mut Parser) -> Result<(), ParserError> {
    // TODO: Pass in Token instead of needing to use "previous"?
    match &parser.previous {
        Some(Token::Num(n)) => parser.compiler.emit_constant(Value::Num(*n)),
        Some(Token::String(s)) => parser
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
    let rule = parser.previous.as_ref().unwrap().grammar_rule();
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
        let found_comma = parser.match_comma()?;
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
    let name = match &parser.previous {
        Some(Token::Name(n)) => Ok(n),
        _ => Err(ParserError::Grammar(
            "named_call previous token not name".into(),
        )),
    }?;
    let mut signature = Signature {
        name: name.into(),
        call_type: SignatureType::Getter,
        arity: 0,
    };

    let found_left_paren = parser.match_left_paren()?;
    if found_left_paren {
        ignore_newlines(parser)?;
        signature.call_type = SignatureType::Method;
        if !parser.peek_for_right_paren() {
            signature.arity = finish_arguments_list(parser)?;
        }
        parser.consume_expecting_right_paren()?;
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

#[derive(Debug)]
enum Scope {
    // Local,
    // Upvalue,
    Module,
}

#[derive(Debug)]
pub struct Variable {
    scope: Scope,
    pub index: usize,
}

fn name(parser: &mut Parser) -> Result<(), ParserError> {
    // This needs to be much more complicated to handle module
    // lookups as well as setters.

    // Search current scope
    // let variable = resolveNonmodule(compiler, token->start, token->length);
    // If failed handle in-method case.

    // Otherwise if in module scope handle module case:

    let name = parser.previous.as_ref().unwrap().name();
    let maybe_index = parser.module.lookup_symbol(name);
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

// Class definitions, imports, etc.
fn definition(parser: &mut Parser) -> Result<(), ParserError> {
    // We don't handle class definitions, etc. yet.
    // Fall through to the "statement" case.
    statement(parser)
}

fn grouping(parser: &mut Parser) -> Result<(), ParserError> {
    expression(parser)?;
    parser.consume_expecting_right_paren()?;
    Ok(())
}

fn boolean(parser: &mut Parser) -> Result<(), ParserError> {
    if let Some(Token::Boolean(b)) = parser.previous {
        parser.compiler.emit_boolean(b);
    } else {
        panic!("boolean called w/o boolean token");
    }
    Ok(())
}

struct GrammarRule {
    prefix: Option<PrefixParslet>,
    infix: Option<InfixParslet>,
    precedence: Precedence,
    name: Option<String>,
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
    fn name(&self) -> &String {
        match self {
            Token::Name(name) => name,
            _ => panic!("invaid"),
        }
    }

    fn grammar_rule(&self) -> GrammarRule {
        // This should switch on TokenType, not Token itself.
        match self {
            Token::LeftParen => GrammarRule::prefix(grouping),
            Token::RightParen => GrammarRule::unused(),
            Token::OpTerm(c) => GrammarRule::infix_operator(Precedence::Term, &c.to_string()),
            Token::OpFactor(c) => GrammarRule::infix_operator(Precedence::Factor, &c.to_string()),
            Token::Num(_) => GrammarRule::prefix(literal),
            Token::String(_) => GrammarRule::prefix(literal),
            Token::Dot => GrammarRule::infix(Precedence::Call, call),
            Token::Boolean(_) => GrammarRule::prefix(boolean),
            Token::Name(_) => GrammarRule::prefix(name), // TODO: Also wrong.
            Token::Comma => GrammarRule::unused(),
            Token::Newline => GrammarRule::unused(),
            Token::EndOfFile => GrammarRule::unused(),
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
        self.previous = self.current.take();
        self.current = self.next.take();
        self.next = Some(next_token(&mut self.input).map_err(|e| ParserError::Lexer(e))?);
        Ok(())
    }

    // Hack until we split TokenType and Token.
    fn peek_for_right_paren(&self) -> bool {
        match self.current {
            Some(Token::RightParen) => true,

            _ => false,
        }
    }

    // Hack until we split TokenType and Token.
    fn match_eof(&mut self) -> Result<bool, ParserError> {
        match self.current {
            Some(Token::EndOfFile) => {
                self.consume()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }
    // Hack until we split TokenType and Token.
    fn match_newline(&mut self) -> Result<bool, ParserError> {
        match self.current {
            Some(Token::Newline) => {
                self.consume()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    // Hack until we split TokenType and Token.
    fn match_comma(&mut self) -> Result<bool, ParserError> {
        match self.current {
            Some(Token::Comma) => {
                self.consume()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    // Hack until we split TokenType and Token.
    fn match_left_paren(&mut self) -> Result<bool, ParserError> {
        match self.current {
            Some(Token::LeftParen) => {
                self.consume()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    // Hack until we split TokenType and Token.
    fn match_line(&mut self) -> Result<bool, ParserError> {
        let mut saw_line = false;
        while let Some(Token::Newline) = self.current {
            self.consume()?;
            saw_line = true;
        }
        Ok(saw_line)
    }

    // Hack until we split TokenType and Token.
    fn consume_expecting_name(&mut self) -> Result<(), ParserError> {
        self.consume()?;
        match self.previous {
            Some(Token::Name(_)) => Ok(()),
            _ => Err(ParserError::Grammar("Expected name".into())),
        }
    }

    // Hack until we split TokenType and Token.
    fn consume_expecting_right_paren(&mut self) -> Result<(), ParserError> {
        self.consume()?;
        match self.previous {
            Some(Token::RightParen) => Ok(()),
            _ => Err(ParserError::Grammar("Expected right paren".into())),
        }
    }

    // Hack until we split TokenType and Token.
    fn consume_expecting_eof(&mut self) -> Result<(), ParserError> {
        self.consume()?;
        match self.previous {
            Some(Token::EndOfFile) => Ok(()),
            _ => Err(ParserError::Grammar("Expected end of file".into())),
        }
    }

    fn current_precedence(&self) -> Precedence {
        // TODO: There must be a shorter way to write this.
        // self.current being None would be a programming error/panic.
        self.current.as_ref().unwrap().grammar_rule().precedence
    }

    fn parse_precendence(&mut self, precedence: Precedence) -> Result<(), ParserError> {
        self.consume()?;
        let prefix_parser = self
            .previous
            .as_ref()
            .unwrap()
            .grammar_rule()
            .prefix
            .ok_or(ParserError::Grammar("Expected Expression".into()))?;
        prefix_parser(self)?;

        while precedence <= self.current_precedence() {
            self.consume()?;
            let infix_parser = self
                .previous
                .as_ref()
                .unwrap()
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
        previous: None,
        current: None,
        next: None,
        compiler: Compiler::default(),
        module: &vm.module,
    };
    parser.consume()?; // Fill next
    parser.consume()?; // Move next -> current

    ignore_newlines(&mut parser)?;
    loop {
        let found_eof = parser.match_eof()?;
        if found_eof {
            break;
        }
        definition(&mut parser)?;

        let found_newline = parser.match_newline()?;
        // If there is no newline we must be EOF?
        if !found_newline {
            parser.consume_expecting_eof()?;
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
