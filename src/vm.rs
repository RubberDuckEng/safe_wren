use std::error;
use std::fmt;

use num_traits::FromPrimitive;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Op(char),
    Num(u64),
    Dot,
    Keyword(String),
    Name(String),
    // Paren(char),
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
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexError::UnexpectedChar(c) => write!(f, "Unexpected char '{}'", c),
        }
    }
}

impl error::Error for LexError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            LexError::UnexpectedChar(_) => None,
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
            b'.' => {
                return Ok(Token::Dot);
            }
            b'+' | b'*' => {
                return Ok(Token::Op(c.into()));
            }
            // b'(' | b')' => {
            //     return Ok(Token::Paren(c.into()));
            // }
            b' ' => {
                while input.peek().unwrap_or(b'\0') == b' ' {
                    input.next();
                }
            }
            // TODO: How can we share code with is_name above?
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return Ok(read_name(c, input));
            }
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

const KEYWORDS: &[&str] = &["break", "continue"];

fn is_keyword(word: &str) -> bool {
    KEYWORDS.iter().any(|keyword| keyword.eq(&word))
}

fn is_name(c: Option<u8>) -> bool {
    matches!(c, Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'_'))
}

fn is_digit(c: Option<u8>) -> bool {
    matches!(c, Some(b'0'..=b'9'))
}

fn read_name(start_char: u8, input: &mut InputManager) -> Token {
    // This should be a string?
    let mut name_bytes = Vec::new();
    name_bytes.push(start_char);
    while is_name(input.peek()) || is_digit(input.peek()) {
        name_bytes.push(input.next());
    }

    let name = String::from_utf8(name_bytes).expect("Decoder err");

    if is_keyword(&name) {
        Token::Keyword(name)
    } else {
        Token::Name(name)
    }
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

// enum OpCodes {
//     End,
// }

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, FromPrimitive)]
enum Precedence {
    None = 0, // Newlines, EOF, etc.
    Lowest,
    Assignment,
    Term,
    Call,
}

// fn turn(d: Direction) -> Direction {
//     FromPrimitive::from_u8((d as u8 + 1) % 4).unwrap()
// }

impl Precedence {
    fn one_higher(self) -> Precedence {
        FromPrimitive::from_u8(self as u8 + 1).unwrap()
    }
}

#[derive(Debug)]
enum Ops {
    Constant(usize),
    Call(Signature),
    Load(Variable),
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
    fn emit_constant(&mut self, value: u64) {
        let index = self.constants.len();
        self.constants.push(Value::Num(value));
        self.code.push(Ops::Constant(index));
    }

    fn emit_call(&mut self, signature: Signature) {
        self.code.push(Ops::Call(signature));
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
    match parser.previous {
        Some(Token::Num(num)) => parser.compiler.emit_constant(num),
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
struct Signature {
    name: String,
    call_type: SignatureType,
    arity: u8,
}

fn infix_op(parser: &mut Parser) -> Result<(), ParserError> {
    let rule = parser.previous.as_ref().unwrap().grammar_rule();
    // TODO: Ignore newlines.
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
    let signature = Signature {
        name: name.into(),
        call_type: SignatureType::Getter,
        arity: 0,
    };

    // if (match(compiler, TOKEN_LEFT_PAREN)) {
    //     // Allow whitespace.
    //     signature = Signature {
    //         name: name,
    //         call_type: SignatureType::Method,
    //         arity: 0,
    //     };
    //         //     // Allow empty an argument list.
    // //     if (peek(compiler) != TOKEN_RIGHT_PAREN)
    // //     {
    // //       finishArgumentList(compiler, &called);
    // //     }
    // //     consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    // }

    parser.compiler.emit_call(signature);

    Ok(())
}

fn call(parser: &mut Parser) -> Result<(), ParserError> {
    // ignoreNewlines(compiler);
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
struct Variable {
    scope: Scope,
    index: usize,
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
            Token::Keyword(name) => name,
            Token::Name(name) => name,
            _ => panic!("invaid"),
        }
    }

    fn grammar_rule(&self) -> GrammarRule {
        // This should switch on TokenType, not Token itself.
        match self {
            Token::Op(_) => GrammarRule::infix_operator(Precedence::Term, "+"),
            Token::Num(_) => GrammarRule::prefix(literal),
            Token::EndOfFile => GrammarRule::unused(),
            Token::Dot => GrammarRule::infix(Precedence::Call, call),
            Token::Keyword(_) => GrammarRule::unused(), // TODO: This is wrong.
            Token::Name(_) => GrammarRule::prefix(name), // TODO: Also wrong.
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
    fn consume(&mut self) -> Result<(), ParserError> {
        self.previous = self.current.take();
        self.current = self.next.take();
        self.next = Some(next_token(&mut self.input).map_err(|e| ParserError::Lexer(e))?);
        Ok(())
    }

    // Hack until we split TokenType and Token.
    fn consume_expecting_name(&mut self) -> Result<(), ParserError> {
        self.consume()?;
        match self.previous {
            Some(Token::Name(_)) => Ok(()),
            _ => Err(ParserError::Grammar("Expected name".into())),
        }
    }

    fn current_precedence(&self) -> Precedence {
        // Grabs the next token to be consumed
        // Checks its precedence if it has one and returns that
        // Otherwise returns lowest precedence?
        // TODO: This should grab the GrammarRules object, not the parselet.
        let grammar = self.current.as_ref().unwrap().grammar_rule();
        match grammar.infix {
            Some(_) => grammar.precedence,
            None => Precedence::None,
        }
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

        while precedence < self.current_precedence() {
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

#[derive(Default)]
struct Module {
    // Missing some pointer to the actual code?

    // Should this just be a map?  wren_utils.h suggests so?
    variables: Vec<Value>,
    variable_names: Vec<String>,
    // name: String, // Should be a GC'd object?
}

impl Module {
    fn lookup_symbol(&self, name: &str) -> Option<usize> {
        self.variable_names.iter().position(|e| e.eq(name))
    }

    fn define_variable(&mut self, name: &str, value: Value) {
        self.variable_names.push(name.into());
        self.variables.push(value);
    }
}

#[derive(Debug)]
pub struct Function {
    constants: Vec<Value>,
    code: Vec<Ops>,
}

#[derive(Debug)]
pub struct Closure {
    function: Function,
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

    // Ignore newlines
    // loop {
    //     let token = next_token(&mut parser.input)?;
    //     if token == Token::EndOfFile {
    //         break;
    //     }
    // }
    // While not EOF
    parser.parse_precendence(Precedence::Lowest)?;

    // build definitions.
    // End of module
    // Emit return?
    parser.compiler.emit_end();
    Ok(Closure {
        function: Function {
            code: parser.compiler.code,
            constants: parser.compiler.constants,
        },
    })
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Num(u64),
}

#[derive(Debug)]
pub enum RuntimeError {
    StackUnderflow,
    // VariableAlreadyDefined,
    // TooManyVariablesDefined,
    // VariableUsedBeforeDefinition,
    NumberRequired(Value),
}

impl Value {
    fn try_into_num(self) -> Result<u64, RuntimeError> {
        match self {
            Value::Num(value) => Ok(value),
            _ => Err(RuntimeError::NumberRequired(self)),
        }
    }
}

pub struct WrenVM {
    module: Module, // No support for multiple modules yet.
    pub stack: Vec<Value>,
    pc: usize,
    // Missing pointers for wren_core.
    // Missing Global Symbol Table.
}

// enum Method {
//     Primitive,
//     ForeignFunction,
//     Closure,
// }

//   PRIMITIVE(vm->numClass, "+(_)", num_plus);

impl WrenVM {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
            stack: Vec::new(),
            pc: 0,
        }
    }

    pub fn run(&mut self, closure: Closure) -> Result<(), RuntimeError> {
        loop {
            let op = &closure.function.code[self.pc];
            self.pc += 1;
            match op {
                Ops::Constant(index) => {
                    self.push(closure.function.constants[*index]);
                }
                Ops::Call(signature) => {
                    if signature.name == "+" {
                        let a = self.pop()?.try_into_num()?;
                        let b = self.pop()?.try_into_num()?;
                        self.push(Value::Num(a + b));
                    }
                    // Get symbol # from signature?
                    // Args are on the stack.  Grab a slice?
                    // Look up the class for the first arg.
                    // If the class's method table doesn't include the symbol, bail.
                    // method = &classObj->methods.data[symbol]
                    // match on method type.
                    // If primative, make direct call.  Expecting result on the stack.
                }
                Ops::Load(variable) => {
                    let value = self.module.variables[variable.index];
                    self.push(value);
                }
                Ops::End => {
                    return Ok(());
                }
            }
        }
    }
}

impl WrenVM {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }
}
