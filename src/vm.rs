use std::fmt;

use num_traits::FromPrimitive;

// MVP: Able to parse/execute "1+1"
// Lexer
// Compiler
// VM

// Unclear how to check against a type concisely?
// matches!(a, Token::Num(_)) maybe?
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Op(char),
    Num(u64),
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
    UnexpectedChar,
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
            _ => {
                return Err(LexError::UnexpectedChar);
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

#[allow(dead_code)]
fn lex(input: &mut InputManager) -> Vec<Token> {
    let mut tokens = Vec::new();
    while let Ok(token) = next_token(input) {
        tokens.push(token);
        if tokens.last().unwrap() == &Token::EndOfFile {
            break;
        }
    }
    tokens
}

// Keeps the buffer of compiled code for the compiled function.
// Eventually will keep a few other things?
#[derive(Debug, Clone)]
struct WrenFunction {}

// enum OpCodes {
//     End,
// }

// struct WrenCompiler {
//     input: InputManager,
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
    End,
}

#[derive(Default)]
pub struct Program {
    constants: Vec<Value>,
    code: Vec<Ops>,
}

impl Program {
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
}

impl fmt::Debug for Program {
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
struct Parser {
    input: InputManager,
    previous: Option<Token>,
    current: Option<Token>,
    next: Option<Token>,
    program: Program,
}

// Following the Pratt parser example:
// https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type PrefixParslet = fn(parser: &mut Parser) -> Result<(), ParserError>;
type InfixParslet = fn(parser: &mut Parser) -> Result<(), ParserError>;

fn literal(parser: &mut Parser) -> Result<(), ParserError> {
    // TODO: Pass in Token instead of needing to use "previous"?
    println!("{:?}", parser.previous);
    match parser.previous {
        Some(Token::Num(num)) => parser.program.emit_constant(num),
        _ => panic!("invalid literal"),
    }
    Ok(())
}

#[derive(Debug)]
enum SignatureType {
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
    parser.parse_expression(rule.precedence.one_higher())?;

    // Call the operator method on the left-hand side.
    let signature = Signature {
        name: rule.name.unwrap(),
        call_type: SignatureType::Method,
        arity: 1,
    };
    parser.program.emit_call(signature);
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
    fn grammar_rule(&self) -> GrammarRule {
        match self {
            Token::Op(_) => GrammarRule::infix_operator(Precedence::Term, "+"),
            Token::Num(_) => GrammarRule::prefix(literal),
            Token::EndOfFile => GrammarRule::unused(),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Lexer(LexError),
    Grammar(String),
}

impl Parser {
    fn consume(&mut self) -> Result<(), ParserError> {
        self.previous = self.current.take();
        self.current = self.next.take();
        self.next = Some(next_token(&mut self.input).map_err(|e| ParserError::Lexer(e))?);
        Ok(())
    }

    fn next_precedence(&self) -> Precedence {
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
    fn parse_expression(&mut self, precedence: Precedence) -> Result<(), ParserError> {
        let prefix_parser = self
            .current
            .as_ref()
            .unwrap()
            .grammar_rule()
            .prefix
            .ok_or(ParserError::Grammar("Expected Expression".to_string()))?;
        self.consume()?;
        prefix_parser(self)?;

        while precedence < self.next_precedence() {
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

pub fn compile(input: InputManager) -> Result<Program, ParserError> {
    // Init the parser & compiler
    // let mut compiler = WrenCompiler { input };
    let mut parser = Parser {
        input: input,
        previous: None,
        current: None,
        next: None,
        program: Program::default(),
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
    parser.parse_expression(Precedence::Lowest)?;

    // build definitions.
    // End of module
    // Emit return?
    parser.program.emit_end();
    Ok(parser.program)
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Num(u64),
}

#[derive(Debug)]
pub enum RuntimeError {
    StackUnderflow,
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
    pub stack: Vec<Value>,
    pc: usize,
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
            stack: Vec::new(),
            pc: 0,
        }
    }

    pub fn run(&mut self, program: Program) -> Result<(), RuntimeError> {
        loop {
            let op = &program.code[self.pc];
            self.pc += 1;
            match op {
                Ops::Constant(index) => {
                    self.push(program.constants[*index]);
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
