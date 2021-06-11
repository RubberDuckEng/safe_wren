use std::env;

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
    Paren(char),
    EndOfFile, // Does this belong here or as an Err?
}

// Is this really the tokenizer?
// Should have an input stream which it can pull (and cache) from
// And then the ability to pull more when needed and look-ahead when needed.
// Keeps track of when the current token starts, and knows how to start a new.
struct InputManager {
    source: Vec<u8>,
    offset: usize,
}

impl InputManager {
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
enum LexError {
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
            b'(' | b')' => {
                return Ok(Token::Paren(c.into()));
            }
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

struct Expression {}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Assignment,
    // Conditional,
    // Sum,
    // Product,
    // Exponent,
    // Prefix,
    // Postfix,
    // Call,
}

// Takes an InputManager.  Knows how to use a Tokenizer to break it up into
// tokens one at a time.  Turns a stream of tokens into a tree of objects.
// Module / Function / Closure are likely the eventual objects?
struct Parser {
    input: InputManager,
}

// Following the Pratt parser example:
// https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type PrefixParslet = fn(parser: &Parser, token: &Token) -> Expression;
type InfixParslet = fn(parser: &Parser, left: Expression, token: &Token) -> Expression;

// Parslets belong in some sort of grouped parslet object per token?
impl Token {
    fn prefix_parslet(&self) -> PrefixParslet {
        |_, _| Expression {}
    }

    fn infix_parselet(&self) -> Option<InfixParslet> {
        Some(|_, _, _| Expression {})
    }

    fn infix_precedence(&self) -> Precedence {
        Precedence::Assignment
    }
}

impl Parser {
    fn consume(&self) -> Token {
        Token::EndOfFile
    }

    fn look_ahead(&self, offset: u8) -> Token {
        Token::EndOfFile
    }

    fn next_precedence(&self) -> Precedence {
        // Grabs the next token to be consumed
        // Checks its precedence if it has one and returns that
        // Otherwise returns lowest precedence?
        let current = self.look_ahead(0);
        match current.infix_parselet() {
            Some(parser) => current.infix_precedence(),
            None => Precedence::Lowest,
        }
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, LexError> {
        let prefix_token = self.consume();
        let prefix_parser = prefix_token.prefix_parslet();
        let mut left = prefix_parser(self, &prefix_token);
        while precedence < self.next_precedence() {
            let token = next_token(&mut self.input)?;
            let infix_parser = token.infix_parselet().expect("Invalid token");
            left = infix_parser(self, left, &token);
        }
        Ok(left)
    }
}

fn compile(input: InputManager) -> Result<WrenFunction, LexError> {
    // Init the parser & compiler
    // let mut compiler = WrenCompiler { input };
    let mut parser = Parser { input };

    // Ignore newlines
    loop {
        let token = next_token(&mut parser.input)?;
        if token == Token::EndOfFile {
            break;
        }
    }
    // While not EOF
    let expression = parser.parse_expression(Precedence::Lowest);

    // build definitions.
    // End of module
    // Emit return?
    Ok(WrenFunction {})
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: `cargo run STATEMENT`");
        println!("Example: `cargo run \"1+1\"`");
    } else {
        let input = InputManager {
            source: "1 + 1".as_bytes().to_vec(),
            offset: 0,
        };
        // let token = next_token(&mut input);
        // let tokens = lex(&mut input);
        let object = compile(input);
        println!("{:?}", object);
        // println!("Executing: {}", args[1]);
        // println!("{:?}", parse(&args[1])));
    }
}
