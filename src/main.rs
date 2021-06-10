use std::env;

// MVP: Able to parse/execute "1+1"
// Lexer
// Compiler
// VM

// #[derive(Debug, Clone)]
// pub enum GrammarItem {
//     Product,
//     Sum,
//     Number(u64),
//     Paren,
// }

// #[derive(Debug, Clone)]
// pub struct ParseNode {
//     pub children: Vec<ParseNode>,
//     pub entry: GrammarItem,
// }

// impl ParseNode {
//     pub fn new() -> ParseNode {
//         ParseNode {
//             children: Vec::new(),
//             entry: GrammarItem::Paren,
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Op(char),
    Num(u64),
    Paren(char),
    EndOfFile, // Does this belong here or as an Err?
}

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

// pub fn parse(input: &String) -> Result<ParseNode, String> {
//     let tokens = nextToken(input)?;
//     println!("{:?}", tokens);
//     parse_expr(&tokens, 0).and_then(|(n, i)| {
//         if i == tokens.len() {
//             Ok(n)
//         } else {
//             Err(format!(
//                 "Expected end of input, found {:?} at {}",
//                 tokens[i], i
//             ))
//         }
//     })
// }

// fn parse_expr(tokens: &Vec<Token>, pos: usize) -> Result<(ParseNode, usize), String> {
//     let (node_summand, next_pos) = parse_summand(tokens, pos)?;
//     let c = tokens.get(next_pos);
//     match c {
//         Some(&Token::Op('+')) => {
//             // recurse on the expr
//             let mut sum = ParseNode::new();
//             sum.entry = GrammarItem::Sum;
//             sum.children.push(node_summand);
//             let (rhs, i) = parse_expr(tokens, next_pos + 1)?;
//             sum.children.push(rhs);
//             Ok((sum, i))
//         }
//         _ => {
//             // we have just the summand production, nothing more.
//             Ok((node_summand, next_pos))
//         }
//     }
// }

// fn parse_summand(tokens: &Vec<Token>, pos: usize) -> Result<(ParseNode, usize), String> {
//     let (node_term, next_pos) = parse_term(tokens, pos)?;
//     let c = tokens.get(next_pos);
//     match c {
//         Some(&Token::Op('*')) => {
//             // recurse on the summand
//             let mut product = ParseNode::new();
//             product.entry = GrammarItem::Product;
//             product.children.push(node_term);
//             let (rhs, i) = parse_summand(tokens, next_pos + 1)?;
//             product.children.push(rhs);
//             Ok((product, i))
//         }
//         _ => {
//             // we have just the term production, nothing more.
//             Ok((node_term, next_pos))
//         }
//     }
// }

// fn parse_term(tokens: &Vec<Token>, pos: usize) -> Result<(ParseNode, usize), String> {
//     let c: &Token = tokens.get(pos).ok_or(String::from(
//         "Unexpected end of input, expected paren or number",
//     ))?;
//     match c {
//         &Token::Num(n) => {
//             let mut node = ParseNode::new();
//             node.entry = GrammarItem::Number(n);
//             Ok((node, pos + 1))
//         }
//         &Token::Paren(c) => {
//             match c {
//                 '(' | '[' | '{' => {
//                     parse_expr(tokens, pos + 1).and_then(|(node, next_pos)| {
//                         if let Some(&Token::Paren(c2)) = tokens.get(next_pos) {
//                             if c2 == matching(c) {
//                                 // okay!
//                                 let mut paren = ParseNode::new();
//                                 paren.children.push(node);
//                                 Ok((paren, next_pos + 1))
//                             } else {
//                                 Err(format!(
//                                     "Expected {} but found {} at {}",
//                                     matching(c),
//                                     c2,
//                                     next_pos
//                                 ))
//                             }
//                         } else {
//                             Err(format!(
//                                 "Expected closing paren at {} but found {:?}",
//                                 next_pos,
//                                 tokens.get(next_pos)
//                             ))
//                         }
//                     })
//                 }
//                 _ => Err(format!("Expected paren at {} but found {:?}", pos, c)),
//             }
//         }
//         _ => Err(format!(
//             "Unexpected token {:?}, expected paren or number",
//             { c }
//         )),
//     }
// }

// fn matching(c: char) -> char {
//     match c {
//         ')' => '(',
//         ']' => '[',
//         '}' => '{',
//         '(' => ')',
//         '[' => ']',
//         '{' => '}',
//         _ => panic!("should have been a parenthesis!"),
//     }
// }

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

struct WrenFunction {}

enum OpCodes {
    End,
}

fn compile(input: &mut InputManager) -> WrenFunction {
    // Init the parser & compiler
    // Ignore newlines
    // While not EOF
    // build definitions.
    // End of module
    // Emit return?
    WrenFunction {}
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: `cargo run STATEMENT`");
        println!("Example: `cargo run \"1+1\"`");
    } else {
        let mut input = InputManager {
            source: "1 + 1".as_bytes().to_vec(),
            offset: 0,
        };
        // let token = next_token(&mut input);
        let tokens = lex(&mut input);
        println!("{:?}", tokens);
        // println!("Executing: {}", args[1]);
        // println!("{:?}", parse(&args[1])));
    }
}
