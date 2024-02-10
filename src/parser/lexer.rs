use std::rc::Rc;

use super::{LiteralType, OpType, Span, Token, TokenStream, TokenType};
use self::ParseStateType::*;


macro_rules! illegal {
    ($($t:tt)*) => {
        IllegalChar(Rc::new(move || { format!($($t)*) }))
    };
}

pub struct Lexer {

}

enum NumberParseStateType {
    Signed
}

#[derive(Default, Clone)]
enum ParseStateType {
    #[default]
    Initial,
    // we've seen a zero as the first n characters of a token.
    // this will either be used to determine a hex/bin literal or be removed
    LeadingZero,
    // we've seen a numeric digit as the first character of a token
    InNumber,
    // we've seen a decimal as the first character of a token
    // this is typically a float literal, but might be a lex error
    SeenDecimal,
    // we've seen the range operator ..
    // but it might be ..=
    SeenTwoDecimals,
    // we've seen a decimal following a number
    InHexLiteral,
    InBinaryLiteral,
    InFloat,
    InDoubleString,
    InDoubleStringEscaped,
    InSingleString,
    InSingleStringEscaped,
    InIdent,
    SeenOp(OpType),
    InSingleLineComment,
    ForwardSlashCommentOrDiv { prev_state: Box<ParseStateType> },
    IllegalChar(Rc<dyn Fn() -> String>),
}

impl Lexer {
    pub fn lex(src: &'_ str) -> TokenStream<'_> {
        let mut parse_state = ParseState::default();

        for (i, c) in src.chars().enumerate() {
            let is_new_line = (c == '\n') as usize | (c == ';') as usize;
            parse_state.line_number += is_new_line;
            parse_state.column_number += 1;
            parse_state.column_number *= is_new_line ^ 0b1;

            match parse_state.state_type {
                Initial => parse_state.handle_initial(c, i),
                InNumber => parse_state.handle_in_number(c, i),
                LeadingZero => parse_state.handle_leading_zero(c, i),
                SeenDecimal => parse_state.handle_seen_decimal(c, i),
                SeenTwoDecimals => parse_state.handle_seen_two_decimals(c, i),
                InHexLiteral => parse_state.handle_in_hex_literal(c, i),
                InBinaryLiteral => parse_state.handle_in_bin_literal(c, i),
                InFloat => parse_state.handle_in_float(c, i),
                InDoubleString => todo!(),
                InDoubleStringEscaped => todo!(),
                InSingleString => todo!(),
                InSingleStringEscaped => todo!(),
                InIdent => todo!(),
                IllegalChar(_) => todo!(),
                SeenOp(_) => todo!(),
                InSingleLineComment => todo!(),
                ForwardSlashCommentOrDiv { prev_state } => todo!(),
            }
        }

        todo!()
    }
}

#[derive(Default, Clone)]
struct ParseState<'src> {
    source: &'src str,
    state_type: ParseStateType,
    token_stream: TokenStream<'src>,
    line_number: usize,
    column_number: usize,
    token_start: usize, 
}

impl<'src> ParseState<'src> {

    fn finish_token(&mut self, end_index: usize, token_type: TokenType) -> &mut Self {
        let new_token = Token {
            span: Span {
                contents: &self.source[self.token_start..=end_index],
                line_number: self.line_number as u32,
                column_number: self.column_number as u32,
            },
            token_type,
        };
        self.token_start = end_index;
        self.token_stream.stream.push(new_token);
        self
    }
    
    fn start_token(&mut self, start_index: usize) -> &mut Self {
        self.token_start = start_index;
        self
    }

    fn state(&mut self, state: ParseStateType) -> &mut Self {
        self.state_type = state;
        self
    }

    fn handle_escaped_char(&mut self, c: char, i: usize, return_state: ParseStateType) {
        match c {
            'n' | '\\' | 'r' | 't' | 'f' => {
                self.state(return_state);
            },
            _ => {
                self.state(illegal!("Unknown escape sequence '\\{c}'"));
            }
        }
    }

    fn handle_end_literal(&mut self, c: char, i: usize, lit_type: LiteralType) {
        match c {
            ' ' | '\t'
                => {
                self.finish_token(i, TokenType::Literal(lit_type))
                    .state(Initial);
            },
            ';' | '\n' => {
                self.finish_token(i, TokenType::Literal(lit_type))
                    .start_token(i)
                    .finish_token(i, TokenType::NewLine);
            },
            '(' | ')'
                | '[' | ']'
                | '{' | '}'
                | ',' => {
                self.finish_token(i, TokenType::Literal(lit_type))
                    .start_token(i)
                    .finish_token(i, TokenType::from_single(c))
                    .state(Initial);
            },
            '/' => {
                let prev_state = Box::new(self.state_type.clone());
                self.finish_token(i, TokenType::Literal(lit_type))
                    .start_token(i)
                    .state(ForwardSlashCommentOrDiv { prev_state });
            }
            '+' | '-' | '*' | '%' | '&' | '|' | '^' | '=' | '<' | '>' => {
                self.finish_token(i, TokenType::Literal(lit_type))
                    .start_token(i)
                    .state(SeenOp(OpType::from_single(c)));
            },
            _ => {
                self.start_token(i)
                    .state(illegal!("Unexpected character '{c}'"));
            },
        }
    }

    fn handle_initial(&mut self, c: char, i: usize) {
        match c {
            ' ' | '\t' => {},
            '0' => {
                self.state(LeadingZero);
            },
            '1'..='9' => {
                self.state(InNumber)
                    .start_token(i);
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                self.state(InIdent)
                    .start_token(i);
            },
            '"' => {
                self.state(InDoubleString)
                    .start_token(i);
            },
            '\'' => {
                self.state(InSingleString)
                    .start_token(i);
            },
            '.' => {
                self.state(SeenDecimal)
                    .start_token(i);
            },
            _ => {
                self.state(illegal!("Unexpected character '{c}'"))
                    .start_token(i);
            },
        };
    }

    fn handle_in_number(&mut self, c: char, i: usize) {
        match c {
            '0'..='9' | '_' => {},
            '.' => {
                self.state(InFloat);
            }
            _ => {
                self.handle_end_literal(c, i, LiteralType::Int)
            },
        }
    }

    fn handle_leading_zero(&mut self, c: char, i: usize) {
        match c {
            '0' => {},
            'b' | 'B' => {
                self.start_token(i - 1)
                    .state(InBinaryLiteral);
            },
            'x' | 'X' => {
                self.start_token(i - 1)
                    .state(InHexLiteral);
            }
            '.' => {
                self.start_token(i - 1)
                    .state(InFloat);
            }
            '1'..='9' => {
                self.start_token(i)
                    .state(InNumber);
            }
            _ => {
                self.handle_end_literal(c, i, LiteralType::Int)
            }
        }
    }

    fn handle_seen_decimal(&mut self, c: char, _: usize) {
        match c {
            ' ' | '\t' => {},
            '.' => {
                self.state(SeenTwoDecimals);
            }
            '0'..='9' => {
                self.state(InFloat);
            }
            _ => {
                self.state(illegal!("Unexpected character '{c}'"));
            }
        }
    }

    fn handle_seen_two_decimals(&mut self, c: char, i: usize) {
        match c {
            '=' => {
                self.finish_token(i, TokenType::Operator(OpType::RangeInclusive))
                    .state(Initial);
            },
            _ => {
                self.finish_token(i, TokenType::Operator(OpType::Range));
                self.handle_initial(c, i);
            }
        }
    }

    fn handle_in_hex_literal(&mut self, c: char, i: usize) {
        match c {
            '0'..='9' | 'A'..='F' | 'a'..='f' | '_' => {},
            _ => {
                self.handle_end_literal(c, i, LiteralType::Hex)
            }
        }
    }

    fn handle_in_bin_literal(&mut self, c: char, i: usize) {
        match c {
            '0' | '1' | '_' => {},
            _ => {
                self.handle_end_literal(c, i, LiteralType::Binary)
            }
        }
    }

    fn handle_in_float(&mut self, c: char, i: usize) {
        match c {
            '0'..='9' | '_' => {},
            _ => {
                self.handle_end_literal(c, i, LiteralType::Int)
            },
        }
    }

    fn handle_in_double_string(&mut self, c: char, i: usize) {
        match c {
            '"' | '\n' => {
                self.finish_token(i, TokenType::DoubleString)
                    .state(Initial);
            },
            '\\' => {
                self.state(InDoubleStringEscaped);
            }
            _ => {},
        }
    }

    fn handle_in_double_string_escaped(&mut self, c: char, i: usize) {
        match c {
            '\n' => {
                self.finish_token(i, TokenType::DoubleString)
                    .state(illegal!("Nothing following start of escape sequence"));
            },
            '\\' => {
                self.state(InDoubleStringEscaped);
            }
            _ => {},
        }
    }
}