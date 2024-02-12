use super::{LiteralType, OpType, Span, Token, TokenStream, TokenType};
use self::ParseStateType::*;


pub struct Lexer;

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
    IllegalToken,
}

impl Lexer {
    pub fn lex(src: &'_ str) -> TokenStream<'_> {
        let mut parse_state = ParseState::from(src);

        for (i, c) in src.chars().enumerate() {
            let is_new_line = (c == '\n') as usize | (c == ';') as usize;
            parse_state.line_number += is_new_line;
            parse_state.column_number += 1;

            match parse_state.state_type {
                Initial => parse_state.handle_initial(c, i),
                InNumber => parse_state.handle_in_number(c, i),
                LeadingZero => parse_state.handle_leading_zero(c, i),
                SeenDecimal => parse_state.handle_seen_decimal(c, i),
                SeenTwoDecimals => parse_state.handle_seen_two_decimals(c, i),
                InHexLiteral => parse_state.handle_in_hex_literal(c, i),
                InBinaryLiteral => parse_state.handle_in_bin_literal(c, i),
                InFloat => parse_state.handle_in_float(c, i),
                InDoubleString => parse_state.handle_in_double_string(c, i),
                InDoubleStringEscaped => parse_state.handle_in_double_string_escaped(c, i),
                InSingleString => parse_state.handle_in_single_string(c, i),
                InSingleStringEscaped => parse_state.handle_in_single_string_escaped(c, i),
                InIdent => parse_state.handle_in_ident(c, i),
                SeenOp(op_type) => parse_state.handle_seen_op(c, i, op_type),
                InSingleLineComment => parse_state.handle_in_single_line_comment(c, i),
                IllegalToken => parse_state.handle_illegal_token(c, i),
            }

            parse_state.column_number *= is_new_line ^ 0b1;
        }

        parse_state.token_stream
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
    pub fn from(src: &'src str) -> ParseState {
        ParseState {
            source: src,
            .. Default::default()
        }
    }

    fn finish_token(&mut self, end_index: usize, token_type: TokenType, offset: isize) -> &mut Self {
        let new_token = Token {

            span: Span {
                contents: &self.source[self.token_start..=(end_index as isize + offset) as usize],
                line_number: self.line_number as u32,
                column_number: (self.column_number as isize + offset) as u32,
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
            'n' | '\\' | 'r' | 't' | 'f' | '"' | '\'' => {
                self.state(return_state);
            },
            _ => {
                self.state(IllegalToken);
            }
        }
    }

    fn handle_end_literal(&mut self, c: char, i: usize, lit_type: LiteralType) {
        match c {
            ' ' | '\t'
                => {
                self.finish_token(i, TokenType::Literal(lit_type), 0)
                    .state(Initial);
            },
            ';' | '\n' => {
                self.finish_token(i, TokenType::Literal(lit_type), -1)
                    .start_token(i)
                    .finish_token(i, TokenType::NewLine, 0);
            },
            '(' | ')'
                | '[' | ']'
                | '{' | '}'
                | ',' => {
                self.finish_token(i, TokenType::Literal(lit_type), 0)
                    .start_token(i)
                    .finish_token(i, TokenType::from_single(c), 0)
                    .state(Initial);
            },
            '#' => {
                self.finish_token(i, TokenType::Literal(lit_type), 0)
                    .start_token(i)
                    .state(InSingleLineComment);
            }
            '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '=' | '<' | '>' | ':' => {
                self.finish_token(i, TokenType::Literal(lit_type), 0)
                    .start_token(i)
                    .state(SeenOp(OpType::from_single(c)));
            },
            _ => {
                self.start_token(i)
                    .state(IllegalToken);
            },
        }
    }

    fn handle_initial(&mut self, c: char, i: usize) {
        match c {
            '\n' | '\r' | ';' => {
                self.start_token(i)
                    .finish_token(i, TokenType::NewLine, 0);
            }
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
            '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '!' | '=' | '<' | '>' | ':' => {
                self.state(SeenOp(OpType::from_single(c)))
                    .start_token(i);
            },
            '#' => {
                self.state(InSingleLineComment);
            }
            _ => {
                self.state(IllegalToken)
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
                self.state(Initial)
                .handle_end_literal(c, i, LiteralType::Int)
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
                self.state(Initial)
                    .handle_end_literal(c, i, LiteralType::Int)
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
                self.state(IllegalToken);
            }
        }
    }

    fn handle_seen_two_decimals(&mut self, c: char, i: usize) {
        match c {
            '=' => {
                self.finish_token(i, TokenType::Operator(OpType::RangeEq), 0)
                    .state(Initial);
            },
            _ => {
                self.finish_token(i, TokenType::Operator(OpType::Range), 0);
                self.handle_initial(c, i);
            }
        }
    }

    fn handle_in_hex_literal(&mut self, c: char, i: usize) {
        match c {
            '0'..='9' | 'A'..='F' | 'a'..='f' | '_' => {},
            _ => {
                self.state(Initial)
                    .handle_end_literal(c, i, LiteralType::Hex)
            }
        }
    }

    fn handle_in_bin_literal(&mut self, c: char, i: usize) {
        match c {
            '0' | '1' | '_' => {},
            _ => {
                self.state(Initial)
                    .handle_end_literal(c, i, LiteralType::Binary)
            }
        }
    }

    fn handle_in_float(&mut self, c: char, i: usize) {
        match c {
            '0'..='9' | '_' => {},
            _ => {
                self.state(Initial)
                    .handle_end_literal(c, i, LiteralType::Float)
            },
        }
    }

    fn handle_in_double_string(&mut self, c: char, i: usize) {
        match c {
            '"' => {
                self.finish_token(i, TokenType::DoubleString, 0)
                    .state(Initial);
            },
            '\\' => {
                self.state(InDoubleStringEscaped);
            }
            _ => {},
        }
    }

    fn handle_in_double_string_escaped(&mut self, c: char, i: usize) {
        self.handle_escaped_char(c, i, ParseStateType::InDoubleString);
    }

    fn handle_in_single_string(&mut self, c: char, i: usize) {
        match c {
            '\'' => {
                self.finish_token(i, TokenType::SingleString, 0)
                    .state(Initial);
            },
            '\\' => {
                self.state(InSingleStringEscaped);
            }
            _ => {},
        }
    }

    fn handle_in_single_string_escaped(&mut self, c: char, i: usize) {
        self.handle_escaped_char(c, i, ParseStateType::InSingleString);
    }

    fn handle_in_ident(&mut self, c: char, i: usize) {
        match c {
            '\n' | '\r' => {
                self.finish_token(i, TokenType::Identifier, -1)
                    .state(Initial)
                    .start_token(i).finish_token(i, TokenType::NewLine, 0);
            }
            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {},
            _ => {
                self.finish_token(i, TokenType::Identifier, -1)
                    .state(Initial)
                    .handle_initial(c, i);
            },
        }
    }

    fn handle_seen_op(&mut self, c: char, i: usize, op_type: OpType) {
        match (op_type, c) {
            (OpType::Add | OpType::Sub | OpType::Mul | OpType::Div | OpType::Mod
                | OpType::BitAnd | OpType::BitOr | OpType::XOr | OpType::Not
                | OpType::Eq | OpType::Lt | OpType::Gt,
                '=') => {
                self.finish_token(i, TokenType::Operator(OpType::before_eq(op_type)), 0)
                    .state(Initial);
            },
            (OpType::BitAnd, '&') => {
                self.finish_token(i, TokenType::Operator(OpType::And), 0)
                    .state(Initial);
            },
            (OpType::BitOr, '|') => {
                self.finish_token(i, TokenType::Operator(OpType::Or), 0)
                    .state(Initial);
            },
            (OpType::Lt, '<') => {
                self.finish_token(i, TokenType::Operator(OpType::Shl), 0)
                    .state(Initial);
            },
            (OpType::Gt, '>') => {
                self.finish_token(i, TokenType::Operator(OpType::Shr), 0)
                    .state(Initial);
            },
            (op_type, _) => {
                self.finish_token(i, TokenType::Operator(op_type), -1)
                    .state(Initial)
                    .handle_initial(c, i);
            },
        }
    }

    fn handle_in_single_line_comment(&mut self, c: char, i: usize) {
        match c {
            '\n' | '\r' => {
                self.state(Initial)
                    .start_token(i)
                    .finish_token(i, TokenType::NewLine, 0);
            },
            _ => {},
        }
    }

    fn handle_illegal_token(&mut self, c: char, i: usize) {
        let token_error = TokenType::TokenError(|t| {
            format!("Unexpected token '{}'", t.span.contents)
        });
        match c {
            '\n' | '\r' | ';' => {
                self.finish_token(i, token_error, -1)
                    .start_token(i)
                    .finish_token(i, TokenType::NewLine, 0);
            }
            ' ' | '\t' => {},
            '0' => {
                self.finish_token(i, token_error, -1)
                    .state(LeadingZero);
            },
            '1'..='9' => {
                self.finish_token(i, token_error, -1)
                    .state(InNumber)
                    .start_token(i);
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                self.finish_token(i, token_error, -1)
                    .state(InIdent)
                    .start_token(i);
            },
            '"' => {
                self.finish_token(i, token_error, -1)
                    .state(InDoubleString)
                    .start_token(i);
            },
            '\'' => {
                self.finish_token(i, token_error, -1)
                    .state(InSingleString)
                    .start_token(i);
            },
            '.' => {
                self.finish_token(i, token_error, -1)
                    .state(SeenDecimal)
                    .start_token(i);
            },
            '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '!' | '=' | '<' | '>' | ':' => {
                self.state(SeenOp(OpType::from_single(c)))
                    .start_token(i);
            },
            _ => {},
        }
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;

    #[test]
    fn test_lexer() {
        let src = r#"
        abc = 1
        # wow
        b123 = "a'bc\" 123"
        b_123 = 'a"bc\' 123'

        a = 0.123
        b = .123
        c = 100_000
        d = 0xFF
        e = 0b11

        abc += 1

        "#;
        let token_stream = Lexer::lex(src);

        println!("{token_stream:?}")
    }
}