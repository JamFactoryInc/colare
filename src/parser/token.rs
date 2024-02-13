use std::{fmt::Debug, ops::{Deref, DerefMut}};
use super::{keyword::Keyword, op::OpType};


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BracketType {
    Paren,
    Curly,
    Square
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralType {
    Char,
    String,
    Int,
    Float,
    Hex,
    Binary,
    Bool,
    DoubleString,
    SingleString,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenErrorType {
    UnexpectedEndOfInput,
    UnknownEscapeCharacter,
    UnexpectedToken,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    Keyword(Keyword),
    Literal(LiteralType),
    Operator(OpType),
    Identifier,
    Delimeter,
    Open(BracketType),
    Close(BracketType),
    NewLine,
    TokenError(TokenErrorType),
}

impl TokenType {
    #[inline(always)]
    pub fn from_single(c: char) -> TokenType {
        match c {
            '(' => TokenType::Open(BracketType::Paren),
            ')' => TokenType::Close(BracketType::Paren),
            '[' => TokenType::Open(BracketType::Square),
            ']' => TokenType::Close(BracketType::Square),
            '{' => TokenType::Open(BracketType::Curly),
            '}' => TokenType::Close(BracketType::Curly),
            '\n' => TokenType::NewLine,
            ',' => TokenType::Delimeter,
            _ => TokenType::TokenError(TokenErrorType::UnexpectedToken),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Span<'src> {
    contents: &'src str,
    line_number: u32,
    column_number: u32,
}

impl<'src> Span<'src> {
    pub fn new(contents: &str, line_number: u32, column_number: u32) -> Span {
        Span {
            contents,
            line_number,
            column_number
        }
    }

    pub fn contents(&self) -> &str {
        self.contents
    }
}

#[derive(Clone)]
pub struct Token<'src> {
    span: Span<'src>,
    token_type: TokenType
}

impl<'src> Token<'src> {
    pub fn new(span: Span<'src>, token_type: TokenType) -> Token {
        Token {
            span,
            token_type,
        }
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type
    }
}

impl<'src> Deref for Token<'src> {
    type Target = Span<'src>;

    fn deref(&self) -> &Self::Target {
        &self.span
    }
}

impl<'src> Debug for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({}:{} {:?} {:?}) ",
            self.span.line_number,
            self.span.column_number,
            self.token_type,
            self.span.contents
        ))
    }
}

#[derive(Default, Clone)]
pub struct TokenStream<'src> {
    stream: Vec<Token<'src>>
}

impl<'src> Debug for TokenStream<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in self.stream.iter() {
            f.write_fmt(format_args!("{token:?}"))?
        }
        Ok(())
    }
}

impl<'src> Deref for TokenStream<'src> {
    type Target = Vec<Token<'src>>;

    fn deref(&self) -> &Self::Target {
        &self.stream
    }
}

impl<'src> DerefMut for TokenStream<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stream
    }
}