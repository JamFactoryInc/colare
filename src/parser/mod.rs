use core::fmt;
use std::{fmt::{Debug, Pointer}, rc::Rc};

use self::{identifiers::IdentifierManager, scope::ScopeManager};


pub mod identifiers;
pub mod scope;
pub mod lexer;

pub struct Parser<'src> {
    source: &'src str,
    ident_manager: IdentifierManager<'src>,
    scope_manaeger: ScopeManager,
}

#[derive(Clone, Copy, Debug)]
pub enum BracketType {
    Paren,
    Curly,
    Square
}

#[derive(Clone, Copy, Debug)]
pub enum LiteralType {
    Char,
    String,
    Int,
    Float,
    Hex,
    Binary,
    Bool,
}

#[macro_export]
macro_rules! illegal {
    ($($t:tt)*) => {
        Rc::new(move || { format!($($t)*) })
    };
}

#[derive(Clone, Debug)]
pub enum TokenType {
    Keyword,
    Literal(LiteralType),
    Operator(OpType),
    Identifier,
    Delimeter,
    DoubleString,
    SingleString,
    Open(BracketType),
    Close(BracketType),
    NewLine,
    TokenError(fn(&Token) -> String),
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
            _ => TokenType::TokenError(|t| format!("")),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OpType {
    Add,
    AddEq,
    Sub,
    SubEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    Mod,
    ModEq,
    And,
    BitAnd,
    BitAndEq,
    Or,
    BitOr,
    BitOrEq,
    XOr,
    XOrEq,
    Not,
    NotEq,
    Eq,
    DoubleEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Shr,
    Shl,
    Range,
    RangeEq,
    TypeAssign,
    Err
}

impl OpType {
    #[inline(always)]
    pub const fn from_single(c: char) -> OpType {
        match c {
            '+' => OpType::Add,
            '-' => OpType::Sub,
            '*' => OpType::Mul,
            '/' => OpType::Div,
            '%' => OpType::Mod,
            '&' => OpType::BitAnd,
            '|' => OpType::BitOr,
            '^' => OpType::XOr,
            '=' => OpType::Eq,
            '<' => OpType::Gt,
            '>' => OpType::Lt,
            ':' => OpType::TypeAssign,
            _ => OpType::Err
        }
    }

    #[inline(always)]
    pub const fn before_eq(op_type: OpType) -> OpType {
        match op_type {
            OpType::Add => OpType::AddEq,
            OpType::Sub => OpType::SubEq,
            OpType::Mul => OpType::MulEq,
            OpType::Div => OpType::DivEq,
            OpType::Mod => OpType::ModEq,
            OpType::BitAnd => OpType::BitAndEq,
            OpType::Or => OpType::BitOrEq,
            OpType::XOr => OpType::XOrEq,
            OpType::Not => OpType::NotEq,
            OpType::Eq => OpType::DoubleEq,
            _ => OpType::Err,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Span<'src> {
    contents: &'src str,
    line_number: u32,
    column_number: u32,
}

#[derive(Clone)]
pub struct Token<'src> {
    span: Span<'src>,
    token_type: TokenType
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

#[derive(Clone, Copy)]
pub enum BlockType {
    /// Surrounded by {}
    Block,
    /// Surrounded by () or a complete expression
    Expression,
    /// Surrounded by []
    Slice,
    /// Surrounded by <>
    Type,
}

#[derive(Clone)]
pub enum ParseTokenVariant<'src> {
    Directive(ParseDirectiveType),
    EndDirective(ParseDirectiveType),
    Leaf { leaf_type: ParseLeafType, token: Token<'src> },
    ParseError(ParseErrorVariant<'src>)
}

#[derive(Clone, Copy)]
pub enum ParseLeafType {
    IdentDeclaration,
    Ident,
    Type,
}

#[derive(Clone, Copy)]
pub enum ParseDirectiveType {
    /// <ident_decl> : <type?> = <expression>
    VariableDeclaration,
    /// <ident> = <expression>
    VariableAssignment,
    /// <vis?> fn <ident_decl>(<function_params?>) : <type>
    FunctionDeclaration,
    /// <<<ident> : <type>>*>
    FunctionParams,
    /// 
    TypeDeclaration,
    StartBlock(BlockType),
    EndBlock(BlockType),
}

#[derive(Clone, Copy)]
pub enum ParseErrorVariant<'src> {
    TokenError(Span<'src>),
    MessageProvider(fn(Span<'src>) -> String)
}