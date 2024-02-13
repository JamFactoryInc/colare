use self::{identifiers::IdentifierManager, op::OpType, scope::ScopeManager, token::{Span, Token}};


pub mod identifiers;
pub mod scope;
pub mod lexer;
pub mod op;
pub mod token;
pub mod keyword;

pub struct Parser<'src> {
    source: &'src str,
    ident_manager: IdentifierManager<'src>,
    scope_manaeger: ScopeManager,
}

#[macro_export]
macro_rules! illegal {
    ($($t:tt)*) => {
        Rc::new(move || { format!($($t)*) })
    };
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