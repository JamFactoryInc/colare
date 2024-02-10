use std::num::NonZeroU64;

pub enum IdentifierType {
    Variable,
    Type,
    GenericType,
}

pub struct IdentifierManager<'src> {
    identifiers: Vec<Identifier<'src>>
}

pub struct Identifier<'src> {
    identifier: &'src str,
    id: NonZeroU64,
    ident_type: IdentifierType
}