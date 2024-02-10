use std::num::NonZeroU64;


pub struct ScopeManager {
    scopes: Vec<Scope>,
    scope_identifiers: Vec<Vec<NonZeroU64>>,
}

pub struct Scope {
    id: NonZeroU64,
    parent: Option<NonZeroU64>
}