//! Module for type derivation

use super::IdentId;
use std::collections::HashMap;

struct Deductions {
    ls: Vec<Type>,
}

enum Type {
    Unspecified(Deductions),
    Int,
    Bool,
    Str,
    Func(Box<Type>, Vec<Type>),
}
