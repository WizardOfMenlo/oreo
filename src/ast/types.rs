//! Module for type derivation

use super::IdentId;
use std::collections::HashMap;

enum Type {
    Unspecified,
    Int,
    Bool,
    Str,
    Func(Box<Type>, Vec<Type>),
}
