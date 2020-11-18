/// This module defines the core literal values that the LAM can operate on.
///
/// It also defines operations on them, such as formatting for pretty printing,
/// equalities, and (de)serializations.
///
use num_bigint::BigInt;
use num_traits::cast::FromPrimitive;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

pub type Atom = String;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Pid {
    pub scheduler_id: u32,
    pub process_id: u64,
}

impl Display for Pid {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "<{:?}.{:?}.0>", self.scheduler_id, self.process_id)
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Tuple {
    pub size: u32,
    pub elements: Vec<Literal>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
/// NOTE(@ostera): consider reusing an existing implementation of cons lists
pub enum List {
    Nil,
    Cons(Box<Literal>, Box<Literal>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Literal {
    /// NOTE(@ostera): consider interning atoms so this becomes Atom(u32) instead
    Atom(Atom),

    /// NOTE(@ostera): consider interning binaries into a table so that this particular Literal is
    /// either Binary(InternedRef(u32) | ActualString(String))
    Binary(String),
    Bool(bool),
    Character(u8),
    Float(f64),
    Integer(BigInt),
    List(List),
    Pid(Pid),
    Tuple(Tuple),
    // Map(Map),
}

impl Into<BigInt> for Literal {
    fn into(self) -> BigInt {
        match self {
            Literal::Integer(bi) => bi,
            Literal::Float(f) => BigInt::from_f64(f).unwrap(),
            _ => panic!("Could not turn {:?} into a BigInt", self),
        }
    }
}
