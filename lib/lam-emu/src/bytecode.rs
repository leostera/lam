use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Pid {
    id: u64,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Tuple {
    size: u8,
    elements: Vec<Literal>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum List {
    Nil,
    Cons(Box<Literal>, Box<List>),
}

pub type Map = Vec<(Literal, Literal)>;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Literal {
    Atom(String),
    Binary(String),
    Bool(bool),
    Character(u8),
    Float(f64),
    Integer(BigInt),
    List(List),
    Map(Map),
    Pid(Pid),
    Tuple(Tuple),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Value {
    Register(Register),
    Literal(Literal),
    Nil,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Register {
    X(u8),
    Y(u8),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum FnCall {
    Local {
        function: String,
        arity: u8,
    },
    Qualified {
        module: String,
        function: String,
        arity: u8,
    },
}

impl FnCall {
    pub fn arity(&self) -> u8 {
        match self {
            FnCall::Local { arity, .. } => arity,
            FnCall::Qualified { arity, .. } => arity,
        }
        .clone()
    }

    pub fn module(&self) -> Option<String> {
        match self {
            FnCall::Local { .. } => None,
            FnCall::Qualified { module, .. } => Some(module.to_string()),
        }
    }

    pub fn function(&self) -> String {
        match self {
            FnCall::Local { function, .. } => function,
            FnCall::Qualified { function, .. } => function,
        }
        .clone()
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Instruction {
    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Kill-switch.
    ///
    Halt,

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Working with the Register Machine
    ///

    /** Move value or register value to a register */
    Move(Value, Register),

    /** Zero a register */
    Clear(Register),

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Working with the Heap
    ///

    /** Allocate */
    Allocate(u8),

    /** Deallocate */
    Deallocate(u8),

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Flow-control operations
    ///

    /** Define a new label.  */
    Label(u8),

    /** Jump to a label.  */
    Jump(u8),

    /** Returns control to the last Continuation Pointer.  */
    Return,

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Calling functions
    ///

    /** Perform a function call with or without allocating a new stack frame.  */
    Call(FnCall),
    TailCall(FnCall),

    /** Creates a new process and puts the Pid on the X(0) register */
    Spawn,
    Kill,
    Monitor,

    /** Puts the message (value in X(1)) into the mailbox of the pid in X(0) */
    Send,

    /** Puts the top of the */
    Receive,
}
