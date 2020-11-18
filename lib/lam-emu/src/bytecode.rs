use super::literal::*;
use serde::{Deserialize, Serialize};

pub type Label = u32;
pub type Arity = u32;

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
    X(u32),
    Y(u32),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum FnCall {
    BuiltIn {
        module: String,
        function: String,
        arity: Arity,
        arguments: Vec<Value>,
        destination: Register,
    },
    Local {
        function: String,
        arity: Arity,
    },
    Qualified {
        module: String,
        function: String,
        arity: Arity,
    },
}

impl FnCall {
    pub fn arity(&self) -> Arity {
        match self {
            FnCall::Local { arity, .. } => *arity,
            FnCall::Qualified { arity, .. } => *arity,
            FnCall::BuiltIn { arity, .. } => *arity,
        }
    }

    pub fn module(&self) -> Option<String> {
        match self {
            FnCall::Local { .. } => None,
            FnCall::Qualified { module, .. } => Some(module.to_string()),
            FnCall::BuiltIn { module, .. } => Some(module.to_string()),
        }
    }

    pub fn function(&self) -> String {
        match self {
            FnCall::Local { function, .. } => function,
            FnCall::Qualified { function, .. } => function,
            FnCall::BuiltIn { function, .. } => function,
        }
        .clone()
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Test {
    Equals(Value, Value),
    IsGreaterOrEqualThan(Value, Value),
    IsNil(Value),
    IsNonEmptyList(Value),
    IsTaggedTuple {
        value: Value,
        element: u32,
        atom: Atom,
    },
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

    /** Swap the values of two registers */
    Swap(Register, Register),

    /** Zero a register */
    Clear(Register),

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Working with the Heap
    ///

    /** Allocate */
    Allocate {
        /** Amount of words to allocate on the heap */
        words: u8,
        /** how many registers to preserve */
        /** NOTE(@ostera): this is currently an artifact of how BEAM byteops
         * work. This should be split into 2 operations: allocate + clear_many */
        keep_registers: u8,
    },

    /** Deallocate */
    Deallocate {
        words: u8,
    },

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Flow-control operations
    ///

    /** Define a new label.  */
    Label(Label),

    /** Jump to a label.  */
    Jump(Label),

    /** Returns control to the last Continuation Pointer.  */
    Return,

    /** Perform a test and jump to label if it fails. */
    Test(Label, Test),

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Calling functions
    ///

    /// Perform a function call with or without allocating a new stack frame.
    Call(FnCall),
    TailCall(FnCall),

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Working with Values
    ///

    /// Cons `head` onto `tail` and place it in the `target` register
    ConsList {
        head: Register,
        tail: Register,
        target: Register,
    },

    /// Deconstruct a list from `list` and place its head in register `head` and its tail in
    /// register `tail`
    SplitList {
        list: Register,
        head: Register,
        tail: Register,
    },

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Processes
    ///

    /** Creates a new process and puts the Pid on the X(0) register */
    Spawn,
    Kill,
    Monitor,

    /** Puts the message (value in X(1)) into the mailbox of the pid in X(0) */
    Send,

    /** Puts the top of the */
    Receive,

    /** Puts the identifier of the current process in a register */
    PidSelf(Register),
}
