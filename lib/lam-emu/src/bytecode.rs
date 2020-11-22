use super::literal::*;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Value {
    Register(Register),
    Literal(Literal),
    Nil,
}

impl Into<Literal> for Value {
    fn into(self) -> Literal {
        match self {
            Value::Literal(l) => l.clone(),
            _ => panic!("Can not turn {:?} into a Literal", self),
        }
    }
}

impl Into<Value> for Literal {
    fn into(self) -> Value {
        Value::Literal(self)
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Value::Nil => write!(fmt, "nil"),
            Value::Register(r) => write!(fmt, "{}", r),
            Value::Literal(l) => write!(fmt, "{}", l),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Register {
    /// Global registers are available for all functions within a process, and
    /// they are used for passing arguments to other function calls, and for
    /// receiving return values.
    ///
    /// Calling `f(1)` is roughly translated to:
    ///
    /// ```lam
    /// Global(0) = 1
    /// Call(f)
    /// ```
    ///
    /// And after it runs, `Global(0)` will have the return value.
    ///
    Global(u32),

    /// Local registers are private to a function call, and are used to save
    /// intermediary values necessary to compute the final result.
    ///
    /// Between calls, any value from a Global register that needs to be used
    /// after the call, should be moved to a Local register first and restored
    /// after.
    ///
    /// For example, this code:
    ///
    /// ```erlang
    /// f(A) -> g(1) + A.
    /// ```
    ///
    /// Roughly translates to:
    ///
    /// ```lam
    /// Local(0) = Global(0)
    /// Global(0) = 1
    /// Call(g)
    /// Add(Global(0), Local(0))
    /// ```
    ///
    /// And the return value will be available at `Global(0)` (assuming that's
    /// where Add puts it).
    ///
    Local(u32),
}

impl Display for Register {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Register::Local(l) => write!(fmt, "L#{}", l),
            Register::Global(l) => write!(fmt, "G#{}", l),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum FnCall {
    BuiltIn {
        module: Atom,
        function: Atom,
        arity: Arity,
        arguments: Vec<Value>,
        destination: Register,
    },
    Local {
        module: Atom,
        label: Label,
        arity: Arity,
    },
    Qualified {
        module: Atom,
        function: Atom,
        arity: Arity,
    },
    ApplyLambda {
        arity: Arity,
        register: Register,
    },
}

impl FnCall {
    pub fn arity(&self) -> Arity {
        match self {
            FnCall::Local { arity, .. } => *arity,
            FnCall::Qualified { arity, .. } => *arity,
            FnCall::BuiltIn { arity, .. } => *arity,
            FnCall::ApplyLambda { .. } => panic!("Lambdas do not have an arity?"),
        }
    }

    pub fn module(&self) -> Option<String> {
        match self {
            FnCall::Local { module, .. } => Some(module.to_string()),
            FnCall::Qualified { module, .. } => Some(module.to_string()),
            FnCall::BuiltIn { module, .. } => Some(module.to_string()),
            FnCall::ApplyLambda { .. } => panic!("Lambdas do not have a module?"),
        }
    }

    pub fn function(&self) -> String {
        match self {
            FnCall::Qualified { function, .. } => function.clone(),
            FnCall::BuiltIn { function, .. } => function.clone(),
            FnCall::Local { .. } => panic!("Local calls don't have a name?"),
            FnCall::ApplyLambda { .. } => panic!("Lambdas do not have a name?"),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum FnKind {
    Native,
    User,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Test {
    Equals(Value, Value),
    IsGreaterOrEqualThan(Value, Value),
    IsNil(Value),
    IsNonEmptyList(Value),
    IsTaggedTuple { value: Value, size: u32, atom: Atom },
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Spawn {
    /// Spawn a new process by applying this lambda
    Lambda { register: Register },

    /// Spawn a new process starting with this MFA invocation
    MFA {
        module: Atom,
        function: Atom,
        arity: Arity,
        arguments: Vec<Value>,
    },
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Instruction {
    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Kill-switch.
    ///
    /// A Halt instruction will stop an emulator.
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
    Call(FnCall, FnKind),
    TailCall(FnCall, FnKind),

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Working with Values
    ///

    /// Create a boxed lambda and place it on `Global(0)`. When called with
    /// `Call(Lambda { register })` execution will start at `first_label`.
    MakeLambda {
        first_label: Label,
        module: Atom,
        arity: Arity,
        environment_size: u32,
    },

    /// Cons `head` onto `tail` and place it in the `target` register
    ConsList {
        head: Value,
        tail: Value,
        target: Register,
    },

    /// Deconstruct a list from `list` and place its head in register `head` and its tail in
    /// register `tail`
    SplitList {
        list: Register,
        head: Register,
        tail: Register,
    },

    /// Copy a tuple element onto a specific registry
    GetTupleElement {
        tuple: Register,
        element: u32,
        target: Register,
    },

    ///////////////////////////////////////////////////////////////////////////
    ///
    /// Processes
    ///

    /// Creates a new process looking for a lambda at Register, and puts the Pid on the X(0)
    /// register
    Spawn(Spawn),

    Kill,
    Monitor,

    /** Puts the message (value in X(1)) into the mailbox of the pid in X(0) */
    Send,

    /** Puts the top of the */
    Receive,

    /** Puts the identifier of the current process in a register */
    PidSelf(Register),
}
