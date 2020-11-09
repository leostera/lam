use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Attribute {}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct MFA {
    module: String,
    function: String,
    arity: u8,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Export {
    function: String,
    arity: u8,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Location {
    file_name: String,
    line_number: u32,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Literal {
    Atom(String),
    Binary(String),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Value {
    Register(Register),
    Nil,
    Literal(Literal),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Register {
    X(u8),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub enum Instruction {
    /** Define module attributes */
    Attributes(Vec<Attribute>),

    /** Call external function */
    CallExtOnly { arity: u8, mfa: MFA },

    /** Define module exports */
    Exports(Vec<Export>),

    /** Mark the beginning of a function */
    Function {
        name: String,
        label_count: u32,
        arity: u8,
    },

    /** Add information about a function */
    FunctionInfo(MFA),

    /** Declare a label */
    Label { id: u8 },

    /** Declare the total number of labels */
    Labels { total: u32 },

    /** Add file information for debugging purposes */
    Line(Vec<Location>),

    /** Begin a module of name [name] */
    Module { name: String },

    /** Move value or register value to a register */
    Move(Value, Register),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Default for Program {
    fn default() -> Program {
        Program {
            instructions: vec![],
        }
    }
}

pub fn sample() -> Program {
    Program {
        instructions: vec![
            Instruction::Module {
                name: "hello_joe".to_string(),
            },
            Instruction::Exports(vec![Export {
                function: "main".to_string(),
                arity: 0,
            }]),
            Instruction::Attributes(vec![]),
            Instruction::Labels { total: 7 },
            Instruction::Function {
                name: "main".to_string(),
                arity: 0,
                label_count: 2,
            },
            Instruction::Label { id: 1 },
            Instruction::Line(vec![]),
            Instruction::FunctionInfo(MFA {
                module: "hello_joe".to_string(),
                function: "main".to_string(),
                arity: 0,
            }),
            Instruction::Label { id: 2 },
            Instruction::Move(Value::Nil, Register::X(1)),
            Instruction::Move(
                Value::Literal(Literal::Binary("Hello, Joe!".to_string())),
                Register::X(0),
            ),
            Instruction::Line(vec![Location {
                file_name: "hello_joe.erl".to_string(),
                line_number: 7,
            }]),
            Instruction::CallExtOnly {
                arity: 2,
                mfa: MFA {
                    module: "io".to_string(),
                    function: "format".to_string(),
                    arity: 2,
                },
            },
        ],
    }
}
