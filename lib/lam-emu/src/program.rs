use anyhow::{Context, Error};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Attribute {}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct MFA {
    pub module: String,
    pub function: String,
    pub arity: u8,
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
    /** Perform an allocation of some sort
        NOTE(@ostera): define what these u8's are in a struct
    */
    Allocate(u8, u8),

    /** Define module attributes */
    Attributes(Vec<Attribute>),

    /** Call external function */
    CallExtOnly { arity: u8, mfa: MFA },

    /** Local module call (jump to label) */
    CallOnly { arity: u8, label: u8 },

    /** Define module exports */
    Exports(Vec<Export>),

    /** Mark the beginning of a function */
    Function {
        name: String,
        first_label: u8,
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

    /** Cons a list onto a register */
    PutList {
        head: Value,
        tail: Value,
        reg: Register,
    },

    /** Perform a test on the heap,
        NOTE(@ostera): define what these u8's are in a struct
    */
    TestHeap(u8, u8),
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct FunctionLabel {
    id: u8,
    instructions: Vec<Instruction>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Module {
    name: String,
    funs: HashMap<String, u8>,
    labels: HashMap<u8, FunctionLabel>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Program {
    main: MFA,
    mods: HashMap<String, Module>,
}

impl Default for Program {
    fn default() -> Program {
        Program {
            main: MFA {
                module: "hello_joe".to_string(),
                function: "main".to_string(),
                arity: 0,
            },
            mods: HashMap::new(),
        }
    }
}

impl Program {
    pub fn from_instructions(instructions: Vec<Instruction>) -> Program {
        let mut program = Program::default();
        let mut current_module_name = String::new();
        let mut current_module_funs: HashMap<String, u8> = HashMap::new();
        let mut current_module_labels: HashMap<u8, FunctionLabel> = HashMap::new();
        let mut current_label_id: u8 = 0;
        let mut current_label_instructions = Vec::with_capacity(1024);
        for i in instructions {
            match i {
                Instruction::Module { name } => {
                    if !current_module_name.is_empty() {
                        program.mods.insert(
                            current_module_name.clone(),
                            Module {
                                name: current_module_name.clone(),
                                funs: current_module_funs.clone(),
                                labels: current_module_labels.clone(),
                            },
                        );
                        current_module_funs = HashMap::new();
                        current_module_labels = HashMap::new();
                    }
                    current_module_name = name;
                }
                Instruction::Function {
                    name, first_label, ..
                } => {
                    if current_label_id > 0 {
                        current_module_labels.insert(
                            current_label_id,
                            FunctionLabel {
                                id: current_label_id,
                                instructions: current_label_instructions.clone(),
                            },
                        );
                        current_label_instructions = Vec::with_capacity(1024);
                    }
                    current_module_funs.insert(name.clone(), first_label);
                }
                Instruction::Label { id: 0 } => (),
                Instruction::Label { id } => {
                    current_label_id = id;
                }
                Instruction::Labels { total } => {
                    current_module_labels.reserve(total as usize);
                }

                /* Actual function instructions to be pushed to the current_label vector */
                instr @ Instruction::CallExtOnly { .. } => {
                    current_label_instructions.push(instr);
                }
                instr @ Instruction::Move(_, _) => {
                    current_label_instructions.push(instr);
                }
                /* temporarily ignored */
                _ => (),
            }
        }

        // insert last labels
        if current_label_instructions.len() > 0 {
            current_module_labels.insert(
                current_label_id,
                FunctionLabel {
                    id: current_label_id,
                    instructions: current_label_instructions.clone(),
                },
            );
        }

        // insert last modules
        if current_module_name != "" {
            program.mods.insert(
                current_module_name.clone(),
                Module {
                    name: current_module_name.clone(),
                    funs: current_module_funs.clone(),
                    labels: current_module_labels.clone(),
                },
            );
        };

        program
    }

    pub fn instructions(&self) -> std::slice::Iter<Instruction> {
        let module = self.mods.get(&self.main.module).unwrap();
        let first_label = module.funs.get(&self.main.function).unwrap();
        module.labels[first_label].instructions.iter()
    }

    pub fn serialize(&self) -> Result<Vec<u8>, Error> {
        bincode::serialize(self).context("Could not serialize program")
    }
    pub fn deserialize(data: &[u8]) -> Result<Program, Error> {
        bincode::deserialize(data).context("Could not serialize program")
    }
}

pub fn sample() -> Program {
    let instructions = vec![
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
            first_label: 2,
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
    ];
    Program::from_instructions(instructions)
}
