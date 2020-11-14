use anyhow::{Context, Error};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::bytecode::Instruction;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct MFA {
    pub module: String,
    pub function: String,
    pub arity: u8,
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
    /*
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
                Instruction::Label ( 0 ) => (),
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
        if !current_label_instructions.is_empty() {
            current_module_labels.insert(
                current_label_id,
                FunctionLabel {
                    id: current_label_id,
                    instructions: current_label_instructions,
                },
            );
        }

        // insert last modules
        if current_module_name != "" {
            program.mods.insert(
                current_module_name.clone(),
                Module {
                    name: current_module_name,
                    funs: current_module_funs,
                    labels: current_module_labels,
                },
            );
        };

        program
    }
    */

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
