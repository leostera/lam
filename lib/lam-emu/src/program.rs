use anyhow::{Context, Error};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::iter::FromIterator;

use super::bytecode::*;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct MFA {
    pub module: String,
    pub function: String,
    pub arity: u8,
}

impl Into<MFA> for FnCall {
    fn into(self) -> MFA {
        match self {
            FnCall::Qualified {
                module,
                function,
                arity,
            } => MFA {
                module,
                function,
                arity,
            },
            _ => panic!("Can't turn local calls into MFAs"),
        }
    }
}

#[derive(Default, Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct FunctionLabel {
    pub id: u8,
    pub instructions: Vec<Instruction>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Module {
    pub name: String,
    pub functions: HashMap<(String, u8), u8>,
    pub labels: Vec<FunctionLabel>,
}

impl Default for Module {
    fn default() -> Module {
        Module {
            name: "".to_string(),
            functions: HashMap::default(),
            labels: vec![],
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Program {
    main: MFA,
    modules: HashMap<String, Module>,
}

impl Default for Program {
    fn default() -> Program {
        Program {
            main: MFA {
                module: "main".to_string(),
                function: "main".to_string(),
                arity: 0,
            },
            modules: HashMap::new(),
        }
    }
}

impl Program {
    pub fn with_modules(self, mods: Vec<Module>) -> Program {
        let modules = HashMap::from_iter(mods.iter().cloned().map(|m| (m.name.clone(), m)));
        Program { modules, ..self }
    }

    pub fn with_main(self, module: String, function: String) -> Program {
        Program {
            main: MFA {
                module,
                function,
                arity: 0,
            },
            ..self
        }
    }

    pub fn next(&self, instr_ptr: &mut InstructionPointer) {
        let module = self.modules.get(&instr_ptr.current_module).unwrap();

        let instructions = &module.labels[instr_ptr.current_label as usize].instructions;

        // let last_label = module.labels.len();
        let last_offset = instructions.len();

        if (instr_ptr.current_instruction + 1) < last_offset {
            instr_ptr.current_instruction += 1;
            instr_ptr.instr = instructions[instr_ptr.current_instruction as usize].clone()
        } else {
            instr_ptr.instr = Instruction::Halt
        }
    }

    pub fn jump(&self, instr_ptr: &mut InstructionPointer, call: &FnCall) {
        let last_ptr = instr_ptr.clone();

        let module_name = call
            .module()
            .unwrap_or_else(|| last_ptr.current_module.clone());
        let module = self
            .modules
            .get(&module_name)
            .unwrap_or_else(|| panic!("Could not find module: {:?}", &module_name));
        /* NOTE: labels are 1 indexed! */
        let function_key = (call.function(), call.arity());
        let first_label = module
            .functions
            .get(&function_key)
            .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key))
            - 1;
        let first_instruction = module.labels[first_label as usize].instructions[0].clone();

        *instr_ptr = InstructionPointer {
            last_instr_ptr: Some(Box::new(last_ptr)),
            current_module: module.name.clone(),
            current_label: first_label,
            current_instruction: 0,
            instr: first_instruction,
        }
    }

    pub fn first_instruction(&self) -> InstructionPointer {
        let module = self.modules.get(&self.main.module).unwrap();
        /* NOTE: labels are 1 indexed! */
        let first_label = module
            .functions
            .get(&(self.main.function.clone(), self.main.arity))
            .unwrap()
            - 1;
        let first_instruction = module.labels[first_label as usize].instructions[0].clone();

        InstructionPointer {
            last_instr_ptr: None,
            current_module: module.name.clone(),
            current_label: first_label,
            current_instruction: 0,
            instr: first_instruction,
        }
    }

    pub fn serialize(&self) -> Result<Vec<u8>, Error> {
        bincode::serialize(self).context("Could not serialize program")
    }
    pub fn deserialize(data: &[u8]) -> Result<Program, Error> {
        bincode::deserialize(data).context("Could not serialize program")
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct InstructionPointer {
    last_instr_ptr: Option<Box<InstructionPointer>>,
    current_module: String,
    current_label: u8,
    current_instruction: usize,
    pub instr: Instruction,
}
