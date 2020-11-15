use anyhow::{Context, Error};
use log::trace;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::iter::FromIterator;

use super::bytecode::*;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct MFA {
    pub module: String,
    pub function: String,
    pub arity: u32,
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
    pub id: Label,
    pub instructions: Vec<Instruction>,
}

impl FunctionLabel {
    pub fn new(id: Label) -> FunctionLabel {
        FunctionLabel {
            id,
            ..FunctionLabel::default()
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct Module {
    pub name: String,
    pub functions: HashMap<(String, Arity), Label>,
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
                arity: 1,
            },
            ..self
        }
    }

    pub fn next(&self, instr_ptr: &mut InstructionPointer) {
        let module = self.modules.get(&instr_ptr.current_module).unwrap();

        let label = (instr_ptr.current_label - 1) as usize;
        let instructions = &module.labels[label].instructions;

        // let last_label = module.labels.len();
        let last_offset = instructions.len();

        let next_instr = instr_ptr.current_instruction + 1;
        if next_instr < last_offset {
            instr_ptr.current_instruction += 1;
            instr_ptr.instr = instructions[instr_ptr.current_instruction as usize].clone()
        } else if let Some(last_ptr) = &instr_ptr.last_instr_ptr {
            *instr_ptr = (**last_ptr).clone()
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
            .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key));
        let first_instruction = module.labels[(first_label - 1) as usize].instructions[0].clone();

        *instr_ptr = InstructionPointer {
            last_instr_ptr: Some(Box::new(last_ptr)),
            current_module: module.name.clone(),
            current_label: *first_label,
            current_instruction: 0,
            instr: first_instruction,
        }
    }

    pub fn jump_to_label(&self, instr_ptr: &mut InstructionPointer, label: &Label) {
        trace!("Jumping to label: {:?}", label - 1);

        let last_ptr = instr_ptr.clone();

        let module_name = last_ptr.current_module.clone();
        let module = self
            .modules
            .get(&module_name)
            .unwrap_or_else(|| panic!("Could not find module: {:?}", &module_name));
        trace!("Found module: {:?}", module_name);

        let first_instruction = module.labels[(label - 1) as usize].instructions[0].clone();
        trace!("First instruction: {:?}", first_instruction);

        *instr_ptr = InstructionPointer {
            current_module: last_ptr.current_module.clone(),
            current_label: *label,
            current_instruction: 0,
            instr: first_instruction,
            last_instr_ptr: None,
        }
    }

    pub fn return_to_last_instr(&self, instr_ptr: &mut InstructionPointer) {
        let last_ptr = instr_ptr.last_instr_ptr.clone().unwrap();
        *instr_ptr = *last_ptr;
    }

    pub fn first_instruction(&self) -> InstructionPointer {
        let module = self.modules.get(&self.main.module).unwrap();
        /* NOTE: labels are 1 indexed! */
        let function_key = (self.main.function.clone(), self.main.arity);
        let first_label = module
            .functions
            .get(&function_key)
            .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key));
        let first_instruction = module.labels[(first_label - 1) as usize].instructions[0].clone();

        InstructionPointer {
            last_instr_ptr: None,
            current_module: module.name.clone(),
            current_label: *first_label,
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
    current_label: Label,
    current_instruction: usize,
    pub instr: Instruction,
}
