use anyhow::{Context, Error};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::iter::FromIterator;

use super::bytecode::Instruction;

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[repr(C)]
pub struct MFA {
    pub module: String,
    pub function: String,
    pub arity: u8,
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
                module: "hello_joe".to_string(),
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

    pub fn instructions(&self) -> std::slice::Iter<Instruction> {
        let module = self.modules.get(&self.main.module).unwrap();
        let first_label = module
            .functions
            .get(&(self.main.function.clone(), self.main.arity))
            .unwrap();
        module.labels[*first_label as usize].instructions.iter()
    }

    pub fn serialize(&self) -> Result<Vec<u8>, Error> {
        bincode::serialize(self).context("Could not serialize program")
    }
    pub fn deserialize(data: &[u8]) -> Result<Program, Error> {
        bincode::deserialize(data).context("Could not serialize program")
    }
}
