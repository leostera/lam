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
    pub main: MFA,
    pub modules: HashMap<String, Module>,
}

impl Default for Program {
    fn default() -> Program {
        Program {
            main: MFA {
                module: "main".to_string(),
                function: "main".to_string(),
                arity: 1,
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

    pub fn serialize(&self) -> Result<Vec<u8>, Error> {
        bincode::serialize(self).context("Could not serialize program")
    }
    pub fn deserialize(data: &[u8]) -> Result<Program, Error> {
        bincode::deserialize(data).context("Could not serialize program")
    }
}
