use std::boxed::Box;

use super::bytecode::{Instruction, Value};
use super::runtime::Runtime;

#[derive(Debug, Default, Clone)]
#[repr(C)]
pub struct Emulator {
    pub registers: Vec<Value>,
}

impl Emulator {
    pub fn execute(
        &mut self,
        instruction: &Instruction,
        _runtime: &mut Box<dyn Runtime>,
    ) -> &mut Emulator {
        match instruction {
            _ => (),
        }
        self
    }
}
