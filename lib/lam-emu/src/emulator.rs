use std::boxed::Box;
use std::collections::HashMap;

use super::program::{Instruction, Register, Value, MFA};
use super::runtime::Runtime;

#[derive(Debug, Default, Clone)]
pub struct Emulator {
    pub registers: HashMap<u8, Value>,
}

impl Emulator {
    pub fn execute(
        &mut self,
        instruction: &Instruction,
        runtime: &mut Box<dyn Runtime>,
    ) -> &mut Emulator {
        match instruction {
            Instruction::CallExtOnly { mfa, .. } => runtime.execute(mfa, self),
            Instruction::Move(value, Register::X(register)) => {
                self.registers.insert(
                    *register,
                    match value {
                        Value::Register(Register::X(reg)) => {
                            self.registers.get(&reg).unwrap().clone()
                        }
                        x => x.clone(),
                    },
                );
            }
            _ => (),
        }
        self
    }
}
