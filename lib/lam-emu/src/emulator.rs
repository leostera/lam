use super::bytecode::*;
use super::program::*;
use super::runtime::Runtime;
use log::debug;
use std::boxed::Box;

#[repr(C)]
pub struct Emulator {
    program: Program,
    runtime: Box<dyn Runtime>,
    pub registers: Vec<Value>,
}

impl Emulator {
    pub fn new(program: Program, runtime: Box<dyn Runtime>) -> Emulator {
        Emulator {
            runtime,
            program,
            registers: [0; 32].to_vec().iter().map(|_| Value::Nil).collect(),
        }
    }

    pub fn run(&mut self) {
        debug!("Program: {:?}", &self.program);
        debug!("==================================================");
        let mut instr_ptr = self.program.first_instruction();
        loop {
            debug!("Instr => {:?}", instr_ptr.instr);
            match instr_ptr.instr.clone() {
                Instruction::Halt => break,
                Instruction::Move(value, Register::X(rx)) => {
                    self.registers[rx as usize] = value.clone();
                    self.program.next(&mut instr_ptr);
                }
                Instruction::Call(call) /* if is_built_in(call) */ => {
                    let arity = call.arity() as usize;
                    let args = self.registers[0..arity].to_vec();
                    let ret = self.runtime.execute(&call.into(), &args);
                    // self.program.jump(&mut instr_ptr, &call);
                    for i in 0..arity {
                        self.registers[i] = Value::Nil;
                    }
                    self.registers[0] = ret;
                    break;
                }
                _ => (),
            }
            debug!("Registers => {:?}", self.registers);
            debug!("");
        }
    }
}
