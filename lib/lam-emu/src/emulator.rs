use super::bytecode::*;
use super::program::*;
use super::runtime::Runtime;
use std::boxed::Box;

use log::{debug, trace};

use num_bigint::BigInt;

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
        debug!("Program: {:#?}", &self.program);
        debug!("==================================================");
        let mut instr_ptr = self.program.first_instruction();
        loop {
            match instr_ptr.instr.clone() {
                ////////////////////////////////////////////////////////////////
                //
                // Kill switch
                //
                Instruction::Halt => break,

                ////////////////////////////////////////////////////////////////
                //
                // Working with the Register Machine
                //
                Instruction::Move(value, Register::X(rx)) => {
                    self.registers[rx as usize] = value.clone();
                    self.program.next(&mut instr_ptr);
                }

                Instruction::Swap(Register::X(rx0), Register::X(rx1)) => {
                    self.registers.swap(rx0 as usize, rx1 as usize);
                    self.program.next(&mut instr_ptr);
                }

                Instruction::Clear(Register::X(rx)) => {
                    self.registers[rx as usize] = Value::Nil;
                    self.program.next(&mut instr_ptr);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Function calls
                //
                Instruction::Call(FnCall::BuiltIn {
                    module,
                    function,
                    arity,
                    arguments,
                    destination: Register::X(rx),
                }) => {
                    let args = self.fetch_values(&arguments);
                    let ret = self.runtime.execute(
                        &MFA {
                            module,
                            function,
                            arity,
                        },
                        &args,
                    );
                    self.registers[rx as usize] = Value::Literal(ret);
                    self.program.next(&mut instr_ptr);
                }

                Instruction::TailCall(call) => {
                    let arity = call.arity() as usize;
                    let args = self.fetch_values(&self.registers[0..arity]);
                    let ret = self.runtime.execute(&call.into(), &args);
                    for i in 0..arity {
                        self.registers[i] = Value::Nil;
                    }
                    self.registers[0] = Value::Literal(ret);
                    self.program.next(&mut instr_ptr);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Flow-control operations
                //
                Instruction::Jump(label) => {
                    self.program.jump_to_label(&mut instr_ptr, &label);
                }

                Instruction::Return => {
                    self.program.return_to_last_instr(&mut instr_ptr);
                }

                Instruction::Test(label, test) => {
                    if self.run_test(&test) {
                        self.program.next(&mut instr_ptr);
                    } else {
                        self.program.jump_to_label(&mut instr_ptr, &label);
                    }
                }

                ////////////////////////////////////////////////////////////////
                //
                //  Creating Values
                //
                Instruction::PutValue {
                    register: Register::X(rx),
                    value,
                } => {
                    self.registers[rx as usize] = Value::Literal(self.fetch_value(&value));
                    self.program.next(&mut instr_ptr);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Function calls
                //
                _ => self.program.next(&mut instr_ptr),
            }

            trace!("\nInstr => {:?}", instr_ptr.instr);
            trace!(
                "Registers: \n    0 => {:?}\n    1 => {:?}\n    2 => {:?}",
                self.registers[0],
                self.registers[1],
                self.registers[2]
            );
        }
    }

    pub fn run_test(&self, test: &Test) -> bool {
        match test {
            Test::IsGreaterOrEqualThan(a, b) => {
                let a: BigInt = self.fetch_value(a).into();
                let b: BigInt = self.fetch_value(b).into();
                a >= b
            }
        }
    }

    pub fn fetch_values(&self, vs: &[Value]) -> Vec<Literal> {
        vs.iter().map(|v| self.fetch_value(v)).collect()
    }

    pub fn fetch_value(&self, v: &Value) -> Literal {
        match v {
            Value::Literal(Literal::List(ls)) => Literal::List(self.make_list_concrete(ls)),
            Value::Literal(l) => l.clone(),
            Value::Register(Register::X(rx)) => self.fetch_value(&self.registers[*rx as usize]),
            _ => panic!("Could not fetch value: {:?}", v),
        }
    }

    pub fn make_list_concrete(&self, l: &List) -> List {
        match l {
            List::Nil => List::Nil,
            List::Cons(hd, tl) => List::Cons(
                Box::new(Value::Literal(self.fetch_value(&*hd))),
                Box::new(Value::Literal(self.fetch_value(&*tl))),
            ),
        }
    }
}
