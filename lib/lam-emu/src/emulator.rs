use super::bytecode::*;
use super::instr_ptr::*;
use super::literal::*;
use super::program::*;
use super::registers::*;
use super::runtime::Runtime;
use super::scheduler::*;
use anyhow::Error;
use std::boxed::Box;

use log::*;

use num_bigint::BigInt;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Emulator {
    registers: Registers,
    instr_ptr: InstructionPointer,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub enum EmulationStatus {
    Continue,
    Terminated,
}

impl Emulator {
    pub fn new(mfa: &MFA, program: &Program) -> Emulator {
        Emulator {
            registers: Registers::new(),
            instr_ptr: InstructionPointer::new(mfa, program),
        }
    }

    pub fn preload(&mut self, x: u32, v: Value) -> &mut Emulator {
        self.registers.put(&Register::Global(x), v);
        self
    }

    pub fn run(
        &mut self,
        reduction_count: u64,
        program: &Program,
        _scheduler: &mut Scheduler,
        runtime: &mut Box<dyn Runtime>,
        pid: Pid,
    ) -> Result<EmulationStatus, Error> {
        let mut reductions = 0;
        while reductions < reduction_count {
            trace!("Reductions: {}", reductions);
            trace!("Registers: {}", self.registers,);
            trace!("Instr => {:?}", self.instr_ptr.instr);
            match self.instr_ptr.instr.clone() {
                ////////////////////////////////////////////////////////////////
                //
                // Kill switch
                //
                Instruction::Halt => runtime.halt(),

                ////////////////////////////////////////////////////////////////
                //
                // Working with the Register Machine
                //
                Instruction::Move(value, target) => {
                    match value {
                        Value::Register(reg) => {
                            self.registers.copy(&reg, &target);
                        }
                        v => {
                            self.registers.put(&target, v.clone());
                        }
                    }
                    self.instr_ptr.next(&program);
                }

                Instruction::Swap(a, b) => {
                    self.registers.swap(a, b);
                    self.instr_ptr.next(&program);
                }

                Instruction::Clear(reg) => {
                    self.registers.clear(&reg);
                    self.instr_ptr.next(&program);
                }

                Instruction::Deallocate { .. } | Instruction::Allocate { .. } => {
                    self.instr_ptr.next(&program);
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
                    destination,
                }) => {
                    let args: Vec<Literal> = arguments
                        .iter()
                        .map(|a| self.registers.get_literal_from_value(&a))
                        .collect();
                    let ret = runtime.execute(
                        &MFA {
                            module,
                            function,
                            arity,
                        },
                        &args,
                    );
                    self.registers.put(&destination, ret.into());
                    self.instr_ptr.next(&program);
                }

                Instruction::Call(FnCall::ApplyLambda { register }) => {
                    if let Literal::Lambda(lambda) = self.registers.get(&register).into() {
                        self.registers.fill_globals(&lambda.environment);
                        self.instr_ptr.jump_to_label(&program, &lambda.first_label);
                    } else {
                        panic!(
                            "Can not apply value found in {:?} since it is not a lambda",
                            register
                        )
                    };
                }

                Instruction::Call(call) => {
                    self.registers.clear_globals(call.arity());
                    self.instr_ptr.call(&program, &call);
                }

                Instruction::TailCall(call) => {
                    let arity = call.arity();
                    let args = self.registers.get_many_literals_from_global_range(0, arity);
                    let ret = runtime.execute(&call.into(), &args);
                    self.registers.clear_globals(arity);
                    self.registers.put_global(0, Value::Literal(ret));
                    self.instr_ptr.next(&program);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Flow-control operations
                //
                Instruction::Jump(label) => {
                    trace!("Jump to label {}", label);
                    self.instr_ptr.jump_to_label(&program, &label);
                }

                Instruction::Return => {
                    self.instr_ptr.return_to_last_instr();
                }

                Instruction::Test(label, test) => {
                    if self.run_test(&test) {
                        trace!("Test passed! continuing...");
                        self.instr_ptr.next(&program);
                    } else {
                        trace!("Test failed! Jumping to: {}", label);
                        self.instr_ptr.jump_to_label(&program, &label);
                    }
                }

                ////////////////////////////////////////////////////////////////
                //
                //  Creating Values
                //
                Instruction::ConsList { target, head, tail } => {
                    let head = Box::new(self.registers.get_literal_from_value(&head));
                    let tail = {
                        let lit = self.registers.get_literal_from_value(&tail);
                        match lit {
                            Literal::List(t) => Box::new(t),
                            _ => panic!(
                                "Can not allow consing improper list with {} and {}",
                                head, lit
                            ),
                        }
                    };
                    self.registers
                        .put(&target, Literal::List(List::Cons(head, tail)).into());
                    self.instr_ptr.next(&program);
                }

                Instruction::SplitList { list, head, tail } => {
                    match self.registers.get(&list).into() {
                        Literal::List(List::Cons(boxed_head, boxed_tail)) => {
                            self.registers
                                .put(&head, Value::Literal(*boxed_head.clone()));
                            self.registers
                                .put(&tail, Literal::List(*boxed_tail.clone()).into());
                        }
                        value => panic!("Attempted to split a value that is not a list: {}", value),
                    };
                    self.instr_ptr.next(&program);
                }

                Instruction::GetTupleElement {
                    tuple,
                    element,
                    target,
                } => {
                    match self.registers.get(&tuple) {
                        Value::Literal(Literal::Tuple(Tuple { elements, .. })) => {
                            let element = elements[element as usize].clone();
                            self.registers.put(&target, Value::Literal(element));
                        }
                        v => panic!(
                            "Attempted to extract element from value that is not a tuple: {}",
                            v
                        ),
                    }
                    self.instr_ptr.next(&program);
                }

                Instruction::MakeLambda {
                    module,
                    first_label,
                    arity,
                } => {
                    let environment = self.registers.get_many_literals_from_global_range(0, arity);
                    self.registers.put(
                        &Register::Global(0),
                        Value::Literal(Literal::Lambda(Lambda {
                            first_label,
                            module,
                            environment,
                            arity,
                        })),
                    );
                    self.instr_ptr.next(&program);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Processes
                //
                Instruction::Kill => return Ok(EmulationStatus::Terminated),

                Instruction::PidSelf(target) => {
                    self.registers
                        .put(&target, Literal::Pid(pid.clone()).into());
                    self.instr_ptr.next(&program);
                }

                Instruction::Spawn
                | Instruction::Monitor
                | Instruction::Send
                | Instruction::Receive => {
                    self.instr_ptr.next(&program);
                }

                Instruction::Label(_) => self.instr_ptr.next(&program),
            }
            reductions += 1;
        }
        Ok(EmulationStatus::Continue)
    }

    pub fn run_test(&self, test: &Test) -> bool {
        trace!("Running test: {:?}", test);
        match test {
            Test::IsGreaterOrEqualThan(a, b) => {
                let a: BigInt = self.registers.get_literal_from_value(a).into();
                let b: BigInt = self.registers.get_literal_from_value(b).into();
                a >= b
            }

            Test::IsNil(a) => match self.registers.get_literal_from_value(a) {
                Literal::List(List::Nil) => true,
                Literal::List(_) => false,
                _ => panic!("Can not check if non list value {} is an empty list", a),
            },

            Test::IsNonEmptyList(a) => match self.registers.get_literal_from_value(a) {
                Literal::List(List::Cons(_, _)) => true,
                Literal::List(List::Nil) => false,
                _ => panic!("Can not check if non list value {} is a non empty list", a),
            },

            Test::Equals(a, b) => {
                self.registers.get_literal_from_value(a) == self.registers.get_literal_from_value(b)
            }

            Test::IsTaggedTuple {
                value,
                element,
                atom,
            } => match self.registers.get_literal_from_value(value) {
                Literal::Tuple(Tuple { elements, .. }) => match &elements[*element as usize] {
                    Literal::Atom(tag) => tag == atom,
                    _ => false,
                },
                x => panic!("Cannot check if value {} is a tagged tuple", x),
            },
        }
    }
}
