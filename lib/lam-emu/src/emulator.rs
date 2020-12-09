use super::bytecode::*;
use super::instr_ptr::*;
use super::literal::*;
use super::mailbox::*;
use super::program::*;
use super::registers::*;
use super::runtime::Runtime;
use super::scheduler::*;
use anyhow::Error;
use log::*;
use num_bigint::BigInt;
use std::boxed::Box;
use std::cell::RefCell;

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct Emulator {
    registers: RefCell<Registers>,
    instr_ptr: RefCell<InstructionPointer>,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub enum EmulationStatus {
    Continue,
    Suspended,
    Terminated,
}

impl Emulator {
    pub fn new() -> Emulator {
        Emulator::default()
    }

    pub fn set_initial_call_from_mfa(&mut self, mfa: &MFA, program: &Program) -> &mut Emulator {
        self.instr_ptr.borrow_mut().setup_mfa(&mfa, &program);
        self
    }

    pub fn set_initial_call_from_lambda(
        &mut self,
        lambda: &Lambda,
        program: &Program,
    ) -> &mut Emulator {
        self.registers
            .borrow_mut()
            .fill_globals_from_offset(lambda.arity, &lambda.environment);
        self.instr_ptr.borrow_mut().setup_lambda(&lambda, &program);
        self
    }

    pub fn preload(&mut self, x: u32, v: Value) -> &mut Emulator {
        self.registers.borrow_mut().put(&Register::Global(x), v);
        self
    }

    pub fn run(
        &self,
        reduction_count: u64,
        program: &Program,
        scheduler: &mut Scheduler,
        runtime: &mut Box<dyn Runtime>,
        mailbox: &Mailbox,
        pid: Pid,
    ) -> Result<EmulationStatus, Error> {
        trace!("Emulator starting for {}", pid);
        let mut reductions = 0;
        let mut last_red = 0;

        let mut registers = self.registers.borrow_mut();
        let mut instr_ptr = self.instr_ptr.borrow_mut();

        while reductions < reduction_count {
            if last_red < reductions {
                trace!(
                    "================ reduction #{} ================",
                    reductions
                );
                last_red = reductions;
            }
            trace!("{}", registers);
            trace!("{}", instr_ptr);
            trace!("{:?}", mailbox);
            match instr_ptr.instr.clone() {
                ////////////////////////////////////////////////////////////////
                //
                // Kill switch
                //
                Instruction::Halt => return Ok(EmulationStatus::Terminated),

                ////////////////////////////////////////////////////////////////
                //
                // Working with the Register Machine
                //
                Instruction::Move(value, target) => {
                    match value {
                        Value::Register(reg) => {
                            registers.copy(&reg, &target);
                        }
                        v => {
                            registers.put(&target, v.clone());
                        }
                    }
                    instr_ptr.next(&program);
                }

                Instruction::Swap(a, b) => {
                    registers.swap(a, b);
                    instr_ptr.next(&program);
                }

                Instruction::Clear(reg) => {
                    registers.clear(&reg);
                    instr_ptr.next(&program);
                }

                Instruction::ShiftLocals { amount, .. } => {
                    registers.shift_locals(amount);
                    instr_ptr.next(&program);
                }

                Instruction::RestoreLocals => {
                    registers.restore_last_local();
                    instr_ptr.next(&program);
                }

                Instruction::Allocate { .. } => {
                    instr_ptr.next(&program);
                }

                Instruction::Deallocate { .. } => {
                    instr_ptr.next(&program);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Function calls
                //
                Instruction::Call(
                    FnCall::BuiltIn {
                        module,
                        function,
                        arity,
                        arguments,
                        destination,
                    },
                    _,
                ) => {
                    let args: Vec<Literal> = arguments
                        .iter()
                        .map(|a| registers.get_literal_from_value(&a))
                        .collect();
                    let ret = runtime.execute(
                        &MFA {
                            module,
                            function,
                            arity,
                        },
                        &args,
                    );
                    registers.put(&destination, ret.into());
                    instr_ptr.next(&program);
                    reductions += 1;
                }

                Instruction::Call(
                    FnCall::ApplyLambda {
                        register, arity, ..
                    },
                    _,
                ) => {
                    let value = registers.get(&register);
                    if let Literal::Lambda(lambda) = value.clone().into() {
                        registers.fill_globals_from_offset(arity, &lambda.environment);
                        registers.push_new_local();
                        instr_ptr.call(
                            &program,
                            &FnCall::Local {
                                module: lambda.module,
                                label: lambda.first_label,
                                arity: lambda.arity,
                            },
                            true,
                        );
                    } else {
                        panic!(
                            "Can not apply value found in {:?} since it is not a lambda",
                            value
                        )
                    };
                    reductions += 1;
                }

                Instruction::Call(call, FnKind::User) => {
                    registers.clear_globals(call.arity());
                    registers.push_new_local();
                    instr_ptr.call(&program, &call, true);
                    reductions += 1;
                }

                Instruction::TailCall(call, FnKind::User) => {
                    registers.clear_globals(call.arity());
                    instr_ptr.call(&program, &call, false);
                    reductions += 1;
                }

                Instruction::Call(call, FnKind::Native) => {
                    let arity = call.arity();
                    let args = registers.get_many_literals_from_global_range(0, arity);
                    let ret = runtime.execute(&call.into(), &args);
                    registers.clear_globals(arity);
                    registers.put_global(0, Value::Literal(ret));
                    instr_ptr.next(&program);
                    reductions += 1;
                }

                Instruction::TailCall(call, FnKind::Native) => {
                    let arity = call.arity();
                    let args = registers.get_many_literals_from_global_range(0, arity);
                    let ret = runtime.execute(&call.into(), &args);
                    registers.clear_globals(arity);
                    registers.put_global(0, Value::Literal(ret));
                    instr_ptr.next(&program);
                    reductions += 1;
                }

                ////////////////////////////////////////////////////////////////
                //
                // Flow-control operations
                //
                Instruction::Jump(label) => {
                    instr_ptr.jump_to_label(&program, &label);
                    reductions += 1;
                }

                Instruction::Return => {
                    instr_ptr.next(&program);
                }

                Instruction::Test(label, test) => {
                    if self.run_test(&test, &mut registers) {
                        trace!("Test passed! continuing...");
                        instr_ptr.next(&program);
                    } else {
                        trace!("Test failed! Jumping to: {}", label);
                        instr_ptr.jump_to_label(&program, &label);
                    }
                }

                /* NOTE(@ostera): this needs work, why did we not match? what's the error? */
                Instruction::Badmatch => return Ok(EmulationStatus::Terminated),

                ////////////////////////////////////////////////////////////////
                //
                //  Creating Values
                //
                Instruction::ConsList { target, head, tail } => {
                    let head = Box::new(registers.get_literal_from_value(&head));
                    let tail = {
                        let lit = registers.get_literal_from_value(&tail);
                        match lit {
                            Literal::List(t) => Box::new(t),
                            _ => panic!(
                                "Can not allow consing improper list with {} and {}",
                                head, lit
                            ),
                        }
                    };
                    registers.put(&target, Literal::List(List::Cons(head, tail)).into());
                    instr_ptr.next(&program);
                }

                Instruction::SplitList { list, head, tail } => {
                    match registers.get(&list).into() {
                        Literal::List(List::Cons(boxed_head, boxed_tail)) => {
                            registers
                                .put(&head, Value::Literal(*boxed_head.clone()))
                                .put(&tail, Literal::List(*boxed_tail.clone()).into());
                        }
                        value => panic!("Attempted to split a value that is not a list: {}", value),
                    };
                    instr_ptr.next(&program);
                }

                Instruction::SplitListTail { list, tail } => {
                    match registers.get(&list).into() {
                        Literal::List(List::Cons(_, boxed_tail)) => {
                            registers.put(&tail, Literal::List(*boxed_tail.clone()).into());
                        }
                        value => panic!("Attempted to split a value that is not a list: {}", value),
                    };
                    instr_ptr.next(&program);
                }

                Instruction::SplitListHead { list, head } => {
                    match registers.get(&list).into() {
                        Literal::List(List::Cons(boxed_head, _)) => {
                            registers.put(&head, Value::Literal(*boxed_head.clone()));
                        }
                        value => panic!("Attempted to split a value that is not a list: {}", value),
                    };
                    instr_ptr.next(&program);
                }

                Instruction::GetTupleElement {
                    tuple,
                    element,
                    target,
                } => {
                    match registers.get(&tuple) {
                        Value::Literal(Literal::Tuple(Tuple { elements, .. })) => {
                            let element = elements[element as usize].clone();
                            registers.put(&target, Value::Literal(element));
                        }
                        v => panic!(
                            "Attempted to extract element from value that is not a tuple: {}",
                            v
                        ),
                    }
                    instr_ptr.next(&program);
                }

                Instruction::MakeLambda {
                    module,
                    first_label,
                    arity,
                    environment_size,
                } => {
                    let environment =
                        registers.get_many_literals_from_global_range(0, environment_size);
                    registers.put(
                        &Register::Global(0),
                        Value::Literal(Literal::Lambda(Lambda {
                            first_label,
                            module,
                            environment,
                            arity,
                        })),
                    );
                    instr_ptr.next(&program);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Processes
                //
                Instruction::Kill => return Ok(EmulationStatus::Terminated),

                Instruction::PidSelf(target) => {
                    registers.put(&target, Literal::Pid(pid.clone()).into());
                    instr_ptr.next(&program);
                }

                Instruction::Spawn(spawn) => {
                    let pid = match spawn {
                        Spawn::Lambda { register } => {
                            let value = registers.get(&register);
                            if let Literal::Lambda(lambda) = value.clone().into() {
                                scheduler.spawn_from_lambda(&lambda)
                            } else {
                                panic!("Value at {:?} is not a lambda", value)
                            }
                        }
                        _ => panic!("mfa spawns are not supported yet"),
                    };
                    registers.put_global(0, Literal::Pid(pid).into());
                    registers.restore_last_local();
                    instr_ptr.next(&program);
                    reductions += 1;
                }

                Instruction::Sleep(label) => {
                    instr_ptr.jump_to_label(&program, &label);
                    return Ok(EmulationStatus::Suspended);
                }

                Instruction::PeekMessage {
                    on_mailbox_empty,
                    message,
                } => {
                    match mailbox.peek_next() {
                        None => {
                            trace!("No messages");
                            instr_ptr.jump_to_label(&program, &on_mailbox_empty);
                        }
                        Some(msg) => {
                            trace!("Peeked message: {}", msg);
                            registers.put(&message, msg.clone().into());
                            instr_ptr.next(&program);
                        }
                    }
                    reductions += 1;
                }
                Instruction::RemoveMessage => {
                    mailbox.drop_current();
                    instr_ptr.next(&program);
                }

                Instruction::Send { message, process } => {
                    let message = registers.get_literal_from_value(&message);
                    let pid = match registers.get_literal_from_value(&process) {
                        Literal::Pid(pid) => pid,
                        x => panic!("Can not send a message to non-pid: {}", x),
                    };
                    scheduler.send_message(&pid, &message);
                    registers.put_global(0, Value::Literal(message));
                    instr_ptr.next(&program);
                }

                Instruction::Monitor => {
                    instr_ptr.next(&program);
                }

                Instruction::Label(_) => instr_ptr.next(&program),
            }
        }
        Ok(EmulationStatus::Continue)
    }

    pub fn run_test(&self, test: &Test, registers: &mut Registers) -> bool {
        trace!("Running test: {:?}", test);
        match test {
            Test::IsGreaterOrEqualThan(a, b) => {
                let a: BigInt = registers.get_literal_from_value(a).into();
                let b: BigInt = registers.get_literal_from_value(b).into();
                a >= b
            }

            Test::IsNil(a) => match registers.get_literal_from_value(a) {
                Literal::List(List::Nil) => true,
                Literal::List(_) => false,
                _ => panic!("Can not check if non list value {} is an empty list", a),
            },

            Test::IsNonEmptyList(a) => match registers.get_literal_from_value(a) {
                Literal::List(List::Cons(_, _)) => true,
                Literal::List(List::Nil) => false,
                _ => panic!("Can not check if non list value {} is a non empty list", a),
            },

            Test::Equals(a, b) => {
                registers.get_literal_from_value(a) == registers.get_literal_from_value(b)
            }

            Test::IsTaggedTuple { value, atom, .. } => {
                match registers.get_literal_from_value(value) {
                    Literal::Tuple(Tuple { elements, .. }) => match elements[0].clone() {
                        Literal::Atom(tag) => tag.eq(atom),
                        _ => false,
                    },
                    x => panic!("Cannot check if value {} is a tagged tuple", x),
                }
            }

            Test::IsFunctionWithArity { fun, arity, .. } => match registers.get(&fun) {
                Value::Literal(Literal::Lambda(Lambda { arity: a2, .. })) => *arity == a2,
                x => panic!("Cannot check arity of non-function value: {}", x),
            },
        }
    }
}
