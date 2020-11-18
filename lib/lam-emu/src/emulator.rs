use super::bytecode::*;
use super::literal::*;
use super::program::*;
use super::runtime::Runtime;
use super::scheduler::*;
use anyhow::Error;
use std::boxed::Box;

use log::*;

use num_bigint::BigInt;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Emulator {
    registers: Vec<Value>,
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
            registers: [0; 32].to_vec().iter().map(|_| Value::Nil).collect(),
            instr_ptr: InstructionPointer::new(mfa, program),
        }
    }

    pub fn preload(&mut self, x: usize, v: Value) -> &mut Emulator {
        self.registers[x] = v;
        self
    }

    pub fn run(
        &mut self,
        reduction_count: u64,
        program: &Program,
        _scheduler: &mut Scheduler,
        runtime: &mut Box<dyn Runtime>,
    ) -> Result<EmulationStatus, Error> {
        let mut reductions = 0;
        while reductions < reduction_count {
            trace!("Reductions: {:?}", reductions);
            trace!(
                "Registers: \n    0 => {:?}\n    1 => {:?}\n    2 => {:?}",
                self.registers[0],
                self.registers[1],
                self.registers[2]
            );
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
                Instruction::Move(value, Register::X(rx)) => {
                    self.registers[rx as usize] = value.clone();
                    self.instr_ptr.next(&program);
                }

                Instruction::Swap(Register::X(rx0), Register::X(rx1)) => {
                    self.registers.swap(rx0 as usize, rx1 as usize);
                    self.instr_ptr.next(&program);
                }

                Instruction::Clear(Register::X(rx)) => {
                    self.registers[rx as usize] = Value::Nil;
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
                    destination: Register::X(rx),
                }) => {
                    let args = self.fetch_values(&arguments);
                    let ret = runtime.execute(
                        &MFA {
                            module,
                            function,
                            arity,
                        },
                        &args,
                    );
                    self.registers[rx as usize] = Value::Literal(ret);
                    self.instr_ptr.next(&program);
                }

                Instruction::Call(call) => {
                    let arity = call.arity() as usize;
                    let args = self.fetch_values(&self.registers[0..arity]);
                    let ret = runtime.execute(&call.into(), &args);
                    for i in 0..arity {
                        self.registers[i] = Value::Nil;
                    }
                    self.registers[0] = Value::Literal(ret);
                    self.instr_ptr.next(&program);
                }

                Instruction::TailCall(call) => {
                    let arity = call.arity() as usize;
                    let args = self.fetch_values(&self.registers[0..arity]);
                    let ret = runtime.execute(&call.into(), &args);
                    for i in 0..arity {
                        self.registers[i] = Value::Nil;
                    }
                    self.registers[0] = Value::Literal(ret);
                    self.instr_ptr.next(&program);
                }

                ////////////////////////////////////////////////////////////////
                //
                // Flow-control operations
                //
                Instruction::Jump(label) => {
                    trace!("Jump to label {:?}", label);
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
                        trace!("Test failed! Jumping to: {:?}", label);
                        self.instr_ptr.jump_to_label(&program, &label);
                    }
                }

                ////////////////////////////////////////////////////////////////
                //
                //  Creating Values
                //
                Instruction::ConsList {
                    target: Register::X(rx),
                    head,
                    tail,
                } => {
                    let head = Box::new(self.fetch_register(&head));
                    let tail = Box::new(self.fetch_register(&tail));
                    self.registers[rx as usize] =
                        Value::Literal(Literal::List(List::Cons(head, tail)));
                    self.instr_ptr.next(&program);
                }

                Instruction::SplitList {
                    list: Register::X(rl),
                    head: Register::X(rh),
                    tail: Register::X(rt),
                } => {
                    let value = &self.registers[rl as usize];
                    match self.fetch_value(&value) {
                        Literal::List(List::Cons(boxed_head, boxed_tail)) => {
                            self.registers[rh as usize] = Value::Literal(*boxed_head.clone());
                            self.registers[rt as usize] = Value::Literal(*boxed_tail.clone());
                        }
                        _ => panic!("Attempted to split a value that is not a list: {:?}", value),
                    };
                    self.instr_ptr.next(&program);
                }

                Instruction::Kill => return Ok(EmulationStatus::Terminated),

                ////////////////////////////////////////////////////////////////
                //
                // Function calls
                //
                _ => self.instr_ptr.next(&program),
            }
            reductions += 1;
        }
        Ok(EmulationStatus::Continue)
    }

    pub fn run_test(&self, test: &Test) -> bool {
        trace!("Running test: {:?}", test);
        match test {
            Test::IsGreaterOrEqualThan(a, b) => {
                let a: BigInt = self.fetch_value(a).into();
                let b: BigInt = self.fetch_value(b).into();
                a >= b
            }

            Test::IsNil(a) => match self.fetch_value(a) {
                Literal::List(List::Nil) => true,
                Literal::List(_) => false,
                _ => panic!("Can not check if non list value {:?} is an empty list", a),
            },

            Test::IsNonEmptyList(a) => match self.fetch_value(a) {
                Literal::List(List::Cons(_, _)) => true,
                Literal::List(List::Nil) => false,
                _ => panic!(
                    "Can not check if non list value {:?} is a non empty list",
                    a
                ),
            },

            Test::Equals(a, b) => self.fetch_value(a) == self.fetch_value(b),

            Test::IsTaggedTuple {
                value,
                element,
                atom,
            } => match self.fetch_value(value) {
                Literal::Tuple(Tuple { elements, .. }) => match &elements[*element as usize] {
                    Literal::Atom(tag) => tag == atom,
                    _ => false,
                },
                x => panic!("Cannot check if value {:?} is a tagged tuple", x),
            },
        }
    }

    pub fn fetch_values(&self, vs: &[Value]) -> Vec<Literal> {
        vs.iter().map(|v| self.fetch_value(v)).collect()
    }

    pub fn fetch_value(&self, v: &Value) -> Literal {
        trace!("fetching value: {:?}", v);
        match v {
            Value::Literal(l) => l.clone(),
            Value::Register(Register::X(rx)) => {
                trace!("found register, fetching from: {:?}", *rx);
                self.fetch_value(&self.registers[*rx as usize])
            }
            Value::Nil => Literal::List(List::Nil),
            _ => panic!("Could not fetch value: {:?}", v),
        }
    }

    pub fn fetch_register(&self, r: &Register) -> Literal {
        self.fetch_value(&Value::Register(r.clone()))
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct InstructionPointer {
    last_instr_ptr: Option<Box<InstructionPointer>>,
    current_module: String,
    current_label: Label,
    current_instruction: usize,
    pub instr: Instruction,
}

impl InstructionPointer {
    pub fn new(mfa: &MFA, program: &Program) -> InstructionPointer {
        trace!("Creating pointer for {:#?} in {:#?}", mfa, program);
        let module = program.modules.get(&mfa.module).unwrap();
        let function_key = (mfa.function.clone(), mfa.arity);
        let first_label = module
            .functions
            .get(&function_key)
            .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key));
        let first_instruction = module.labels[*first_label as usize].instructions[0].clone();

        InstructionPointer {
            last_instr_ptr: None,
            current_module: module.name.clone(),
            current_label: *first_label,
            current_instruction: 0,
            instr: first_instruction,
        }
    }

    pub fn next(&mut self, program: &Program) {
        let module = program.modules.get(&self.current_module).unwrap();

        let label = (self.current_label) as usize;
        let instructions = &module.labels[label].instructions;

        // let last_label = module.labels.len();
        let last_offset = instructions.len();

        let next_instr = self.current_instruction + 1;
        if next_instr < last_offset {
            self.current_instruction += 1;
            self.instr = instructions[self.current_instruction as usize].clone()
        } else if let Some(last_ptr) = &self.last_instr_ptr {
            *self = (**last_ptr).clone()
        } else {
            self.instr = Instruction::Halt
        }
    }

    pub fn jump(&mut self, program: &Program, call: &FnCall) {
        let last_ptr = self.clone();

        let module_name = call
            .module()
            .unwrap_or_else(|| last_ptr.current_module.clone());
        let module = program
            .modules
            .get(&module_name)
            .unwrap_or_else(|| panic!("Could not find module: {:?}", &module_name));
        /* NOTE: labels are 1 indexed! */
        let function_key = (call.function(), call.arity());
        let first_label = module
            .functions
            .get(&function_key)
            .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key));
        let first_instruction = module.labels[*first_label as usize].instructions[0].clone();

        *self = InstructionPointer {
            last_instr_ptr: Some(Box::new(last_ptr)),
            current_module: module.name.clone(),
            current_label: *first_label,
            current_instruction: 0,
            instr: first_instruction,
        }
    }

    pub fn jump_to_label(&mut self, program: &Program, label: &Label) {
        trace!("Jumping to label: {:?}", label);

        let last_ptr = self.clone();

        let module_name = last_ptr.current_module.clone();
        let module = program
            .modules
            .get(&module_name)
            .unwrap_or_else(|| panic!("Could not find module: {:?}", &module_name));
        trace!("Found module: {:?}", module_name);

        let first_instruction = module.labels[*label as usize].instructions[0].clone();
        trace!("First instruction: {:?}", first_instruction);

        *self = InstructionPointer {
            current_module: last_ptr.current_module,
            current_label: *label,
            current_instruction: 0,
            instr: first_instruction,
            last_instr_ptr: None,
        }
    }

    pub fn return_to_last_instr(&mut self) {
        let last_ptr = self.last_instr_ptr.clone().unwrap();
        *self = *last_ptr;
    }
}
