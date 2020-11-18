use super::bytecode::*;
use super::literal::*;
use super::program::*;
use super::runtime::Runtime;
use super::scheduler::*;
use anyhow::Error;
use std::boxed::Box;
use std::collections::VecDeque;

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
        self.registers.put(Register::Global(x), v);
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
            trace!("Registers: {:?}", self.registers,);
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
                    self.registers.put(target, value.clone());
                    self.instr_ptr.next(&program);
                }

                Instruction::Swap(a, b) => {
                    self.registers.swap(a, b);
                    self.instr_ptr.next(&program);
                }

                Instruction::Clear(reg) => {
                    self.registers.clear(reg);
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
                    self.registers.put(destination, ret.into());
                    self.instr_ptr.next(&program);
                }

                Instruction::Call(call) => {
                    let arity = call.arity();
                    let args = self.registers.get_many_literals_from_global_range(0, arity);
                    let ret = runtime.execute(&call.into(), &args);
                    self.registers.clear_globals(arity);
                    self.registers.put_global(0, Value::Literal(ret));
                    self.instr_ptr.next(&program);
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
                Instruction::ConsList { target, head, tail } => {
                    let head = Box::new(self.registers.get_literal_from_value(&head));
                    let tail = Box::new(self.registers.get_literal_from_value(&tail));
                    self.registers
                        .put(target, Literal::List(List::Cons(head, tail)).into());
                    self.instr_ptr.next(&program);
                }

                Instruction::SplitList { list, head, tail } => {
                    match self.registers.get(&list).into() {
                        Literal::List(List::Cons(boxed_head, boxed_tail)) => {
                            self.registers
                                .put(head, Value::Literal(*boxed_head.clone()));
                            self.registers
                                .put(tail, Value::Literal(*boxed_tail.clone()));
                        }
                        value => {
                            panic!("Attempted to split a value that is not a list: {:?}", value)
                        }
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
                            self.registers.put(target, Value::Literal(element));
                        }
                        v => panic!(
                            "Attempted to extract element from value that is not a tuple: {:?}",
                            v
                        ),
                    }
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
                let a: BigInt = self.registers.get_literal_from_value(a).into();
                let b: BigInt = self.registers.get_literal_from_value(b).into();
                a >= b
            }

            Test::IsNil(a) => match self.registers.get_literal_from_value(a) {
                Literal::List(List::Nil) => true,
                Literal::List(_) => false,
                _ => panic!("Can not check if non list value {:?} is an empty list", a),
            },

            Test::IsNonEmptyList(a) => match self.registers.get_literal_from_value(a) {
                Literal::List(List::Cons(_, _)) => true,
                Literal::List(List::Nil) => false,
                _ => panic!(
                    "Can not check if non list value {:?} is a non empty list",
                    a
                ),
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
                x => panic!("Cannot check if value {:?} is a tagged tuple", x),
            },
        }
    }
}

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct Registers {
    global: Vec<Value>,
    local: VecDeque<Vec<Value>>,
    current_local: Vec<Value>,
}

impl Registers {
    pub fn new() -> Registers {
        let empty: Vec<Value> = [0; 32].to_vec().iter().map(|_| Value::Nil).collect();
        Registers {
            global: empty.clone(),
            local: VecDeque::new(),
            current_local: empty,
        }
    }

    pub fn clear(&mut self, r: Register) -> &mut Registers {
        self.put(r, Value::Nil);
        self
    }

    pub fn clear_globals(&mut self, n: u32) -> &mut Registers {
        for i in (n as usize)..self.global.len() {
            self.global[i] = Value::Nil;
        }
        self
    }

    pub fn put_global(&mut self, n: u32, v: Value) -> &mut Registers {
        self.global[n as usize] = v.clone();
        self
    }

    pub fn put(&mut self, r: Register, v: Value) -> &mut Registers {
        match r {
            Register::Local(l) => self.current_local[l as usize] = v.clone(),
            Register::Global(g) => self.global[g as usize] = v.clone(),
        }
        self
    }

    pub fn get(&self, r: &Register) -> Value {
        trace!("fetching register: {:?}", r);
        match r {
            Register::Local(l) => self.current_local[*l as usize].clone(),
            Register::Global(g) => self.global[*g as usize].clone(),
        }
    }

    pub fn get_many_literals_from_global_range(&self, from: u32, to: u32) -> Vec<Literal> {
        self.global[from as usize..to as usize]
            .iter()
            .map(|v| self.get_literal_from_value(v))
            .collect()
    }

    pub fn get_literal_from_value(&self, v: &Value) -> Literal {
        trace!("fetching value: {:?}", v);
        match v {
            Value::Literal(l) => l.clone(),
            Value::Register(reg) => self.get_literal_from_value(&self.get(reg)),
            Value::Nil => Literal::List(List::Nil),
        }
    }

    pub fn swap(&mut self, a: Register, b: Register) {
        match (a, b) {
            (Register::Local(l0), Register::Local(l1)) => {
                self.current_local.swap(l0 as usize, l1 as usize);
            }
            (Register::Global(g0), Register::Global(g1)) => {
                self.global.swap(g0 as usize, g1 as usize);
            }
            (Register::Local(l), Register::Global(g))
            | (Register::Global(g), Register::Local(l)) => {
                let temp = self.current_local[l as usize].clone();
                self.current_local[l as usize] = self.global[g as usize].clone();
                self.global[g as usize] = temp;
            }
        }
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
