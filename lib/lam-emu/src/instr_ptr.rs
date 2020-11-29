use super::bytecode::*;
use super::literal::*;
use super::program::*;
use std::boxed::Box;
use std::fmt::{Display, Formatter};

use log::*;

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct InstructionPointer {
    last_instr_ptr: Option<Box<InstructionPointer>>,
    current_module: String,
    current_label: Label,
    current_instruction: usize,
    pub instr: Instruction,
}

impl Display for InstructionPointer {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "Current ML: {}/{} -- ",
            self.current_module, self.current_label
        )?;
        write!(fmt, "Current Instr: {:?}", self.instr)
    }
}

impl InstructionPointer {
    pub fn new() -> InstructionPointer {
        trace!("Creating empty instruction pointer");
        InstructionPointer {
            last_instr_ptr: None,
            current_module: "".to_string(),
            current_label: 0,
            current_instruction: 0,
            instr: Instruction::Halt,
        }
    }

    pub fn setup_mfa(&mut self, mfa: &MFA, program: &Program) -> &mut InstructionPointer {
        trace!("Setting pointer up for {:#?}", mfa);
        let module = program.modules.get(&mfa.module).unwrap();
        let function_key = (mfa.function.clone(), mfa.arity);
        let first_label = module
            .functions
            .get(&function_key)
            .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key));
        let first_instruction = module.labels[*first_label as usize].instructions[0].clone();

        self.last_instr_ptr = None;
        self.current_module = module.name.clone();
        self.current_label = *first_label;
        self.current_instruction = 0;
        self.instr = first_instruction;
        self
    }

    pub fn setup_lambda(&mut self, lambda: &Lambda, program: &Program) -> &mut InstructionPointer {
        trace!("Setting pointer up for {:?}", lambda);

        let module = program.modules.get(&lambda.module).unwrap();
        let first_instruction = module.labels[lambda.first_label as usize].instructions[0].clone();

        self.last_instr_ptr = None;
        self.current_module = lambda.module.clone();
        self.current_label = lambda.first_label;
        self.current_instruction = 0;
        self.instr = first_instruction;
        self
    }

    pub fn get_next(&self, program: &Program) -> InstructionPointer {
        let module = program.modules.get(&self.current_module).unwrap();

        let label = (self.current_label) as usize;
        let instructions = &module.labels[label].instructions;

        let last_offset = instructions.len();

        let next_instr = self.current_instruction + 1;

        let should_fall_through = match self.instr {
            Instruction::RestoreLocals
            | Instruction::Return
            | Instruction::Spawn { .. }
            | Instruction::Call(_, _)
            | Instruction::TailCall(_, _) => false,
            _ => true,
        };

        let mut next_instr_ptr = self.clone();
        // First lets try to advance to the next instruciton in the current label
        if next_instr < last_offset {
            next_instr_ptr.current_instruction += 1;
            next_instr_ptr.instr =
                instructions[next_instr_ptr.current_instruction as usize].clone();
        }
        // Otherwise, lets check if the last instruction grants us a label fall through
        else if should_fall_through && label + 1 < module.labels.len() {
            /* Follow to the next label if we're done with this one */
            next_instr_ptr.current_instruction = 0;
            next_instr_ptr.current_label = (label + 1) as u32;
            next_instr_ptr.instr = module.labels[label + 1].instructions[0].clone();
        }
        // if we shouldn't fall through, lets see if we can jump back
        else if let Some(last_ptr) = &self.last_instr_ptr {
            next_instr_ptr = (**last_ptr).clone();
        }
        // lastly, there's nothing more to do, so we just halt
        else {
            next_instr_ptr.instr = Instruction::Halt;
        }
        next_instr_ptr
    }

    /// Advance the planned execution.
    pub fn next(&mut self, program: &Program) {
        *self = self.get_next(&program);
    }

    /// Performs a jump based on the function call, by looking up the appropriate next instruction
    /// and threading the function's instructions in the middle.
    ///
    /// Will jump back right into the planned execution after the function call is over.
    ///
    pub fn call(&mut self, program: &Program, call: &FnCall, pop_after: bool) {
        let next_ptr = self.get_next(&program);

        let module_name = call
            .module()
            .unwrap_or_else(|| next_ptr.current_module.clone());
        let module = program
            .modules
            .get(&module_name)
            .unwrap_or_else(|| panic!("Could not find module: {:?}", &module_name));
        let first_label = match call {
            FnCall::Local { label, .. } => label,
            _ => {
                let function_key = (call.function(), call.arity());
                module
                    .functions
                    .get(&function_key)
                    .unwrap_or_else(|| panic!("Could not find function : {:?}", &function_key))
            }
        };
        let last_label = module.labels.len();
        let last_instruction = module.labels[*first_label as usize].instructions.len();
        let first_instruction = module.labels[*first_label as usize].instructions[0].clone();

        let next_instr = if !pop_after {
            InstructionPointer {
                last_instr_ptr: Some(Box::new(next_ptr)),
                current_module: module.name.clone(),
                current_label: *first_label,
                current_instruction: 0,
                instr: first_instruction,
            }
        } else {
            /* If we should pop the local stack after calling this function,
             * then we will interject a RestoreLocals call before returning
             * to the continuation.
             * NOTE(@ostera): this feels hacky!
             * */
            InstructionPointer {
                last_instr_ptr: Some(Box::new(InstructionPointer {
                    last_instr_ptr: Some(Box::new(next_ptr)),
                    current_module: module.name.clone(),
                    current_label: (last_label as u32) - 1,
                    current_instruction: last_instruction - 1,
                    instr: Instruction::RestoreLocals,
                })),
                current_module: module.name.clone(),
                current_label: *first_label,
                current_instruction: 0,
                instr: first_instruction,
            }
        };

        *self = next_instr;
    }

    /// Performs a jump to a label in the same module, by looking up the label number into the
    /// Program.
    ///
    /// It will preserve the _last instruction pointer_, so it will jump right back into the
    /// planned execution after the jump is over.
    ///
    pub fn jump_to_label(&mut self, program: &Program, label: &Label) {
        trace!("Jumping to label: {:?}", label);

        let module_name = self.current_module.clone();
        let module = program
            .modules
            .get(&module_name)
            .unwrap_or_else(|| panic!("Could not find module: {:?}", &module_name));
        trace!("Found module: {:?}", module_name);

        let first_instruction = module.labels[*label as usize].instructions[0].clone();
        trace!("First instruction: {:?}", first_instruction);

        *self = InstructionPointer {
            current_module: module_name,
            current_label: *label,
            current_instruction: 0,
            instr: first_instruction,
            last_instr_ptr: self.last_instr_ptr.clone(),
        }
    }

    /// If there is an instruction to return to, this will jump back there, continuing the planned
    /// execution.
    ///
    /// If there isn't one, it will consider this program halted.
    ///
    pub fn return_to_last_instr(&mut self) {
        match &self.last_instr_ptr {
            Some(last_ptr) => {
                *self = *last_ptr.clone();
            }
            None => {
                self.instr = Instruction::Halt;
            }
        };
    }
}
