use super::bytecode::Instruction;
use super::emulator::Emulator;
use super::program::Program;
use super::runtime::Runtime;
use log::debug;

pub struct Runner {
    emulator: Emulator,
    runtime: Box<dyn Runtime>,
    program: Program,
}

impl Runner {
    pub fn for_program(program: Program, runtime: Box<dyn Runtime>) -> Runner {
        Runner {
            emulator: Emulator::default(),
            program,
            runtime,
        }
    }

    pub fn run(&mut self) {
        debug!("Program: {:#?}", &self.program);
        debug!("==================================================");
        for instr in self.program.instructions().flat_map(|instr| match instr {
            _ => vec![instr],
        }) {
            debug!("Instr => {:?}", instr);
            self.emulator.execute(instr, &mut self.runtime);
            debug!("{:#?}", self.emulator.registers);
        }
        self.emulator.execute(&Instruction::Halt, &mut self.runtime);
    }
}
