use lam_emu::program::Program;
use std::path::PathBuf;

#[derive(Clone, Debug, Default)]
pub struct Target {
    program: Program,
    output: PathBuf,
}

impl Target {
    pub fn of_program(program: Program) -> Target {
        Target {
            program,
            ..Target::default()
        }
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn output(&self) -> &PathBuf {
        &self.output
    }

    pub fn with_name(self, output: PathBuf) -> Target {
        Target { output, ..self }
    }
}
