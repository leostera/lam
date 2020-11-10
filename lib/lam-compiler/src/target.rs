use lam_emu::program::Program;
use std::path::PathBuf;

#[derive(Clone, Debug, Default)]
pub struct Target {
    bytecode: Program,
    output: PathBuf,
}

impl Target {
    pub fn of_bytecode(bytecode: Program) -> Target {
        Target {
            bytecode,
            ..Target::default()
        }
    }

    pub fn bytecode(&self) -> &Program {
        &self.bytecode
    }

    pub fn output(&self) -> &PathBuf {
        &self.output
    }

    pub fn with_name(self, output: PathBuf) -> Target {
        Target { output, ..self }
    }
}
