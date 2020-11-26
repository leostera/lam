use super::program::*;
use super::runtime::*;
use anyhow::Error;

#[repr(C)]
pub struct Coordinator {
    program: Program,
    runtime: Box<dyn Runtime>,
    scheduler_count: u32,
}

impl Coordinator {
    pub fn new(scheduler_count: u32, program: Program, runtime: Box<dyn Runtime>) -> Coordinator {
        Coordinator {
            scheduler_count,
            runtime,
            program,
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        self.runtime
            .run_schedulers(self.scheduler_count, &self.program)
    }
}
