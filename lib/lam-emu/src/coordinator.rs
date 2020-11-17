use super::program::*;
use super::runtime::*;
use super::scheduler::*;
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
        let scheduler_count = self.scheduler_count;
        let mut schedulers = vec![];
        for i in 0..scheduler_count {
            let s = Scheduler::new(i, &self.program);
            schedulers.push(s);
        }
        self.runtime.run_schedulers(schedulers)
    }
}
