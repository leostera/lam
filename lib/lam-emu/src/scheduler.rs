use super::bytecode::*;
use super::process::*;
use super::program::*;
use super::runtime::*;
use anyhow::Error;
use log::trace;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Scheduler<'a> {
    id: u32,
    sleep_delay: u64,
    reduction_count: u64,
    current_process: u32,
    process_queue: ProcessQueue,
    program: &'a Program,
}

impl<'a> Scheduler<'a> {
    pub fn new(id: u32, program: &'a Program) -> Scheduler<'a> {
        Scheduler {
            id,
            program,
            sleep_delay: 20,
            reduction_count: 1000,
            current_process: 0,
            process_queue: ProcessQueue::new(),
        }
    }

    pub fn run(mut self, mut runtime: Box<dyn Runtime>) -> Result<(), Error> {
        trace!("Scheduler {:?} started...", self.id);

        loop {
            if let Some(mut process) = self.process_queue.next_process() {
                trace!("Working on process {}", process.pid());
                match process.run(self.reduction_count, self.program, &mut self, &mut runtime) {
                    Ok(_) => {
                        self.process_queue.enqueue(process);
                    }
                    Err(_) => {
                        break;
                    }
                }
            } else {
                runtime.sleep(self.sleep_delay);
            }
        }

        Ok(())
    }

    pub fn boot(&mut self, args: Value) -> &mut Scheduler<'a> {
        self.process_queue
            .spawn(&self.program.main, args, &self.program, self.id);
        self
    }

    /*
    fn spawn(&mut self, mfa, args);
    fn kill(&mut self, pid: &Pid);

    fn link(&mut self, a: &Pid, b: &Pid);
    fn unlink(&mut self, a: &Pid, b: &Pid);

    fn monitor(&mut self, a: &Pid, b: &Pid);
    fn unmonitor(&mut self, a: &Pid, b: &Pid);
    */
}
