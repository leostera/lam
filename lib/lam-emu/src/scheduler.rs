use super::bytecode::*;
use super::emulator::*;
use super::literal::*;
use super::mailbox::*;
use super::process_queue::*;
use super::process_registry::*;
use super::program::*;
use super::runtime::*;
use anyhow::Error;
use log::*;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Scheduler {
    id: u32,
    reduction_count: u64,
    process_registry: ProcessRegistry,
    process_queue: ProcessQueue,
    program: Program,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub enum RunFuel {
    Bounded(u32),
    Infinite,
}

impl Scheduler {
    pub fn new(id: u32, reduction_count: u64, program: Program) -> Scheduler {
        Scheduler {
            id,
            program,
            reduction_count,
            process_queue: ProcessQueue::new(),
            process_registry: ProcessRegistry::new(),
        }
    }

    pub fn boot(&mut self, arg: Value) -> &mut Scheduler {
        let main = self.program.main.clone();
        self.spawn_from_mfa(&main, vec![arg]);
        self
    }

    pub fn spawn_from_mfa(&mut self, mfa: &MFA, args: Vec<Value>) -> Pid {
        let mut emulator = Emulator::new();

        emulator.set_initial_call_from_mfa(&mfa, &self.program);
        for (i, a) in args.iter().enumerate() {
            emulator.preload(i as u32, a.clone());
        }

        let pid = self.process_registry.spawn(emulator, self.id);
        self.process_queue.ready(&pid);
        trace!("Spawned MFA {:?} with args {:?} into {}", mfa, args, pid);
        pid
    }

    pub fn spawn_from_lambda(&mut self, lambda: &Lambda) -> Pid {
        let mut emulator = Emulator::new();

        emulator.set_initial_call_from_lambda(lambda, &self.program);

        let pid = self.process_registry.spawn(emulator, self.id);
        self.process_queue.ready(&pid);
        trace!("Spawned Lambda {:?} into {}", lambda, pid);
        pid
    }

    pub fn send_message(&mut self, pid: &Pid, message: &Message) {
        match self.process_registry.get(&pid) {
            None => {
                trace!(
                    "Attempted to send message {} to {}, but couldn't find it",
                    message,
                    pid
                );
            }
            Some(p) => {
                trace!("Sending message {} to {}", message, pid);
                p.send_message(message.clone());
            }
        }
    }

    pub fn stepper(self, iterations: RunFuel) -> Stepper {
        Stepper {
            program: self.program.clone(),
            scheduler: RefCell::new(self),
            iterations,
        }
    }

    /*
    fn kill(&mut self, pid: &Pid);
    fn link(&mut self, a: &Pid, b: &Pid);
    fn unlink(&mut self, a: &Pid, b: &Pid);
    fn monitor(&mut self, a: &Pid, b: &Pid);
    fn unmonitor(&mut self, a: &Pid, b: &Pid);
    */
}

use std::cell::RefCell;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Stepper {
    scheduler: RefCell<Scheduler>,
    iterations: RunFuel,
    program: Program,
}

impl Stepper {
    pub fn step(&self, mut runtime: Box<dyn Runtime>) -> Result<(), Error> {
        debug!(
            "Scheduler stepper {:?} started...",
            self.scheduler.borrow().id
        );

        let mut current_iter = 0;
        let program = self.program.clone();
        let mut scheduler = self.scheduler.borrow_mut();
        loop {
            if let RunFuel::Bounded(max_iterations) = self.iterations {
                trace!("Steps: {}/{}", current_iter, max_iterations);
                if current_iter < max_iterations {
                    current_iter += 1;
                } else {
                    break;
                }
            }
            if let Some(pid) = scheduler.process_queue.next_process() {
                if let Some(process) = scheduler.process_registry.get(&pid) {
                    debug!("Working on process {}", &pid);
                    if process
                        .run(
                            scheduler.reduction_count,
                            &program,
                            &mut scheduler,
                            &mut runtime,
                        )
                        .is_ok()
                    {
                        scheduler.process_queue.enqueue(&process);
                    }
                }
            } else {
                break;
            };
        }

        Ok(())
    }
}
