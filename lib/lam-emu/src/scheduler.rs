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
pub struct Scheduler<'a> {
    id: u32,
    sleep_delay: u64,
    reduction_count: u64,
    current_process: u32,
    process_registry: ProcessRegistry,
    process_queue: ProcessQueue,
    program: &'a Program,
}

impl<'a> Scheduler<'a> {
    pub fn new(id: u32, program: &'a Program) -> Scheduler<'a> {
        Scheduler {
            id,
            program,
            sleep_delay: 20,
            reduction_count: 100,
            current_process: 0,
            process_queue: ProcessQueue::new(),
            process_registry: ProcessRegistry::new(),
        }
    }

    pub fn run(mut self, mut runtime: Box<dyn Runtime>) -> Result<(), Error> {
        debug!("Scheduler {:?} started...", self.id);

        loop {
            if let Some(pid) = self.process_queue.next_process() {
                if let Some(mut process) = self.process_registry.get(&pid) {
                    debug!("Working on process {}", &pid);
                    if let Ok(_) =
                        process.run(self.reduction_count, self.program, &mut self, &mut runtime)
                    {
                        self.process_queue.enqueue(&process);
                    }
                    self.process_registry.update(&process);
                }
            } else {
                break;
                /* NOTE(@ostera): we gotta figure out how exactly to let
                 * schedulers idle _unless_  we know that its time to stop

                if !is_asleep {
                    debug!("Scheduler {} going to sleep...", self.id);
                }
                runtime.sleep(self.sleep_delay);
                is_asleep = true;

                */
            };
        }

        Ok(())
    }

    pub fn boot(&mut self, arg: Value) -> &mut Scheduler<'a> {
        self.spawn_from_mfa(&self.program.main, vec![arg]);
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
        match self.process_registry.get_mut(&pid) {
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

    /*
    fn kill(&mut self, pid: &Pid);
    fn link(&mut self, a: &Pid, b: &Pid);
    fn unlink(&mut self, a: &Pid, b: &Pid);
    fn monitor(&mut self, a: &Pid, b: &Pid);
    fn unmonitor(&mut self, a: &Pid, b: &Pid);
    */
}
