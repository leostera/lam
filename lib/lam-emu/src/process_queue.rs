use super::bytecode::*;
use super::emulator::*;
use super::literal::*;
use super::process::*;
use super::program::*;
use log::*;
use std::collections::VecDeque;

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct ProcessQueue {
    process_count: u64,
    ready: VecDeque<Process>,
    asleep: VecDeque<Process>,
    dead: VecDeque<Process>,
}

impl ProcessQueue {
    pub fn new() -> ProcessQueue {
        ProcessQueue::default()
    }

    pub fn enqueue(&mut self, p: Process) -> &mut ProcessQueue {
        trace!("Enqueue: {}", p.pid());
        match p.status() {
            Status::Alive => self.ready.push_back(p),
            Status::Terminated => self.dead.push_back(p),
            Status::Suspended => self.asleep.push_back(p),
        }
        self
    }

    pub fn spawn(&mut self, mfa: &MFA, args: Value, program: &Program, scheduler_id: u32) -> Pid {
        let pid = Pid {
            scheduler_id,
            process_id: self.process_count,
        };
        trace!("Spawned: {:?} with args {:?} as {} ", mfa, args, pid);
        let mut emulator = Emulator::new(&mfa, &program);
        emulator.preload(0, args);
        self.enqueue(Process::new(pid.clone(), mfa.clone(), emulator));
        self.process_count += 1;
        pid
    }

    pub fn next_process(&mut self) -> Option<Process> {
        self.ready.pop_front()
    }
}
