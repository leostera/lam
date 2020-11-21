use super::emulator::*;
use super::literal::*;
use super::process::*;
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

    pub fn is_empty(&self) -> bool {
        self.ready.is_empty()
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

    pub fn spawn_and_ready(&mut self, emulator: Emulator, scheduler_id: u32) -> Pid {
        let pid = Pid {
            scheduler_id,
            process_id: self.process_count,
        };
        let process = Process::new(pid.clone(), emulator);
        self.enqueue(process);
        self.process_count += 1;
        pid
    }

    pub fn next_process(&mut self) -> Option<Process> {
        self.ready.pop_front()
    }
}
