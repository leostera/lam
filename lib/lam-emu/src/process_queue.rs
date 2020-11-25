use super::literal::*;
use super::process::*;
use log::*;
use std::collections::VecDeque;

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct ProcessQueue {
    ready: VecDeque<Pid>,
    asleep: VecDeque<Pid>,
    dead: VecDeque<Pid>,
}

impl ProcessQueue {
    pub fn new() -> ProcessQueue {
        ProcessQueue::default()
    }

    pub fn is_empty(&self) -> bool {
        self.ready.is_empty()
    }

    pub fn enqueue(&mut self, p: &Process) -> &mut ProcessQueue {
        let pid = p.pid();
        trace!("Enqueue: {}", pid);
        match p.status() {
            Status::Alive => self.ready.push_back(pid),
            Status::Terminated => self.dead.push_back(pid),
            Status::Suspended => self.asleep.push_back(pid),
        }
        self
    }

    pub fn ready(&mut self, p: &Pid) -> &mut ProcessQueue {
        self.ready.push_back(p.clone());
        self
    }

    pub fn next_process(&mut self) -> Option<Pid> {
        self.ready.pop_front()
    }
}
