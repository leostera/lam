use super::emulator::*;
use super::literal::*;
use super::process::*;
use log::*;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct ProcessRegistry {
    process_count: u64,
    processes: HashMap<Pid, Process>,
}

impl ProcessRegistry {
    pub fn new() -> ProcessRegistry {
        ProcessRegistry::default()
    }

    pub fn is_empty(&self) -> bool {
        self.processes.is_empty()
    }

    pub fn spawn(&mut self, emulator: Emulator, scheduler_id: u32) -> Pid {
        let pid = Pid {
            scheduler_id,
            process_id: self.process_count,
        };
        trace!("Spawned {}", &pid);
        let process = Process::new(pid.clone(), emulator);
        self.processes.insert(pid.clone(), process);
        self.process_count += 1;
        pid
    }

    pub fn get(&mut self, p: &Pid) -> Option<Process> {
        self.processes.get_mut(&p).cloned()
    }

    pub fn update(&mut self, p: &Process) {
        self.processes.insert(p.pid(), p.clone());
    }
}