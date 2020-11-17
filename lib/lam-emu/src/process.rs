use super::bytecode::*;
use super::emulator::*;
use super::program::*;
use super::runtime::*;
use super::scheduler::*;
use anyhow::Error;
use log::*;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
#[repr(C)]
pub enum Status {
    Alive,
    Suspended,
    Terminated,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Process {
    status: Status,
    pid: Pid,
    initial_mfa: MFA,
    emulator: Emulator,
}

impl Process {
    pub fn new(pid: Pid, initial_mfa: MFA, emulator: Emulator) -> Process {
        Process {
            pid,
            initial_mfa,
            status: Status::Alive,
            emulator,
        }
    }

    pub fn terminate(&mut self) {
        self.status = Status::Terminated;
    }

    pub fn status(&self) -> Status {
        self.status.clone()
    }

    pub fn pid(&self) -> Pid {
        self.pid.clone()
    }

    pub fn run(
        &mut self,
        reduction_count: u64,
        program: &Program,
        scheduler: &mut Scheduler,
        runtime: &mut Box<dyn Runtime>,
    ) -> Result<(), Error> {
        debug!("Active process {}", self.pid);
        match self
            .emulator
            .run(reduction_count, program, scheduler, runtime)?
        {
            EmulationStatus::Terminated => self.terminate(),
            _ => (),
        };
        Ok(())
    }
}

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
