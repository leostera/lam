use super::emulator::*;
use super::literal::*;
use super::program::*;
use super::runtime::*;
use super::scheduler::*;
use anyhow::Error;
use log::*;

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
            .run(reduction_count, program, scheduler, runtime, self.pid())?
        {
            EmulationStatus::Terminated => self.terminate(),
            EmulationStatus::Continue => (),
        };
        Ok(())
    }
}
