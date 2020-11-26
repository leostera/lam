use super::emulator::*;
use super::literal::*;
use super::mailbox::*;
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

impl Status {
    pub fn suspend(&mut self) {
        *self = Status::Suspended;
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Process {
    status: Status,
    pid: Pid,
    emulator: Emulator,
    pub mailbox: Mailbox,
}

impl Process {
    pub fn new(pid: Pid, emulator: Emulator) -> Process {
        Process {
            pid,
            status: Status::Alive,
            emulator,
            mailbox: Mailbox::new(),
        }
    }

    pub fn set_status(&mut self, s: Status) -> &mut Process {
        self.status = s;
        self
    }

    pub fn set_emulator(&mut self, e: Emulator) -> &mut Process {
        self.emulator = e;
        self
    }

    pub fn send_message(&mut self, m: Message) -> &mut Process {
        self.mailbox.deliver(m);
        self
    }

    pub fn terminate(&mut self) {
        self.status = Status::Terminated;
    }

    pub fn emulator(&self) -> &Emulator {
        &self.emulator
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
        let pid = self.pid();
        match self.emulator.run(
            reduction_count,
            program,
            scheduler,
            runtime,
            &mut self.status,
            &mut self.mailbox,
            pid,
        )? {
            EmulationStatus::Terminated => self.terminate(),
            EmulationStatus::Continue => (),
        };
        Ok(())
    }
}
