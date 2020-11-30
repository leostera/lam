use super::emulator::*;
use super::literal::*;
use super::mailbox::*;
use super::program::*;
use super::runtime::*;
use super::scheduler::*;
use anyhow::Error;
use log::*;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub enum Status {
    Alive,
    Suspended,
    Terminated,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Process {
    status: RefCell<Status>,
    pid: Pid,
    emulator: Emulator,
    pub mailbox: Mailbox,
}

impl Process {
    pub fn new(pid: Pid, emulator: Emulator) -> Process {
        Process {
            pid,
            status: RefCell::new(Status::Alive),
            emulator,
            mailbox: Mailbox::new(),
        }
    }

    pub fn send_message(&self, m: Message) {
        self.mailbox.deliver(m);
    }

    pub fn is_suspended(&self) -> bool {
        self.status.borrow().eq(&Status::Suspended)
    }

    pub fn is_terminated(&self) -> bool {
        self.status.borrow().eq(&Status::Terminated)
    }

    pub fn suspend(&self) {
        *self.status.borrow_mut() = Status::Suspended;
    }

    pub fn terminate(&self) {
        *self.status.borrow_mut() = Status::Terminated;
    }

    pub fn status(&self) -> Status {
        self.status.borrow().clone()
    }

    pub fn pid(&self) -> Pid {
        self.pid.clone()
    }

    pub fn run(
        &self,
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
            &self.mailbox,
            pid,
        )? {
            EmulationStatus::Terminated => self.terminate(),
            // NOTE(@ostera): these 2 statuses should be handled differently
            EmulationStatus::Continue | EmulationStatus::Suspended => self.suspend(),
        };
        Ok(())
    }
}
