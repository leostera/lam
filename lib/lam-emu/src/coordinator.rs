use super::program::*;
use super::runtime::*;
use anyhow::Error;
use log::*;

#[repr(C)]
pub struct Coordinator {
    program: Program,
    scheduler_manager: Box<dyn SchedulerManager>,
    scheduler_count: u32,
}

#[repr(C)]
pub enum CoordinatorAction {
    Halt,
}

impl Coordinator {
    pub fn new(
        scheduler_count: u32,
        program: Program,
        scheduler_manager: Box<dyn SchedulerManager>,
    ) -> Coordinator {
        Coordinator {
            scheduler_count,

            scheduler_manager,
            program,
        }
    }

    /// Set up schedulers and prepare for execution.
    ///
    /// This function will spin up new schedulers and make sure all necessary communication
    /// channels are up and running between them.
    pub fn setup(&mut self) -> Result<(), Error> {
        self.scheduler_manager
            .setup(self.scheduler_count, &self.program)?;
        info!("All {} Schedulers started.", self.scheduler_count);
        Ok(())
    }

    /// Tear down schedulers in an orderly fashion.
    pub fn halt(&self) -> Result<(), Error> {
        info!("halting!");
        Ok(())
    }

    /// The main coordination loop.
    ///
    /// This function will run for as long as the schedulers are configured to. This means it may
    /// return only when all schedulers have completed all of their work, which could be never. Or
    /// it may return as soon as the scheduler steppers are done, which means we can call it again
    /// to continue doing work.
    pub fn run(&mut self) -> Result<(), Error> {
        self.scheduler_manager.run(&self)?;
        Ok(())
    }

    /// Step the coordination loop.
    pub fn step(&self) -> Result<(), Error> {
        Ok(())
    }
}
