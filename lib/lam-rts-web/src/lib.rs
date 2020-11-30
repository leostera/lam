use lam_emu::{Coordinator, List, Literal, Program};
use log::*;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

mod refs;
mod web_runtime;
use web_runtime::*;

thread_local! {
    pub static COORDINATOR: RefCell<Option<Coordinator>> = RefCell::new(None);
}

#[wasm_bindgen]
pub fn start(data: *const u8, size: usize) {
    let program = program(data, size);

    let reduction_count = 20;
    let step_count = 10;
    let args = Literal::List(List::Nil).into();
    let scheduler_manager = WebSchedulerManager::new(args, reduction_count, step_count, &program);

    console_log::init_with_level(log::Level::Info).unwrap();
    info!("Initializing Web Runtime...");

    COORDINATOR.with(|c| {
        let mut coordinator = Coordinator::new(1, program, Box::new(scheduler_manager));
        coordinator.setup().unwrap();
        *c.borrow_mut() = Some(coordinator);
    })
}

#[wasm_bindgen]
pub fn step() -> Result<(), JsValue> {
    COORDINATOR.with(|c| match c.borrow_mut().iter_mut().next() {
        Some(coordinator) => {
            coordinator.run().unwrap();
        }
        None => {
            error!("Did you forget to call boot first? How did we get here");
        }
    });
    Ok(())
}

fn program(data: *const u8, size: usize) -> lam_emu::Program {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    Program::deserialize(&data).unwrap()
}
