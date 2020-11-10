use anyhow::{Context, Error};

use super::target::Target;

#[cfg(debug_assertions)]
const RUNTIME: &[u8] = include_bytes!("../../../target/wasm32-wasi/debug/lam_rts_wasm.wasm");

#[cfg(not(debug_assertions))]
const RUNTIME: &[u8] = include_bytes!("../../../target/wasm32-wasi/release/lam_rts_wasm.wasm");

impl Target {
    pub fn to_wasm(self) -> Result<(), Error> {
        /* Prepare the bytecode */
        let data = self.bytecode().serialize()?;

        /* Create runtime module */
        let mut module = walrus::Module::from_buffer(RUNTIME)?;

        let memory_id = module.memories.iter_mut().last().unwrap().id();

        /* Create our data section */
        module.data.add(
            walrus::DataKind::Active(walrus::ActiveData {
                memory: memory_id,
                location: walrus::ActiveDataLocation::Absolute(0),
            }),
            data.to_vec(),
        );

        /* Find runtime start function */
        let start = module
            .funcs
            .by_name("start")
            .context("Could not find start function in runtime.wasm")?;

        /* Build main function */
        let mut main = walrus::FunctionBuilder::new(&mut module.types, &[], &[]);

        main.name("main".to_string())
            .func_body()
            .i32_const(0)
            .i32_const(data.len() as i32)
            .call(start);

        let main = main.finish(vec![], &mut module.funcs);
        module.start = Some(main);

        /* Write the final .wasm to disk */
        module.emit_wasm_file(self.output())?;

        Ok(())
    }
}
