use anyhow::{Context, Error};

use super::target::Target;

const JS_TEMPLATE: &str = include_str!("index.js");

#[cfg(debug_assertions)]
const RUNTIME: &[u8] =
    include_bytes!("../../../target/wasm32-unknown-unknown/debug/lam_rts_web.wasm");

#[cfg(not(debug_assertions))]
const RUNTIME: &[u8] =
    include_bytes!("../../../target/wasm32-unknown-unknown/release/lam_rts_web.wasm");

impl Target {
    pub fn to_web(&self) -> Result<(), Error> {
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

        main.func_body()
            .i32_const(0)
            .i32_const(data.len() as i32)
            .call(start);

        let main = main.finish(vec![], &mut module.funcs);

        module
            .exports
            .add("boot", walrus::ExportItem::Function(main));

        /* Write the final .wasm to disk */
        module.emit_wasm_file(self.output())?;

        std::fs::write(
            self.output().with_extension("js"),
            JS_TEMPLATE.replace("OUTPUT_NAME", self.output().to_str().unwrap()),
        )?;

        Ok(())
    }
}
