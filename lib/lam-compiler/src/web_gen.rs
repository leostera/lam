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
        let data = self.program().serialize()?;

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

        let js_file = format!(
            "import 'regenerator-runtime/runtime'
{}
const boot = (args) => init(fetch(\"{}\")).then(mod => {{
  mod.boot(args);
  return mod;
}});
export default boot;",
            JS_TEMPLATE,
            self.output().file_name().unwrap().to_str().unwrap()
        )
        .replace("export default init;", "")
        .replace(
            "    if (typeof input === 'undefined') {
        input = import.meta.url.replace(/\\.js$/, '_bg.wasm');
    }",
            "",
        )
        .replace("imports.wbg = {};", "")
        .replace(
            "const imports = {};",
            "const imports = {};
imports.__wbindgen_externref_xform__ = {};
imports.__wbindgen_externref_xform__.__wbindgen_externref_table_grow = function() {};
imports.__wbindgen_externref_xform__.__wbindgen_externref_table_set_null = function() {};

imports.__wbindgen_placeholder__ = {};
imports.__wbindgen_placeholder__.__wbindgen_describe = function() {};
imports.__wbindgen_placeholder__.__wbindgen_describe_closure = function() {};
",
        )
        .replace("imports.wbg", "imports.__wbindgen_placeholder__");

        std::fs::write(self.output().with_extension("js"), js_file)?;

        Ok(())
    }
}
