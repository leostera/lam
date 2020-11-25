use anyhow::{Context, Error};
use log::{debug, info};
use std::path::PathBuf;
use structopt::StructOpt;

use lam_beam::beam_reader::Reader;
use lam_compiler::Translator;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "compile",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "compile .beam to .lam bytecode objects"
)]
pub struct CompileOpt {
    #[structopt(
        name = "FILES",
        help = "the .beam files to compile",
        parse(from_os_str)
    )]
    files: Vec<PathBuf>,

    #[structopt(
        short = "o",
        long = "output",
        name = "OUTPUT",
        help = "the output path where the .lam files will be written",
        parse(from_os_str)
    )]
    output: PathBuf,
}

impl CompileOpt {
    pub fn compile(self) -> Result<(), Error> {
        let t0 = std::time::Instant::now();
        info!("Compiling {} .beam files...", self.files.len());
        for f in self.files {
            let t = std::time::Instant::now();

            let filename = f.with_extension("lam");
            let filename = filename
                .file_name()
                .context("could not add extension to file path")?;

            // read one .beam file
            let beam = Reader::from_file(&f)?;

            // translate it to LAM bytecode
            let program = Translator::default().from_bytecode(vec![beam]);

            // write .lam file
            std::fs::write(
                PathBuf::from(self.output.join(filename)),
                program.serialize()?,
            )?;

            debug!("Compiled {:?} in {}ms", f, t.elapsed().as_millis());
        }
        info!("Done in {}ms", t0.elapsed().as_millis());

        Ok(())
    }
}
