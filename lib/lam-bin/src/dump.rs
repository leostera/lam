use anyhow::Error;
use log::{debug, info};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

use lam_beam::beam_reader;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "dump",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "dump the parsed instructions"
)]
pub struct DumpOpt {
    #[structopt(
        name = "FILES",
        help = "the .beam files to compile",
        parse(from_os_str)
    )]
    files: Vec<PathBuf>,

    #[structopt(
        short = "s",
        long = "stage",
        name = "STAGE",
        help = "the stage at which to dump the inputs",
        default_value = "lam"
    )]
    stage: Stage,
}

#[derive(StructOpt, Debug, Clone)]
#[structopt()]
pub enum Stage {
    BEAM,
    LAM,
}

impl FromStr for Stage {
    type Err = String;
    fn from_str(stage: &str) -> Result<Self, Self::Err> {
        match stage {
            "beam" => Ok(Stage::BEAM),
            "lam" => Ok(Stage::LAM),
            _ => Err("Could not parse stage. Please use lam or beam".to_string()),
        }
    }
}

impl DumpOpt {
    pub fn dump(self) -> Result<(), Error> {
        let t0 = std::time::Instant::now();
        info!("Building project...");

        let t1 = std::time::Instant::now();
        let mut beams = Vec::new();
        for f in self.files {
            beams.push(beam_reader::Reader::from_file(&f).unwrap());
        }
        debug!("Read bytecode in {}ms", t1.elapsed().as_millis());

        match self.stage {
            Stage::BEAM => {
                println!("{:#?}", beams);
            }
            Stage::LAM => {
                let t2 = std::time::Instant::now();
                let program: lam_emu::program::Program =
                    lam_compiler::Translator::default().from_bytecode(beams);

                debug!("Built program in {}ms", t2.elapsed().as_millis());
                println!("{:#?}", program);
            }
        }

        info!("Done in {}ms", t0.elapsed().as_millis());
        return Ok(());
    }
}
