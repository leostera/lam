use anyhow::Error;
use log::*;
use structopt::StructOpt;

use lam_bin::commands::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "lam",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "
░█▒░▒▄▀▄░█▄▒▄█
▒█▄▄░█▀█░█▒▀▒█

A little actor machine that turns BEAM bytecode
into Native and WebAssembly binaries.

"
)]
struct LAM {
    #[structopt(short = "v", long = "verbose", help = "turn on verbosity")]
    verbose: bool,

    #[structopt(subcommand)]
    cmd: Goal,
}

impl LAM {
    fn run(self) -> Result<(), Error> {
        env_logger::init();
        self.cmd.run()
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Goal {
    Build(BuildOpt),
    Dump(DumpOpt),
    Link(LinkOpt),
    Compile(CompileOpt),
}

impl Goal {
    fn run(self) -> Result<(), Error> {
        match self {
            Goal::Build(opts) => opts.build(),
            Goal::Dump(opts) => opts.dump(),
            Goal::Link(opts) => opts.link(),
            Goal::Compile(opts) => opts.compile(),
        }
    }
}

fn main() {
    match LAM::from_args().run() {
        Ok(()) => (),
        Err(err) => error!("Something went wrong: {:?}", err),
    }
}
