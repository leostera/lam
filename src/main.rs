use fern::colors::{Color, ColoredLevelConfig};
use log::{debug, info};
use std::path::PathBuf;
use structopt::StructOpt;

use lam::bytecode::Reader;
use lam::runtime::Runner;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "lam",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "
░█▒░▒▄▀▄░█▄▒▄█
▒█▄▄░█▀█░█▒▀▒█

Compile your BEAM bytecode to WASM.
"
)]
struct LAM {
    #[structopt(short = "v", long = "verbose", help = "turn on verbosity")]
    verbose: bool,

    #[structopt(subcommand)]
    cmd: Goal,
}

impl LAM {
    async fn run(self) {
        self.setup_logging();
        self.cmd.run().await;
    }

    fn setup_logging(&self) {
        let colors_line = ColoredLevelConfig::new()
            .error(Color::Red)
            .warn(Color::Yellow)
            .info(Color::White)
            .debug(Color::White)
            .trace(Color::BrightBlack);
        let colors_level = colors_line.clone().info(Color::Green);
        fern::Dispatch::new()
            .format(move |out, message, record| {
                out.finish(format_args!(
                    "{color_line}{date} {level}{color_line} :: {message}\x1B[0m",
                    color_line = format_args!(
                        "\x1B[{}m",
                        colors_line.get_color(&record.level()).to_fg_str()
                    ),
                    date = chrono::Local::now().format("%H:%M:%S"),
                    level = colors_level.color(record.level()),
                    message = message,
                ));
            })
            .level(if self.verbose {
                log::LevelFilter::Debug
            } else {
                log::LevelFilter::Info
            })
            .level_for("pretty_colored", log::LevelFilter::Trace)
            .chain(std::io::stdout())
            .apply()
            .unwrap();
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Goal {
    Build(BuildOpt),
}

impl Goal {
    async fn run(self) {
        match self {
            Goal::Build(opts) => opts.build().await,
        }
    }
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "build", about = "build this project")]
struct BuildOpt {
    #[structopt(
        name = "FILES",
        help = "the .beam files to compile",
        parse(from_os_str)
    )]
    files: Vec<PathBuf>,
}

impl BuildOpt {
    async fn build(self) {
        let t0 = std::time::Instant::now();
        debug!("Files: {:?}", self.files);
        lam::bytecode::Reader::from_file(PathBuf::from("")).unwrap();
        info!("Building project...");
        info!("Done in {}ms", t0.elapsed().as_millis());
    }
}

#[tokio::main]
async fn main() {
    LAM::from_args().run().await;
}
