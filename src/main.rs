use anyhow::Context as _;
use clap::Parser as _;
use std::path::PathBuf;

#[derive(clap::Parser)]
#[clap(no_binary_name = true)]
struct Args {
    #[clap(hide = true)]
    binary: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let binary = args
        .binary
        .file_name()
        .context("Failed to get binary name")?
        .to_str()
        .context("Binary name not valid UTF-8")?;

    #[allow(clippy::match_single_binding)]
    match binary {
        binary => {
            anyhow::bail!("wtf does {binary} mean");
        }
    }
}
