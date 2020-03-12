use sql_parser::select;
use std::path::PathBuf;
use structopt::StructOpt;

/// Queries one or more structured files with SQL
#[derive(Debug, StructOpt)]
#[structopt(version = "0.0.1")]
struct Opts {
    /// Query to issue
    query: String,

    /// Files to query
    #[structopt(name = "file", parse(from_os_str))]
    files: Vec<PathBuf>,
}

fn main() {
    let opt = Opts::from_args();
    let query = select(&opt.query).unwrap();
    println!("Running Query: {}", query);
    println!("Against files: {:?}", opt.files);
}
