#![feature(slice_patterns)]

extern crate clap;
#[macro_use]
extern crate glium;
extern crate time;

mod cpu;
mod disassemble;
mod spaceinvaders;

use std::fs::File;
use std::io::Read;
use std::u8;

use clap::{Arg, App, SubCommand};

use cpu::Cpu;
use disassemble::disassemble;
use spaceinvaders::SpaceInvadersMachine;

enum Options {
    SpaceInvaders {
        filename: String,
    },
    Run {
        filename: String,
        offset: usize,
    },
    Disassemble {
        filename: String,
        offset: usize,
    },
}

fn get_opts() -> Options {
    let matches = App::new("emu8080")
        .version("0.1")
        .subcommand(SubCommand::with_name("run")
            .arg(Arg::with_name("FILENAME")
                .required(true))
            .arg(Arg::with_name("OFFSET")
                .short("o")
                .long("offset")
                .takes_value(true)))
        .subcommand(SubCommand::with_name("dis")
            .arg(Arg::with_name("FILENAME")
                .required(true))
            .arg(Arg::with_name("OFFSET")
                .short("o")
                .long("offset")
                .takes_value(true)))
        .subcommand(SubCommand::with_name("spaceinvaders")
            .arg(Arg::with_name("FILENAME")
                .required(true)))
        .get_matches();

    if let Some(sub_matches) = matches.subcommand_matches("spaceinvaders") {
        Options::SpaceInvaders {
            filename: String::from(sub_matches.value_of("FILENAME").unwrap()),
        }
    } else {
        if let Some(sub_matches) = matches.subcommand_matches("run") {
            let offset = sub_matches.value_of("OFFSET").unwrap_or("0").parse::<usize>().ok().expect("--offset is not valid usize");
            Options::Run {
                filename: String::from(sub_matches.value_of("FILENAME").unwrap()),
                offset: offset,
            }
        } else {
            let sub_matches = matches.subcommand_matches("dis").unwrap();
            let offset = sub_matches.value_of("OFFSET").unwrap_or("0").parse::<usize>().ok().expect("--offset is not valid usize");
            Options::Disassemble {
                filename: String::from(sub_matches.value_of("FILENAME").unwrap()),
                offset: offset,
            }
        }
    }
}

fn read_file(filename: &str) -> Vec<u8> {
    let mut file = File::open(filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();
    buf
}

fn main() {
    let options = get_opts();

    match options {
        Options::SpaceInvaders { filename } => {
            let buf = read_file(&filename);
            let mut machine = SpaceInvadersMachine::new(&buf);
            machine.run();
        }
        Options::Run { filename, offset } => {
            let buf = read_file(&filename);
            let mut cpu = Cpu::with_data_and_offset(&buf, offset);
            loop {
                cpu.step();
            }
        },
        Options::Disassemble { filename, offset } => {
            let buf = read_file(&filename);
            disassemble(&buf, offset);
        },
    }
}
