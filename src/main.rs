#![feature(slice_patterns)]

extern crate clap;
#[macro_use]
extern crate glium;
extern crate time;

mod cpu;
mod disassemble;
mod spaceinvaders;

use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::u8;

use clap::{Arg, App};

use disassemble::disassemble;
use spaceinvaders::SpaceInvadersMachine;

struct Options {
    filename: String,
}

fn get_opts() -> Options {
    let matches = App::new("emu8080")
                       .version("0.1")
                       .arg(Arg::with_name("FILENAME")
                            .required(true))
                       .get_matches();
    let filename = matches.value_of("FILENAME").unwrap();
    Options {
        filename: String::from(filename),
    }
}

fn main() {
    let options = get_opts();
    let mut file = File::open(options.filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let mut machine = SpaceInvadersMachine::new(&buf);
    machine.run();

    // disassemble(&buf);

    // let program = match disassemble(&buf) {
    //     nom::IResult::Done(input, output) => Binary(output),
    //     _ => panic!("Failed to parse input.")
    // };
    // io::stdout().write(&format!("{}", program).into_bytes()).unwrap();
    // program.write(&mut io::stdout());
}

#[test]
fn test_nop() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x00);
    cpu.emulate();
    let mut expected = Cpu::new();
    expected.pc += 1;
    assert_eq!(cpu, expected);
}

#[test]
fn test_lxi_b() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x01);
    cpu.mem.write(1, 0xfe);
    cpu.mem.write(2, 0xff);
    let mut expected = cpu.clone();
    expected.pc = 3;
    expected.b = 0xff;
    expected.c = 0xfe;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_stax_b() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x02);
    cpu.mem.write(0xfffe, 0xaa);
    cpu.b = 0xfe;
    cpu.c = 0xff;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0xfe;
    expected.c = 0xff;
    expected.a = 0xaa;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_inx_b() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x03);
    cpu.b = 0xfe;
    cpu.c = 0xff;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0xff;
    expected.c = 0xff;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_inr_b() {
    //! Test increment register B
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x04);
    cpu.b = 0x01;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x02;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_inr_b_carry_zero() {
    //! Test increment register B with Carry and Zero flag
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x04);
    cpu.b = 0xff;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x00;
    expected.cc.z = 1;
    expected.cc.cy = 1;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_dcr_b() {
    //! Test decrement register B
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x05);
    cpu.b = 0x02;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x01;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_dcr_b_zero() {
    //! Test decrement register B with Zero flag
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x05);
    cpu.b = 0x01;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x00;
    expected.cc.z = 1;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_mvi_b() {
    //! Test move immediate word to register B
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x06);
    cpu.mem.write(1, 0xee);
    let mut expected = cpu.clone();
    expected.pc = 2;
    expected.b = 0xee;

    cpu.emulate();
    assert_eq!(cpu, expected);
}