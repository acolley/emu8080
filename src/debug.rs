
use std::io;
use std::io::{BufWriter, BufRead, BufReader, Read, Write};

use nom;
use nom::{eof, hex_u32, space};

use machine;
use machine::{Machine};

#[derive(Clone, Debug, Eq, PartialEq)]
enum Printable {
    Register(machine::Reg),
    Addr(u16),
    Breakpoints
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Cmd {
    Break(u16),
    Continue,
    Exit, // Exit the debugger
    List, // Print context around current instruction
    Step,
    Print(Printable),
}

named!(parse_addr<Printable>,
    chain!(
        tag!("a")     ~
        many1!(space) ~
        addr: hex_u32 ,
        || { Printable::Addr(addr as u16) }
    )
);

named!(parse_register<Printable>,
    chain!(
        tag!("r")     ~
        many1!(space) ~
        reg: alt!(
            tag!("a") => { |_| machine::Reg::A } |
            tag!("b") => { |_| machine::Reg::B } |
            tag!("c") => { |_| machine::Reg::C } |
            tag!("d") => { |_| machine::Reg::D } |
            tag!("e") => { |_| machine::Reg::E } |
            tag!("h") => { |_| machine::Reg::H } |
            tag!("l") => { |_| machine::Reg::L }
        ),
        || { Printable::Register(reg) }
    )
);

named!(parse_printable<Printable>,
    alt!(
        tag!("b") => { |_| Printable::Breakpoints } |
        parse_register |
        parse_addr
    )
);

named!(parse_print<Cmd>,
    chain!(
        tag!("p")                  ~
        many1!(space)              ~
        printable: parse_printable ,
        || { Cmd::Print(printable) }
    )
);

named!(parse_break<Cmd>,
    chain!(
        tag!("b")      ~
        many1!(space)  ~
        value: hex_u32 , // TODO: make a hex_u16 parser based on hex_u32
        || { Cmd::Break(value as u16) }
    )
);

named!(parse_cmd<Cmd>,
    // alt! tags must be in length order, shortest -> longest
    // See http://rust.unhandledexpression.com/nom/macro.alt!.html
    alt!(
        eof => { |_| Cmd::Exit } |
        tag!("c") => { |_| Cmd::Continue } |
        tag!("s") => { |_| Cmd::Step } |
        parse_break |
        parse_print
    )
);

/// Virtual debugger that provides a nicer interface
/// for stepping through a program.
pub struct Debugger<T: Machine> {
    machine: T,
    breakpoints: Vec<u16>,
}

impl<T: Machine> Debugger<T> {
    pub fn new(machine: T) -> Self {
        Debugger {
            machine: machine,
            breakpoints: Vec::new(),
        }
    }

    /// Should the program break execution?
    fn should_break(&self) -> bool {
        self.breakpoints.iter().any(|&pc| self.machine.get_pc() == pc)
    }

    pub fn run(&mut self) {
        let mut last_cmd = Cmd::Step;
        'main: loop {
            match last_cmd {
                Cmd::Continue => { // continue until breakpoint is hit
                    if self.should_break() {
                        last_cmd = Cmd::Step;
                    }
                },
                _ => { // prompt user for input
                    let mut writer = BufWriter::new(io::stdout());
                    writer.write(b">>> ").expect("Could not write to stdout.");
                    writer.flush();                    

                    let mut buf = String::new();
                    let mut reader = BufReader::new(io::stdin());
                    reader.read_line(&mut buf).expect("Could not read from stdin");

                    let cmd = parse_cmd(buf.trim().as_bytes());
                    // let cmd = parse_break(buf.trim().as_bytes());
                    match cmd {
                        nom::IResult::Done(input, output) => {
                            match output {
                                // TODO: allow printing in hexadecimal format
                                Cmd::Break(addr) => self.breakpoints.push(addr), // break when PC becomes this address
                                Cmd::Continue => {}, // continue until next breakpoint is hit
                                Cmd::Exit => { break 'main },
                                Cmd::List => {}, // TODO: implement this
                                Cmd::Step => { self.machine.step(); }, // step into next instruction
                                Cmd::Print(Printable::Addr(addr)) => println!("{}", self.machine.read(addr)),
                                Cmd::Print(Printable::Register(machine::Reg::A)) => println!("{}", self.machine.get_a()),
                                Cmd::Print(Printable::Register(machine::Reg::B)) => println!("{}", self.machine.get_b()),
                                Cmd::Print(Printable::Register(machine::Reg::C)) => println!("{}", self.machine.get_c()),
                                Cmd::Print(Printable::Register(machine::Reg::D)) => println!("{}", self.machine.get_d()),
                                Cmd::Print(Printable::Register(machine::Reg::E)) => println!("{}", self.machine.get_e()),
                                Cmd::Print(Printable::Register(machine::Reg::H)) => println!("{}", self.machine.get_h()),
                                Cmd::Print(Printable::Register(machine::Reg::L)) => println!("{}", self.machine.get_l()),
                                Cmd::Print(Printable::Breakpoints) => println!("{:?}", self.breakpoints),
                            }
                            last_cmd = output;
                        },
                        _ => println!("Unrecognised command"),
                    }
                }
            }
        }
    }
}
