# emu8080
An emulator for 8080 CPU written in Rust.

Also contains an implementation of the Space Invaders machine that uses the emulated CPU.

## Building
`cargo build --release`

## Running
`cargo run --release`

There are a few options that can be passed to the binary produced, of which two are only currently supported:

### dis
Disassemble an 8080-compiled binary: `emu8080 dis /path/to/binary.bin`.

### spaceinvaders
Play a provided Space Invaders binary: `emu8080 spaceinvaders /path/to/spaceinvaders.bin`.
