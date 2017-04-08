# Space Invaders
An emulator for the original Space Invaders game, including the emulation of the 8080 CPU.

You must be running the latest nightly version of Rust in order to compile it.

## Building
`cargo build --release`

## Running
`cargo run --release`

There are a few options that can be passed to the binary produced, of which two are only currently supported:

### dis
Disassemble an 8080-compiled binary: `emu8080 dis /path/to/binary.bin`.

### spaceinvaders
Play a provided Space Invaders binary: `emu8080 spaceinvaders /path/to/spaceinvaders.bin`.

This requires OpenAL and libsndfile to play sounds. Follow instructions at https://github.com/jhasse/ears for installation on your system.

The sounds should be located in the same directory as the binary and be named '0.wav' -> '8.wav' for the appropriate sounds to be played.
