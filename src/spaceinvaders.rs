use glium;
use glium::{DisplayBuild, Program, Rect, Surface};
use glium::backend::glutin_backend::GlutinFacade;
use glium::glutin;
use glium::glutin::ElementState::Pressed;
use glium::glutin::Event;
use glium::glutin::VirtualKeyCode;
use glium::index::PrimitiveType;
use glium::texture::{MipmapsOption, Texture2dDataSource, UncompressedFloatFormat};
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};

use time;

use cpu::Cpu;

#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

implement_vertex!(Vertex, position, tex_coords);

const WIDTH: u32 = 224;
const HEIGHT: u32 = 256;

/// Space Invaders, (C) Taito 1978, Midway 1979
///
/// CPU: Intel 8080 @ 2MHz (CPU similar to the (newer) Zilog Z80)    
///
/// Interrupts: $cf (RST 8) at the start of vblank, $d7 (RST $10) at the end of vblank.    
///
/// Video: 256(x)*224(y) @ 60Hz, vertical monitor. Colours are simulated with a    
/// plastic transparent overlay and a background picture.    
/// Video hardware is very simple: 7168 bytes 1bpp bitmap (32 bytes per scanline).    
///
/// Sound: SN76477 and samples.    
///
/// Memory map:    
/// ROM    
/// $0000-$07ff:    invaders.h
/// $0800-$0fff:    invaders.g
/// $1000-$17ff:    invaders.f
/// $1800-$1fff:    invaders.e
///
/// RAM    
/// $2000-$23ff:    work RAM
/// $2400-$3fff:    video RAM
///
/// $4000-:     RAM mirror
pub struct SpaceInvadersMachine {
    cpu: Cpu,
    time: u64, // time since machine start in nanoseconds
    shiftx: u8,
    shifty: u8,
    shift_offset: u8,
    window: GlutinFacade,
    program: Program,
    texture: glium::texture::Texture2d,
    vertex_buffer: glium::VertexBuffer<Vertex>,
    index_buffer: glium::IndexBuffer<u16>,
    vbuffer: Vec<Vec<(u8, u8, u8)>>,
}

impl SpaceInvadersMachine {
    pub fn new(data: &[u8]) -> Self {
        let window = glutin::WindowBuilder::new()
            .with_dimensions(WIDTH, HEIGHT)
            .with_title("Space Invaders".to_string())
            .build_glium()
            .expect("Could not create glutin window.");
        let program = program!(&window,
            140 => {
                vertex: "
                    #version 140
                    uniform mat4 matrix;
                    in vec2 position;
                    in vec2 tex_coords;
                    out vec2 v_tex_coords;
                    void main() {
                        gl_Position = matrix * vec4(position, 0.0, 1.0);
                        v_tex_coords = tex_coords;
                    }
                ",

                fragment: "
                    #version 140
                    uniform sampler2D tex;
                    in vec2 v_tex_coords;
                    out vec4 f_color;
                    void main() {
                        f_color = texture(tex, v_tex_coords);
                    }
                "
            },

            110 => {  
                vertex: "
                    #version 110
                    uniform mat4 matrix;
                    attribute vec2 position;
                    attribute vec2 tex_coords;
                    varying vec2 v_tex_coords;
                    void main() {
                        gl_Position = matrix * vec4(position, 0.0, 1.0);
                        v_tex_coords = tex_coords;
                    }
                ",

                fragment: "
                    #version 110
                    uniform sampler2D tex;
                    varying vec2 v_tex_coords;
                    void main() {
                        gl_FragColor = texture2D(tex, v_tex_coords);
                    }
                ",
            },

            100 => {  
                vertex: "
                    #version 100
                    uniform lowp mat4 matrix;
                    attribute lowp vec2 position;
                    attribute lowp vec2 tex_coords;
                    varying lowp vec2 v_tex_coords;
                    void main() {
                        gl_Position = matrix * vec4(position, 0.0, 1.0);
                        v_tex_coords = tex_coords;
                    }
                ",

                fragment: "
                    #version 100
                    uniform lowp sampler2D tex;
                    varying lowp vec2 v_tex_coords;
                    void main() {
                        gl_FragColor = texture2D(tex, v_tex_coords);
                    }
                ",
            },
        ).expect("Could not create shader program.");
        let hidpi_factor = window.get_window().unwrap().hidpi_factor() as u32;
        let texture = glium::texture::Texture2d::empty_with_format(&window,
            UncompressedFloatFormat::U8U8U8,
            MipmapsOption::NoMipmap,
            WIDTH * hidpi_factor, HEIGHT * hidpi_factor)
            .ok().expect("Could not create Texture2d.");
        let vertex_buffer = {
            // Full screen quad
            glium::VertexBuffer::new(&window, 
                &[
                    Vertex { position: [-1.0, -1.0], tex_coords: [0.0, 0.0] },
                    Vertex { position: [-1.0,  1.0], tex_coords: [0.0, 1.0] },
                    Vertex { position: [ 1.0,  1.0], tex_coords: [1.0, 1.0] },
                    Vertex { position: [ 1.0, -1.0], tex_coords: [1.0, 0.0] }
                ]
            ).expect("Could not create VertexBuffer.")
        };
        let index_buffer = glium::IndexBuffer::new(
            &window, PrimitiveType::TriangleStrip,
            &[1 as u16, 2, 0, 3]).expect("Could not create IndexBuffer.");
        let mut vbuffer = Vec::with_capacity(HEIGHT as usize);
        for _ in 0..HEIGHT {
            let mut row = Vec::with_capacity(WIDTH as usize);
            row.resize(WIDTH as usize, (0x00, 0x00, 0x00));
            vbuffer.push(row);
        }
        SpaceInvadersMachine {
            cpu: Cpu::with_data(data),
            time: 0,
            shiftx: 0,
            shifty: 0,
            shift_offset: 0,
            window: window,
            program: program,
            texture: texture,
            vertex_buffer: vertex_buffer,
            index_buffer: index_buffer,
            vbuffer: vbuffer,
        }
    }

    /// Step through a single instruction.
    /// Returns the number of clock cycles
    /// required for the instruction processed.
    #[inline(always)]
    pub fn step(&mut self) -> u32 {
        let op = self.cpu.mem.read(self.cpu.pc);
        // println!("pc: {:>0padpc$x}, op: {:>0padop$x}", self.cpu.pc, op, padpc=4, padop=2);
        // println!("pc: {:>0padpc$x} a: {:>0pad$x} b: {:>0pad$x} c: {:>0pad$x} d: {:>0pad$x} e: {:>0pad$x} h: {:>0pad$x} l: {:>0pad$x}", 
            self.cpu.pc, self.cpu.a, self.cpu.b, self.cpu.c, self.cpu.d, self.cpu.e, self.cpu.h, self.cpu.l, padpc=4, pad=2);
        match op {
            0xd3 => { // OUT D8
                let port = self.cpu.mem.read(self.cpu.pc + 1);
                let value = self.cpu.a;
                self.cpu.ports[port as usize] = value;
                match port {
                    2 => self.shift_offset = value & 0x7,
                    4 => {
                        self.shifty = self.shiftx;
                        self.shiftx = value;
                    },
                    _ => {}
                }
                self.cpu.pc += 2;
                10
            },
            0xdb => { // IN D8
                let port = self.cpu.mem.read(self.cpu.pc + 1);
                self.cpu.a = match port {
                    0 => 1,
                    1 => self.cpu.ports[1],
                    3 => {
                        let value = (self.shifty as u16) << 8 | self.shiftx as u16;
                        ((value >> (8 - self.shift_offset)) & 0xff) as u8
                    },
                    _ => self.cpu.a,
                };
                self.cpu.pc += 2;
                10
            },
            _ => self.cpu.step()
        }
    }

    /// Take the framebuffer from the emulated CPU and
    /// upload the data to a texture on the GPU.
    #[inline(always)]
    pub fn draw(&mut self) {
        // Framebuffer lives between 0x2400 and 0x3fff.
        // Screen is 256x224 pixels but the screen is rotated
        // 90 degrees counter-clockwise in the machine
        // so the visible screen is actually 224x256 pixels.
        // http://computerarcheology.com/Arcade/SpaceInvaders/Hardware.html

        // Game's framebuffer is loaded into an OpenGL
        // texture that is uploaded to the GPU.
        // Remap 1bpp in video memory into an 8bpp
        // Vector that will be uploaded to the GPU.

        // Calculate what part of the video buffer has
        // changed since the last frame. This means we
        // only have to upload the diff instead of the
        // entire buffer every frame.
        let (mut xmin, mut xmax) = (::std::u32::MAX, 0);
        let (mut ymin, mut ymax) = (::std::u32::MAX, 0);
        for y in 0..HEIGHT {
            for x in 0..WIDTH {
                let offset = (x * (HEIGHT / 8)) + y / 8;
                let byte = self.cpu.mem.read(0x2400 + (offset as u16));
                let p = y % 8;
                let value = if (byte & (1 << p)) != 0 {
                    (0xff, 0xff, 0xff)
                } else {
                    (0x00, 0x00, 0x00)
                };
                if self.vbuffer[y as usize][x as usize] != value {
                    self.vbuffer[y as usize][x as usize] = value;
                    if x < xmin {
                        xmin = x;
                    }
                    if x > xmax {
                        xmax = x;
                    }
                    if y < ymin {
                        ymin = y;
                    }
                    if y > ymax {
                        ymax = y;
                    }
                }
            }
        }

        if xmin < ::std::u32::MAX && ymin < ::std::u32::MAX {
            let width = xmax - xmin + 1;
            let height = ymax - ymin + 1;
            let mut pixels = Vec::with_capacity(height as usize);
            for y in ymin..ymax+1 {
                let mut row: Vec<(u8, u8, u8)> = Vec::with_capacity(width as usize);
                for x in xmin..xmax+1 {
                    let offset = (x * (HEIGHT / 8)) + y / 8;
                    let byte = self.cpu.mem.read(0x2400 + (offset as u16));
                    let p = y % 8;
                    let value = if (byte & (1 << p)) != 0 {
                        (0xff, 0xff, 0xff)
                    } else {
                        (0x00, 0x00, 0x00)
                    };
                    row.push(value);
                }
                pixels.push(row);
            }
            self.texture.write(Rect { left: xmin, bottom: ymin, width: width, height: height }, pixels);
        }

        let sampled = self.texture.sampled()
            .minify_filter(MinifySamplerFilter::Nearest)
            .magnify_filter(MagnifySamplerFilter::Nearest);
        let uniforms = uniform! {
            matrix: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0f32]
            ],
            tex: sampled
        };

        // Do the actual drawing
        let mut frame = self.window.draw();
        frame.clear_color(0.0, 0.0, 0.0, 0.0);
        frame.draw(
            &self.vertex_buffer,
            &self.index_buffer,
            &self.program,
            &uniforms,
            &Default::default()).unwrap();
        frame.finish().unwrap();
    }

    #[inline(always)]
    pub fn interrupt(&mut self, int: usize) {
        // TODO: interrupt code should be an enum?
        // PUSH PC
        let hi = ((self.cpu.pc & 0xff00) >> 8) as u8;
        let lo = (self.cpu.pc & 0xff) as u8;
        self.cpu.push(hi, lo);
        // Set PC to low memory vector
        // This is identical to a an `RST int` instruction
        self.cpu.pc = (8 * int) as u16;
        self.cpu.interrupt_enabled = false;
    }

    pub fn run(&mut self) {
        let mut i = 0usize;

        let sixtieth_of_second_ns = ((1.0f64 / 60.0f64) * 1_000_000_000.0) as u64;

        // Nanoseconds per cycle
        let ns_per_cycle = ((1.0 / (self.cpu.speed as f64)) * 1_000_000_000.0) as u32;

        let mut current_real_time = time::precise_time_ns();
        let mut last_real_time = current_real_time;
        let mut last_input_real_time = current_real_time;

        let mut last_interrupt_time_ns = self.time;

        // The next interrupt code to be used
        let mut next_int = 2;

        let mut code_time = 0;

        'main: loop {
            last_real_time = current_real_time;
            current_real_time = time::precise_time_ns();
            code_time += current_real_time - last_real_time;
            print!("{}: ", i);
            let cycles = self.step();

            self.time += (cycles * ns_per_cycle) as u64;

            i += 1;

            // Interrupt after a sixtieth of a second of simulated time
            // Since this is when it is expecting to be interrupted.
            if self.cpu.interrupt_enabled && self.time - last_interrupt_time_ns >= sixtieth_of_second_ns {
                // VBlank interrupt
                self.interrupt(next_int);
                next_int = if next_int == 2 { 1 } else { 2 };
                last_interrupt_time_ns = self.time;

                // TODO: move drawing somewhere else?
                self.draw();
            }

            // Only poll events every sixtieth of a second in real time
            // to reduce processing time of polling for events.
            if current_real_time - last_input_real_time >= sixtieth_of_second_ns {
                last_input_real_time = current_real_time;
                for event in self.window.poll_events() {
                    match event {
                        Event::Closed => break 'main,
                        Event::KeyboardInput(state, _, Some(VirtualKeyCode::Escape)) => if state == Pressed {
                            break 'main;
                        },
                        Event::KeyboardInput(state, _, Some(VirtualKeyCode::Right)) => {
                            if state == Pressed {
                                self.cpu.ports[1] |= 0x20;
                            } else {
                                self.cpu.ports[1] &= !0x20;
                            }
                        },
                        Event::KeyboardInput(state, _, Some(VirtualKeyCode::Left)) => {
                            if state == Pressed {
                                self.cpu.ports[1] |= 0x40;
                            } else {
                                self.cpu.ports[1] &= !0x40;
                            }
                        },
                        Event::KeyboardInput(state, _, Some(VirtualKeyCode::C)) => {
                            if state == Pressed {
                                self.cpu.ports[1] |= 0x01;
                            } else {
                                self.cpu.ports[1] &= !0x01;
                            }
                        }
                        // Event::KeyboardInput(state, _, Some(VirtualKeyCode::Up)) => input.up = state == Pressed,
                        // Event::KeyboardInput(state, _, Some(VirtualKeyCode::Down)) => input.down = state == Pressed,
                        _ => {}
                    }
                }
            }
        }
        println!("Average loop time: {}", (code_time as f64) / (i as f64));
    }
}