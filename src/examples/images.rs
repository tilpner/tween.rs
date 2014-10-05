
extern crate tween;

use tween::ease;
use tween::ease::{ Ease, Mode, In, Out, InOut};
use tween::{ Tween, to};

use std::path;
use std::io::fs::File;
use std::io::BufferedWriter;
use std::io::{Write, Truncate};
use std::cell::Cell;

pub fn clamp<T: Ord>(val: T, low: T, high: T) -> T {
    if val < low {
        low
    } else if val > high {
        high
    } else {
        val
    }
}

struct RGB {
    r: u8,
    g: u8,
    b: u8
}

impl RGB {
    fn new(_r: u8, _g: u8, _b: u8) -> RGB {
        RGB {
            r: _r,
            g: _g,
            b: _b
        }
    }

    #[inline]
    fn write(&self, w: &mut Writer) {
        let _ = w.write_u8(self.r);
        let _ = w.write_u8(self.g);
        let _ = w.write_u8(self.b);
    }
}

struct PPM {
    width: uint,
    height: uint,
    data: Vec<RGB>,
}

impl PPM {

    fn new(w: uint, h: uint) -> PPM {
        PPM {
            width: w,
            height: h,
            data: Vec::from_fn(w * h, |_| RGB::new(0, 0, 0)),
        }
    }

    fn write(&self, w: &mut Writer) {
        let header = format!("P6 {} {} 255\n", self.width, self.height);
        let _ = w.write(header.as_bytes());
        for col in self.data.iter() {
            col.write(w);
        }
    }

    #[inline(always)]
    fn set(&mut self, x: uint, y: uint, val: RGB) {
        *self.data.get_mut(self.width * y + x) = val;
    }

}

fn write_image<E: Ease>(path: &str, ease: E, mode: ease::Mode) {
    let output_path = path::Path::new(path);
    let mut output = BufferedWriter::new(File::open_mode(&output_path, Truncate, Write).unwrap());

    let w = 400;
    let h = 300;
    let mut img = PPM::new(w, h);
    let blue = RGB::new(0, 0, 255);

    let pad = 50.0f64;
    let mut x = 0.0f64;
    let y: Cell<f64> = Cell::new(pad);
    let tween = &mut to(&y, h as f64 - pad, ease, mode, w as f64);

    let step = 0.01f64; // 1 / steps per pixel

    while !tween.done() {
        x += step;
        tween.update(step);
        img.set(clamp(x as uint, 0, w - 1), clamp(y.get() as uint, 0, h - 1), blue);
    }

    img.write(&mut output);
    let _ = output.flush();
}

fn main() {
    write_image("linear_in.ppm", ease::linear(), ease::In);
    write_image("linear_out.ppm", ease::linear(), ease::Out);
    write_image("linear_inout.ppm", ease::linear(), ease::InOut);

    write_image("quad_in.ppm", ease::quad(), ease::In);
    write_image("quad_out.ppm", ease::quad(), ease::Out);
    write_image("quad_inout.ppm", ease::quad(), ease::InOut);

    write_image("cubic_in.ppm", ease::cubic(), ease::In);
    write_image("cubic_out.ppm", ease::cubic(), ease::Out);
    write_image("cubic_inout.ppm", ease::cubic(), ease::InOut);

    write_image("quart_in.ppm", ease::quart(), ease::In);
    write_image("quart_out.ppm", ease::quart(), ease::Out);
    write_image("quart_inout.ppm", ease::quart(), ease::InOut);

    write_image("quint_in.ppm", ease::quint(), ease::In);
    write_image("quint_out.ppm", ease::quint(), ease::Out);
    write_image("quint_inout.ppm", ease::quint(), ease::InOut);

    write_image("sine_in.ppm", ease::sine(), ease::In);
    write_image("sine_out.ppm", ease::sine(), ease::Out);
    write_image("sine_inout.ppm", ease::sine(), ease::InOut);

    write_image("circ_in.ppm", ease::circ(), ease::In);
    write_image("circ_out.ppm", ease::circ(), ease::Out);
    write_image("circ_inout.ppm", ease::circ(), ease::InOut);

    write_image("bounce_in.ppm", ease::bounce(), ease::In);
    write_image("bounce_out.ppm", ease::bounce(), ease::Out);
    write_image("bounce_inout.ppm", ease::bounce(), ease::InOut);

    write_image("elastic_in.ppm", ease::elastic(), ease::In);
    write_image("elastic_out.ppm", ease::elastic(), ease::Out);
    write_image("elastic_inout.ppm", ease::elastic(), ease::InOut);

    write_image("back_in.ppm", ease::back(), ease::In);
    write_image("back_out.ppm", ease::back(), ease::Out);
    write_image("back_inout.ppm", ease::back(), ease::InOut);

}
