
extern crate std;
use tween::ease;
use ease::{InOut};
use tween::{from_to, from, to, seq, par}; 
use std::io::BufferedWriter;
use std::io::fs::File;
use std::path;
use std::cell::Cell;
use std::io::{Open, Read, Write, Truncate};
use std::vec;
mod tween;

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
		w.write_u8(self.r);
		w.write_u8(self.g);
		w.write_u8(self.b);
	}
}

struct PPM {
	width: uint,
	height: uint,
	data: ~[RGB],
	pos: uint
}

impl PPM {

	fn new(w: uint, h: uint) -> PPM {
		PPM {
			width: w,
			height: h,
			data: vec::from_fn(w * h, |i| RGB::new(0, 0, 0)),
			pos: 0
		}
	}

	fn write(&self, w: &mut Writer) {
		let header = format!("P6 {} {} 255\n", self.width, self.height);
    		w.write(header.as_bytes());
		for col in self.data.iter() {
			col.write(w);
		}
	}

	#[inline(always)]
	fn set(&mut self, x: uint, y: uint, val: RGB) {
		self.data[self.width * y + x] = val;
	}


}

fn main() {
	let base_path = ~"/tmp/{1}_{2}.ppm";
	let eases = ~[ease::linear(), ease::sine(), ease::quad(), ease::cubic(), ease::quart(), ease::quint(), ease::back(), ease::elastic(), ease::bounce(), ease::circ()];
	let ease_names = ~[~"linear", ~"sine", ~"quad", ~"cubic", ~"quart", ~"quint", ~"back", ~"elastic", ~"bounce", ~"circ"];
	let modes = [ease::In, ease::Out, ease::InOut];
	let mode_names = ~[~"In", ~"Out", ~"InOut"];
	
	for (ease, ease_name) in eases.iter().zip(ease_names.iter()) {
		for (mode, mode_name) in modes.iter().zip(mode_names.iter()) {
		write_image(
			std::str::replace(std::str::replace(base_path, "{1}", *ease_name), "{2}", *mode_name), *ease, *mode);
		}
	}
}

fn write_image(path: &str, ease: &ease::Ease, mode: ease::Mode) {	
	let output_path = path::Path::new(path);
        let mut output = BufferedWriter::new(File::open_mode(&output_path, Truncate, Write).unwrap());

	let w = 400;
	let h = 300;
	let mut img = PPM::new(w, h);
	let blue = RGB::new(0, 0, 255);

	let pad = 50;
	let mut x = 0.;
	let mut y: uint = pad;
	let mut tween = to(& &mut y, h - pad, ease, mode, w as f64);

	let step = 0.01; // 1 / steps per pixel
	
	while ! tween.done() {
		x += step;
		tween.update(step);
		img.set(tween::ease::clamp(x as uint, 0, w - 1), tween::ease::clamp(y as uint, 0, h - 1), blue);
	}

	img.write(&mut output);
	output.flush();

}
