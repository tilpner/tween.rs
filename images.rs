mod tween;
use tween::ease;

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
	fn write(w: Writer) {
		w.write_u8(r);
		w.write_u8(g);
		w.write_u8(b);
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
			data: ~[RGB::new(100u8, 100u8, 100u8), 3 * w * h]
		}
	}

	fn write(w: &Writer) {
		let header = format!("P6 {} {} 255\n", self.width, self.height);
    		file.write(header.as_bytes());
		for col in data {
			col.write(w);
		}
	}

	#[inline(always)]
	fn set(&self, x: uint, y: uint, val: RGB) {
		self.data[x * y + x] = val;
	}


}

fn main() {
	let f = Cell::new(0.);
	let mut g = 200.;

	let mut tw = par(~[
			seq(~[from_to(&f, 0., 200., &ease::linear, InOut, 1.),
				from_to(&f, 200., 0., &ease::linear, InOut, 1.)]),
			seq(~[from_to(& &mut g, 200., 0., &ease::linear, InOut, 1.),
				from_to(& &mut g, 0., 200., &ease::linear, InOut, 1.)])
	]);
	

	let mut curr = 0.0;	
	while ! tw.done() {
		curr += 0.01f64; 
		tw.update(0.01f64);	
		println!("curr: {}, remain: {}, i: {}, g: {}", curr, tw.remaining(), f.get(), g);
	}

	let mut x = 0;
	let len = 40;
	let mut tween = to(& &mut x, 40, &ease::bounce, ease::Out, 40.);
	for y in range(0, len) {
		tween.update(1.);
		for _ in range(0, x) {
			print!(" ");
		}
		println!("x");
	}

	let output_path = path::Path::new(~"/home/till/res/img/tween.ppm");
        let output = BufferedWriter::new(File::open_mode(&output_path, Open, Write).unwrap());

	let w = 400;
	let h = 300;
	let img = PPM::new(w, h);
	let blue = RGB::new(0, 0, 255);

	let mut y = 0;
	let mut tween = to(& &mut y, h, &ease::sine, ease::InOut, w);
	
	for x in range(0, w) {
		tween.update(1.);
		img.set(x, y, blue);
	}

	img.write(output);
	output.flush();
}

#[test]
fn interpolate() {
	let i = 42;
	println!("{}", (i as f64).interp_new_to(&100f64, 0.5f64)); 
}
