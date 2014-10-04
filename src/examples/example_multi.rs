extern crate tween;

use tween::Tween;
use tween::series;
use tween::ease;
use std::cell::Cell;

fn main() {
	let x = Cell::new(0.0f32);
	let mut tween = series(&x,
       box [
        (0., 10., 5., ease::sine(), ease::In),
        (10., 2., 4., ease::quart(), ease::In)
       ]
    );

	while !tween.done() {
		tween.update(0.1);
        println!("{}", x.get());
	}

}
