extern crate tween;

use tween::Tween;
use tween::series;
use tween::ease;
use std::cell::Cell;

fn main() {
    let x = Cell::new(0.0f32);
    let mut tween = series(&x,
        vec![
        (0.0f32, 10., 5., ease::In),
        (10.0f32, 2., 4., ease::In)
        ], ease::linear()
    );

	while !tween.done() {
		tween.update(0.1);
        println!("{}", x);
	}
}
