use tween::series;
use tween::ease;
use ease::{Mode};
use std::cell::Cell;
use std::io::stdio::println;
#[path = "../lib.rs"] mod tween;

fn main() {
	let x = Cell::new(0.);
	let mut tween = series(&x,
       ~[
        (0., 10., 5., ease::sine(), ease::In),
        (10., 2., 4., ease::quart(), ease::In)
       ] 
    );

	while ! tween.done() {
		tween.update(0.1);
        println(x.get().to_str());
	}

}
