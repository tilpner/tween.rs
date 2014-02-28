use tween::{from_to, rep, seq, exec, pause};
use std::cell::Cell;
use std::io::stdio::println;
mod tween;

fn check() {
	println("Check!");
}

fn main() {
	let x = Cell::new(0);
	let mut tween = rep(seq(~[
		exec(check),
		pause(1000000.)
	]));

	while ! tween.done() {
		tween.update(0.1);
	}

}
