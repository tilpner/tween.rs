extern crate tween;

use tween::{ rep, seq, exec, pause };
use std::cell::Cell;
use std::io::stdio::println;

fn check() {
	println("Check!");
}

fn main() {
	let mut tween = rep(seq(box [
		exec(check),
		pause(1000000.)
	]));

	while ! tween.done() {
		tween.update(0.1);
	}

}
