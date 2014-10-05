extern crate tween;

use tween::{ rep, seq, exec, pause };
use std::io::stdio::println;

fn check() {
	println("Check!");
}

fn main() {
    let mut tween = rep(seq(vec![
		exec(check),
		pause(1000000.)
	]));

	while ! tween.done() {
		tween.update(0.1);
	}

}
