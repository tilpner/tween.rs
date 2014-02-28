use std::io::stdio::println;
use tween::to;
use tween::ease;
use tween::ease::InOut;
mod tween;

fn main() {
	let mut x = 0.; // the value which is changed

	// the object that performs the change
	// (subject, target, easing, mode, duration (any unit))	
	let mut tween = to(& &mut x, 100., ease::quint(), InOut, 10.);

	while ! tween.done() {
		// advance by 1 unit
		tween.update(1.);
		println(x.to_str());
	}
}
