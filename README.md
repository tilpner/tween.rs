redox-tween 
===========

## Introduction

Games and other applications often have a need to change values smoothly, without the need for a fully-blown physics system. This is where [Tweening][wikipedia] (Inbetweening) comes in. It allows to interpolate between numerical values in a lightweight way. 

[This YouTube video][youtube-juice] is an interesting example of how the addition of animations (via tweens) can change the feeling of a game. 

[wikipedia]: http://en.wikipedia.org/wiki/Inbetweening
[youtube-juice]: http://www.youtube.com/watch?v=Fy0aCDmgnxg

## Example
```Rust
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
```
