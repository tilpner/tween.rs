// Thanks to eddyb, cmr, bjz, kimundi, mindslight, hoverbear, and others from mozilla/#rust !

#[crate_id = "redox-tween#0.1"];
#[crate_type = "lib"];

use std::ptr;
use std::cast;
use std::cmp::{max, min};
use std::clone::Clone;
use std::cell::Cell;
use std::io::BufferedWriter;
use std::io::fs::File;
use std::io::FileMode::{Open};
use std::io::FileAccess::{Write};
use ease::InOut;
use ease::Ease;

trait Tweenable: Add<Self, Self> + Sub<Self, Self> + MulWithF64 + Pod {}

impl<T: Primitive + FromPrimitive + Pod> Tweenable for T  {}

trait Access<'a, T> {
	fn get(&self) -> T;
	fn set(&self, val: T);
}

trait Accessible<T> {
	fn create_access(&self) -> ~Access:<T>;
}

struct CellAccess<'a, T> {
	cell: &'a Cell<T>
}

impl<'a, T: Pod> CellAccess<'a, T> {
	fn new(val: &'a Cell<T>) -> CellAccess<'a, T> {
		CellAccess {
			cell: val
		}
	}
}

impl<'a, T: Pod> Access<'a, T> for CellAccess<'a, T> {
	#[inline(always)]
	fn get(&self) -> T {
		self.cell.get()
	}

	#[inline(always)]	
	fn set(&self, val: T) {
		self.cell.set(val);
	}
}

impl<T: Pod> Accessible<T> for Cell<T> {
	fn create_access(&self) -> ~Access:<T> {
		~CellAccess::new(self) as ~Access:<T>
	}
}

struct PtrAccess<'a, T> {
	val: *mut T
}

impl<'a, T:> PtrAccess<'a, T> {

	fn new(new_val: &'a mut T) -> PtrAccess<'a, T> {
		PtrAccess {
			val: unsafe {
				cast::transmute(new_val)
			}
		}
	}

}

impl<'a, T:> Access<'a, T> for PtrAccess<'a, T> {
	#[inline(always)]
	fn get(&self) -> T {
		unsafe {
			ptr::read(cast::transmute_immut_unsafe(self.val))	
		}
	} 

	#[inline(always)]
	fn set(&self, new_val: T) {
		unsafe {		
			*self.val = new_val
		}
	}	
}

impl<'a, T> Accessible<T> for &'a mut T {
	fn create_access(&self) -> ~Access:<T> {
		unsafe {	
			~PtrAccess::<T>::new(cast::transmute_mut(*self)) as ~Access:<T>
		}
	}
}

trait MulWithF64 {
	fn mul_with_f64(&self, rhs: f64) -> Self;
}

impl<T: std::num::ToPrimitive + std::num::FromPrimitive> MulWithF64 for T {
	fn mul_with_f64(&self, rhs: f64) -> T {
		FromPrimitive::from_f64(self.to_f64().unwrap() * rhs).unwrap()
	}
}

trait Interpolation<T> {
	fn interp_new(&self, start: &T, end: &T, alpha: f64) -> T;

	fn interp_new_to(&self, end: &T, alpha: f64) -> T;

	fn interp_apply(&mut self, start: &T, end: &T, alpha: f64);
}

impl<T: Tweenable> Interpolation<T> for T {
	
	#[inline]
	fn interp_new(&self, start: &T, end: &T, alpha: f64) -> T {
		*start + (*end - *start).mul_with_f64(alpha)
	}

	#[inline]
	fn interp_new_to(&self, end: &T, alpha: f64) -> T {
		self.interp_new(self, end, alpha)
	}

	#[inline]
	fn interp_apply(&mut self, start: &T, end: &T, alpha: f64) {
		*self = *start + (*end - *start).mul_with_f64(alpha);
	}

}

/*mod spec {
	use super::{Tweenable, Accessible, Tween, Single, Pause, Sequence, Parallel};

	pub enum Spec<'a, T> {
		FromTo(&'a Accessible<T>, T, T, fn(f64) -> f64, f64),
		Pause(f64),
		Seq(~[Spec<'a, T>]),
		Par(~[Spec<'a, T>])
	}

	fn collect<'a, T: Tweenable + ToStr>(specs: ~[Spec<'a, T>]) -> ~[~Tween:] {
		let mut iter = specs.move_iter();
		iter.map(|a| create(a)).collect()
	}

	pub fn create<'a, T: Tweenable + ToStr>(spec: Spec<'a, T>) -> ~Tween: {
		match spec {
			FromTo(acc, start, end, ease, duration) => ~Single::new(acc.create_access(), start, end, ease, duration) as ~Tween:,
			Pause(duration) => ~Pause::new(duration) as ~Tween:,
			Seq(specs) => ~Sequence::new(collect(specs)) as ~Tween:,
			Par(specs) => ~Parallel::new(collect(specs)) as ~Tween:
		}
	}

}*/

mod ease {
	use std::f64::consts::{PI, FRAC_PI_2, FRAC_PI_4};
	use std::f64::pow;
	use std::num::{sin, cos, sqrt};

	pub enum Mode {
		In,
		Out,
		InOut
	}

	pub trait Ease {
		#[inline]
		fn ease_in(&self, t: f64) -> f64;
		
		#[inline]
		fn ease_out(&self, t: f64) -> f64 {
			1.0 - self.ease_in(1.0 - t)
		}

		#[inline]
		fn ease_in_out(&self, t: f64) -> f64 {
			if t < 0.5 {
				self.ease_in(2.0 * t) / 2.0
			} else {
				0.5 + self.ease_out(2.0 * t - 1.0) / 2.0
			}
		}

		#[inline]
		fn ease(&self, mode: Mode, t: f64) -> f64 {
			match mode {
				In => self.ease_in(t),
				Out => self.ease_out(t),
				InOut => self.ease_in_out(t)
			}
		}
	}

	impl Ease for fn(f64) -> f64 {
		#[inline(always)]
		fn ease_in(&self, t: f64) -> f64 {
			(*self)(t)
		}
	}
	
	fn clamp<T: Ord>(val: T, low: T, high: T) -> T {
		if val < low {
			low
		} else if val > high {
			high
		} else {
			val
		}
	}

	#[inline(always)]
	fn square<T: Mul<T, T>>(a: T) -> T {
		a * a
	}

	pub fn linear(t: f64) -> f64 {
		t
	}

	pub fn fade(t: f64) -> f64 {
		clamp(t * t * t * (t * (t * 6f64 - 15f64) + 10f64), 0f64, 1f64)
	}

	pub fn sine(t: f64) -> f64 {
		1f64 - cos(t * FRAC_PI_2)
	}

	pub fn circ(t: f64) -> f64 {
		1f64 - sqrt(1f64 - t * t)
	}

	pub fn bounce(t: f64) -> f64 {
		if 1.0 - t < 1.0 / 2.75 {
			1.0 - (7.5625 * square(1.0 - t))
		} else if 1.0 - t < 2.0 / 2.75 {
			1.0 - (7.5625 * square(1.0 - t - 1.5 / 2.75) + 0.75) 
		} else if 1.0 - t < 2.5 / 2.75 {
			1.0 - (7.5625 * square(1.0 - t - 2.25 / 2.75) + 0.9375)
		} else {
			1.0 - (7.5625 * square(1.0 - t - 2.625 / 2.75) + 0.984375)
		}
	}

	pub fn elastic(t: f64) -> f64 {
		pow(-2.0, 10.0 * (t - 1.0)) * sin(((t - 1.0) - 0.3 / 4.0) * 2.0 * FRAC_PI_4)
	}

}

trait Tween {	
	fn remaining(&self) -> f64;
	
	fn done(&self) -> bool {
		self.remaining() <= 0.0
	}
	
	fn update(&mut self, delta: f64) -> f64;
}

struct Single<'a, 'b, T> {
	acc: &'a Access<'a, T>,
	start: T,
	end: T,
	current: f64,
	duration: f64,
	ease: &'b ease::Ease,
	mode: ease::Mode
}

impl<'a, 'b, T: Tweenable> Single<'a, 'b, T> {

	fn new(_acc: &'a Access<'a, T>, _start: T, _end: T, _ease: &'b ease::Ease, _mode: ease::Mode,_duration: f64) -> Single<'a, 'b, T> {
		Single {
			acc: _acc,
			start: _start,
			end: _end,
			current: 0f64,
			duration: _duration,
			ease: _ease,
			mode: _mode
		}
	}

}

impl<'a, 'b, T: Tweenable + ToStr + 'static> Tween for Single<'a, 'b, T> {

	fn remaining(&self) -> f64 {
		self.duration - self.current
	}

	fn update(&mut self, delta: f64) -> f64 {
		let t = self.current / self.duration;
		let a = self.ease.ease(self.mode, t);
		self.acc.set(self.acc.get().interp_new(&self.start, &self.end, a));	
		let remain = self.remaining();
		self.current += min(remain, delta);
		-remain
	}

}

struct Sequence<'a> {
	tweens: ~[~Tween:],
	current: uint
}

impl<'a> Sequence<'a> {

	fn new(_tweens: ~[~Tween:]) -> Sequence<'a> {
		Sequence {
			tweens: _tweens,
			current: 0u
		}
	}

}

impl<'a> Tween for Sequence<'a> {
	
	/*fn done(&self) -> bool {
		self.current >= self.tweens.len() || (self.current + 1 == self.tweens.len() && self.tweens[self.current].done())
	}*/

	fn remaining(&self) -> f64 {
		self.tweens.iter()/*.skip(self.current - 1)*/.fold(0f64, |a, b| a + b.remaining())
	}

	fn update(&mut self, delta: f64) -> f64 {
		let mut remain = delta;
		while remain > 0f64 && self.current < self.tweens.len() {
			//println!("seq::remain: {}", remain);
			remain = self.tweens[self.current].update(remain);
			if self.tweens[self.current].done() {
				self.current += 1;
			}
		}
		remain
	}

}

struct Parallel<'a> {
	tweens: ~[~Tween:]
}

impl<'a> Parallel<'a> {

	fn new(_tweens: ~[~Tween:]) -> Parallel<'a> {
		Parallel {
			tweens: _tweens
		}
	}

}

impl<'a> Tween for Parallel<'a> {
	
	fn remaining(&self) -> f64 {
		self.tweens.iter().max_by(|&a| a.remaining()).unwrap().remaining()
	}

	fn update(&mut self, delta: f64) -> f64 {
		let mut max_remain = 0f64;
		for tw in self.tweens.mut_iter() {
			let remain = tw.remaining();
			if remain > max_remain {max_remain = remain;}
			//println!("par::remain: {}, delta: {}, min: {}", remain, delta, min(remain, delta));
			tw.update(min(remain, delta));
		}
		max_remain - delta
	}

}

struct Pause {
	duration: f64,
	current: f64
}

impl Pause {
	fn new(_duration: f64) -> Pause {
		Pause {duration: _duration, current: 0f64}
	}
}	

impl Tween for Pause {
	
	fn remaining(&self) -> f64 {
		self.duration - self.current
	}

	fn update(&mut self, delta: f64) -> f64 {
		let remain = self.remaining();
		self.current += min(remain, delta);
		-remain
	}

}

pub enum RepeatMode {
	Reset,
	Yoyo
}

struct Repeat {
	tween: ~Tween:,
	mode: RepeatMode
}

impl Repeat {
	fn new(_tween: ~Tween:, _mode: RepeatMode) -> Repeat {
		Repeat {
			tween: _tween,
			mode: _mode
		}
	}	
}

impl<'a, T: Tweenable> Tween for Repeat {

	fn remaining(&self) -> f64 {
		std::f64::INFINITY
	}

	fn update(&self, delta: f64) -> f64 {
		let mut remain = delta;
		while remain > 0. {
			let rest = self.tween.update(remain);
			if rest <= 0. {
				match self.mode {
					Reset => self.tween.reset(),
					Yoyo => self.direction *= -1
				}
			}
		}
		0. // It can always continue, so there is no rest
	}

}

fn from_to<'a, 'b, T: Tweenable + ToStr + 'static>(val: &'a Accessible<T>, start: T, end: T, ease: &'b ease::Ease, mode: ease::Mode, duration: f64) -> ~Tween: {
	~Single::new(val.create_access(), start, end, ease, mode, duration) as ~Tween:
}

fn to<'a, 'b, T: Tweenable + Clone + ToStr + 'static>(val: &'a Accessible<T>, end: T, ease: &'b ease::Ease, mode: ease::Mode, duration: f64) -> ~Tween: {
	from_to(val, val.create_access().get(), end, ease, mode, duration)
}

fn from<'a, 'b, T: Tweenable + Clone + ToStr + 'static>(val: &'a Accessible<T>, start: T, ease: &'b ease::Ease, mode: ease::Mode, duration: f64) -> ~Tween: {
	from_to(val, start, val.create_access().get(), ease, mode, duration)
}

fn seq<'a>(_tweens: ~[~Tween:]) -> ~Tween: {
	~Sequence::<'a>::new(_tweens) as ~Tween:
}

fn par<'a>(_tweens: ~[~Tween:]) -> ~Tween: {
	~Parallel::<'a>::new(_tweens) as ~Tween:
}
