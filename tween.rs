#[crate_id = "tween#0.0.1"];
#[crate_type = "lib"];

use std::ptr;
use std::cast;
use std::cmp::min;
use std::clone::Clone;
use std::cell::Cell;
use std::f64::INFINITY;
use std::num::{ToPrimitive, FromPrimitive};

pub mod ease;

trait Tweenable: Add<Self, Self> + Sub<Self, Self> + MulWithF64 + Pod {}

impl<T: Primitive + FromPrimitive + Pod> Tweenable for T  {}

trait Access<T> {
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

impl<'a, T: Pod> Access<T> for CellAccess<'a, T> {
	fn get(&self) -> T {
		self.cell.get()
	}

	fn set(&self, val: T) {
		self.cell.set(val);
	}
}

impl<T: Pod> Accessible<T> for Cell<T> {
	fn create_access(&self) -> ~Access:<T> {
		~CellAccess::new(self) as ~Access:<T>
	}
}

struct PtrAccess<T> {
	val: *mut T
}

impl<T:> PtrAccess<T> {
	fn new(new_val: &mut T) -> PtrAccess<T> {
		PtrAccess {
			val: unsafe {
				cast::transmute(new_val)
			}
		}
	}
}

impl<T:> Access<T> for PtrAccess<T> {
	fn get(&self) -> T {
		unsafe {
			ptr::read(cast::transmute_immut_unsafe(self.val))
		}
	}

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

struct FnAccess<T> {
	get: fn() -> T,
	set: fn(T)
}

impl<T> FnAccess<T> {
	fn new(_get: fn() -> T, _set: fn(T)) -> FnAccess<T> {
		FnAccess {
			get: _get,
			set: _set
		}
	}
}

impl<T> Access<T> for FnAccess<T> {
	fn get(&self) -> T {
		(self.get)()
	}

	fn set(&self, new_val: T) {
		(self.set)(new_val)
	}
}

impl<T> Accessible<T> for (fn() -> T, fn(T)) {
	fn create_access(&self) -> ~Access:<T> {
		match *self {
			(get, set) => ~FnAccess::new(get, set) as ~Access:<T>
		}
	}
}

trait MulWithF64 {
	fn mul_with_f64(&self, rhs: f64) -> Self;
}

impl<T: ToPrimitive + FromPrimitive> MulWithF64 for T {
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
	fn interp_new(&self, start: &T, end: &T, alpha: f64) -> T {
		*start + (*end - *start).mul_with_f64(alpha)
	}

	fn interp_new_to(&self, end: &T, alpha: f64) -> T {
		self.interp_new(self, end, alpha)
	}

	fn interp_apply(&mut self, start: &T, end: &T, alpha: f64) {
		*self = *start + (*end - *start).mul_with_f64(alpha);
	}
}

trait Tween {
	fn remaining(&self) -> f64;

	fn done(&self) -> bool {
		self.remaining() <= 0.0
	}

	fn reset(&mut self);

	fn update(&mut self, delta: f64) -> f64;
}

struct Single<'a, 'b, T> {
	acc: &'a Access<T>,
	start: T,
	end: T,
	current: f64,
	duration: f64,
	ease: &'b ease::Ease,
	mode: ease::Mode
}

impl<'a, 'b, T: Tweenable> Single<'a, 'b, T> {

	fn new(_acc: &'a Access<T>, _start: T, _end: T, _ease: &'b ease::Ease, _mode: ease::Mode,_duration: f64) -> Single<'a, 'b, T> {
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

	fn reset(&mut self) {
		self.current = 0f64;
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

	fn reset(&mut self) {
		self.current = 0;
		for tw in self.tweens.mut_iter() {
			tw.reset();
		}
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

	fn reset(&mut self) {
		for tw in self.tweens.mut_iter() {
			tw.reset();
		}
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

pub struct Pause {
	duration: f64,
	current: f64
}

impl Pause {
	pub fn new(_duration: f64) -> Pause {
		Pause {duration: _duration, current: 0f64}
	}
}

impl Tween for Pause {
	fn remaining(&self) -> f64 {
		self.duration - self.current
	}

	fn reset(&mut self) {
		self.current = 0f64;
	}

	fn update(&mut self, delta: f64) -> f64 {
		let remain = self.remaining();
		self.current += min(remain, delta);
		-remain
	}

}

struct Exec {
	content: fn(),
	executed: bool
}

impl Exec {
	fn new(_content: fn()) -> Exec {
		Exec {content: _content, executed: false}
	}
}

impl Tween for Exec {
	fn remaining(&self) -> f64 {0.}
	fn done(&self) -> bool {self.executed}
	fn reset(&mut self) {self.executed = false;}
	fn update(&mut self, delta: f64) -> f64 {
		(self.content)();
		self.executed = true;
		delta // Exec consumes no time
	}
}

pub struct Repeat {
	tween: ~Tween:,
}

impl Repeat {
	pub fn new(_tween: ~Tween:) -> Repeat {
		Repeat {
			tween: _tween,
		}
	}
}

impl Tween for Repeat {

	fn remaining(&self) -> f64 {
		INFINITY
	}

	fn reset(&mut self) {
		self.tween.reset();
	}

	fn update(&mut self, delta: f64) -> f64 {
		let mut remain = delta;
		while remain > 0. {
			let rest = self.tween.update(remain);
			if rest >= 0. {
				self.tween.reset();
			} else {
				// negative rest means: current cycle still running
				remain += rest;
			}
		}
		0. // It can always continue, so there is no rest
	}

}

pub fn from_to<'a, 'b, T: Tweenable + ToStr + 'static>(val: &'a Accessible<T>, start: T, end: T, ease: &'b ease::Ease, mode: ease::Mode, duration: f64) -> ~Tween: {
	~Single::new(val.create_access(), start, end, ease, mode, duration) as ~Tween:
}

pub fn to<'a, 'b, T: Tweenable + Clone + ToStr + 'static>(val: &'a Accessible<T>, end: T, ease: &'b ease::Ease, mode: ease::Mode, duration: f64) -> ~Tween: {
	from_to(val, val.create_access().get(), end, ease, mode, duration)
}

pub fn from<'a, 'b, T: Tweenable + Clone + ToStr + 'static>(val: &'a Accessible<T>, start: T, ease: &'b ease::Ease, mode: ease::Mode, duration: f64) -> ~Tween: {
	from_to(val, start, val.create_access().get(), ease, mode, duration)
}

pub fn seq<'a>(_tweens: ~[~Tween:]) -> ~Tween: {
	~Sequence::<'a>::new(_tweens) as ~Tween:
}

pub fn par<'a>(_tweens: ~[~Tween:]) -> ~Tween: {
	~Parallel::<'a>::new(_tweens) as ~Tween:
}

pub fn exec(content: fn()) -> ~Tween: {
	~Exec::new(content) as ~Tween:
}
