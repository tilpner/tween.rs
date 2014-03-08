#[crate_id = "tween#0.0.1"];
#[crate_type = "lib"];

use std::ptr;
use std::cast;
use std::cmp::min;
use std::clone::Clone;
use std::cell::Cell;
use std::f64::INFINITY;
use std::num::{ToPrimitive, FromPrimitive};

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

impl<'a, T> Access<'a, T> for FnAccess<T> {
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

pub mod ease {
	use std::f64::NAN;
	use std::f64::pow;
	use std::f64::consts::{PI, FRAC_PI_2};
	use std::num::{sin, cos, asin, sqrt};

	pub enum Mode {
		In,
		Out,
		InOut
	}

	pub trait Ease {
		fn ease_in(&self, t: f64) -> f64;

		fn ease_out(&self, t: f64) -> f64 {
			1.0 - self.ease_in(1.0 - t)
		}

		fn ease_in_out(&self, t: f64) -> f64 {
			if t < 0.5 {
				self.ease_in(2.0 * t) / 2.0
			} else {
				0.5 + self.ease_out(2.0 * t - 1.0) / 2.0
			}
		}

		fn ease(&self, mode: Mode, t: f64) -> f64 {
			match mode {
				In => self.ease_in(t),
				Out => self.ease_out(t),
				InOut => self.ease_in_out(t)
			}
		}
	}

	impl Ease for fn(f64) -> f64 {
		fn ease_in(&self, t: f64) -> f64 {
			(*self)(t)
		}
	}

	pub fn clamp<T: Ord>(val: T, low: T, high: T) -> T {
		if val < low {
			low
		} else if val > high {
			high
		} else {
			val
		}
	}

	struct LinearEase;

	impl Ease for LinearEase {
		fn ease_in(&self, t: f64) -> f64 {
			t
		}
		fn ease_out(&self, t: f64) -> f64 {
			t
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			t
		}
	}

	pub fn linear() -> ~Ease {
		~LinearEase as ~Ease
	}

	struct QuadEase;

	impl Ease for QuadEase {
		fn ease_in(&self, t: f64) -> f64 {
			t * t
		}
		fn ease_out(&self, t: f64) -> f64 {
			-t * (t - 2.)
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			let mut t = t;
			if {t *= 2.;t} < 1. {
				0.5 * t * t
			} else {
				-0.5 * ({t -= 1.;t} * (t - 2.) - 1.)
			}
		}
	}

	pub fn quad() -> ~Ease {
		~QuadEase as ~Ease
	}

	struct CubicEase;

	impl Ease for CubicEase {
		fn ease_in(&self, t: f64) -> f64 {
			t * t * t
		}
		fn ease_out(&self, t: f64) -> f64 {
			let s = t - 1.;
			s * s * s + 1.
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			let s = t * 2.;
			if s < 1. {
				0.5 * s * s * s
			} else {
				let u = s - 2.;
				0.5 * (u * u * u + 2.)
			}
		}
	}

	pub fn cubic() -> ~Ease {
		~CubicEase as ~Ease
	}

	struct QuartEase;

	impl Ease for QuartEase {
		fn ease_in(&self, t: f64) -> f64 {
			t * t * t * t
		}
		fn ease_out(&self, t: f64) -> f64 {
			let s = t - 1.;
			-(s * s * s * s - 1.)
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			let mut t = t;
			if {t *= 2.;t} < 1. {
				0.5 * t * t * t * t
			} else {
				-0.5 * ({t -= 2.;t} * t * t * t - 2.)
			}
		}
	}

	pub fn quart() -> ~Ease {
		~QuartEase as ~Ease
	}

	struct QuintEase;

	impl Ease for QuintEase {
		fn ease_in(&self, t: f64) -> f64 {
			t * t * t * t * t
		}
		fn ease_out(&self, t: f64) -> f64 {
			let s = t - 1.;
			s * s * s * s * s + 1.
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			let mut t = t;

			if {t *= 2.;t} < 1. {
				0.5 * t * t * t * t * t
			} else {
				 0.5 * ({t -= 2.;t} * t * t * t * t + 2.)
			}
		}
	}

	pub fn quint() -> ~Ease {
		~QuintEase as ~Ease
	}

	struct SineEase;

	impl Ease for SineEase {
		fn ease_in(&self, t: f64) -> f64 {
			-cos(t * FRAC_PI_2) + 1.
		}
		fn ease_out(&self, t: f64) -> f64 {
			sin(t * FRAC_PI_2)
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			-0.5 * (cos(PI * t) - 1.)
		}
	}

	pub fn sine() -> ~Ease {
		~SineEase as ~Ease
	}

	struct CircEase;

	impl Ease for CircEase {
		fn ease_in(&self, t: f64) -> f64 {
			-sqrt(1. - t * t) + 1.
		}

		fn ease_out(&self, t: f64) -> f64 {
			let mut t = t;
			sqrt(1. - {t -= 1.;t} * t)
		}

		fn ease_in_out(&self, t: f64) -> f64 {
			let mut t = t;
			if {t *= 2.;t} < 1. {
				-0.5 * (sqrt(1. - t * t) - 1.)
			} else {
				0.5 * (sqrt(1. - {t -= 2.;t} * t) + 1.)
			}
		}
	}

	pub fn circ() -> ~Ease {
		~CircEase as ~Ease
	}

	struct BounceEase;

	impl Ease for BounceEase {
		fn ease_in(&self, t: f64) -> f64 {
			1. - self.ease_out(1. - t)
		}
		fn ease_out(&self, t: f64) -> f64 {
			if t < 1. / 2.75 {
				7.5625 * t * t
			} else if t < 2. / 2.75 {
				let s = t - 1.5 / 2.75;
				7.5625 * s * s + 0.75
			} else if t < 2.5/2.75 {
				let s = t - 2.25 / 2.75;
				7.5625 * s * s + 0.9375
			} else {
				let s = t - 2.625 / 2.75;
				7.5625 * s * s + 0.984375
			}
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			if t < 0.5 {
				self.ease_in(t * 2.) * 0.5
			} else {
				self.ease_out(t * 2. - 1.) * 0.5 + 0.5
			}
		}
	}

	pub fn bounce() -> ~Ease {
		~BounceEase as ~Ease
	}

	struct ElasticEase {
		a: f64,
		p: f64
	}

	impl Ease for ElasticEase {
		fn ease_in(&self, t: f64) -> f64 {
			let mut t = t;
			let p = if self.p.is_nan() {0.3} else {self.p};
			let a = if self.a.is_nan() || self.a < 1. {1.} else {self.a};
			if t == 0. {return 0.;}
			if t == 1. {return 1.;}

			let s = if self.a.is_nan() || self.a < 1. {p / 4.} else {
				p / (2. * PI) * asin(1. / a)
			};

			-(a * pow(2., 10. * {t -= 1.;t}) * sin((t - s) * (2. * PI) / p))
		}
		fn ease_out(&self, t: f64) -> f64 {
			let p = if self.p.is_nan() {0.3} else {self.p};
			let a = if self.a.is_nan() || self.a < 1. {1.} else {self.a};
			if t == 0. {return 0.;}
			if t == 1. {return 1.;}

			let s = if self.a.is_nan() || self.a < 1. {p / 4.} else {
				p / (2. * PI) * asin(1. / a)
			};

			a * pow(2., -10. * t) * sin((t - s) * (2. * PI) / p) + 1.
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			let mut t = t;
			let p = if self.p.is_nan() {0.3 * 1.5} else {self.p};
			let a = if self.a.is_nan() || self.a < 1. {1.} else {self.a};
			if t == 0. {return 0.;}
			if {t *= 2.;t} == 2. {return 1.;}

			let s = if self.a.is_nan() || self.a < 1. {p / 4.} else {
				p / (2. * PI) * asin(1. / a)
			};

			if t < 1. {
				-0.5 * (a * pow(2., 10. * {t -= 1.;t}) * sin((t - s) * (2. * PI) / p))
			} else {
				a * pow(2., -10. * {t -= 1.;t}) * sin((t - s) * (2. * PI) / p) * 0.5 + 1.
			}
		}
	}

	pub fn elastic() -> ~Ease {
		~ElasticEase {
			a: NAN,
			p: NAN
		} as ~Ease
	}

	struct BackEase {
		s: f64
	}

	impl Ease for BackEase {
		fn ease_in(&self, t: f64) -> f64 {
			let s = self.s;
			t * t * ((s + 1.) * t - s)
		}
		fn ease_out(&self, t: f64) -> f64 {
			let s = self.s;
			let u = t - 1.;
			u * u * ((s + 1.) * u + s) + 1.
		}
		fn ease_in_out(&self, t: f64) -> f64 {
			let s = self.s;
			let u = t * 2.;
			if u < 1. {
				let q = s * 1.525;
				0.5 * (u * u * ((q + 1.) * u - q))
			} else {
				let r = u - 2.;
				let q = s * 1.525;
				0.5 * (r * r * ((q + 1.) * r + q) + 2.)
			}
		}
	}

	pub fn back() -> ~Ease {
		~BackEase {
			s: 1.70158
		} as ~Ease
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
