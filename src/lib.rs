#![crate_name = "redox-tween"]
#![crate_type = "lib"]

#![feature(associated_types)]

use std::ptr;
use std::cmp;
use std::num::cast;
use std::cmp::{max};
use std::clone::Clone;
use std::cell::Cell;
use std::f64::INFINITY;
use std::num::{ToPrimitive, FromPrimitive};
use std::mem;

use partial_iter::PartialExtremes;

use ease::Ease;

pub mod partial_iter;
pub mod ease;

/// Any data that can be interpolated by this library.
pub trait Tweenable: Add<Self, Self> + Sub<Self, Self> + MulWithF64 + Float + FloatMath {}

/// A mutable property which is passed to the tweens.
/// Chosen because hardcoding access ways is inflexible.
pub trait Access<T> {
    fn get(&self) -> T;
    fn set(&self, val: T);
}

/// Any data that can be accessed.
pub trait Accessible<'a, T, A: Access<T>> {
    fn create_access(&'a self) -> A;
}

/// A single part of a tween tree.
/// Can do almost anything, examples currently implemented are
/// `Single`, `Multi`, `Sequence`, `Parallel`, `Pause` and `Exec`.
pub trait Tween {
    /// The amount of time remaining in this tween. Passing this value to
    /// `update` should make `done` return true
    #[inline]
    fn remaining(&self) -> f64;

    /// Check if the tween has completed
    #[inline]
    fn done(&self) -> bool {
        self.remaining() <= 0.0
    }

    /// Reset the tween
    #[inline]
    fn reset(&mut self);

    /// Update the tween, after `delta` time has passed
    #[inline]
    fn update(&mut self, delta: f64) -> f64;
}

/// Scalar multiplication of the value with an `f64`.
/// For a vector type, you would want to multiply all its elements with this `f64`.
trait MulWithF64 {
    /// Do a scalar multiplication with `rhs`
    #[inline]
    fn mul_with_f64(&self, rhs: f64) -> Self;
}

/// Linear interpolation between two values
pub trait Lerp<T> {
    /// Linearly interpolate between `start` and `end`.
    /// For numbers this could look like:
    /// ```rust
    /// fn lerp(&self, start: &f64, end: &f64, alpha: f64) -> f64 {
    ///     *start + (*end - *start) * alpha
    /// }
    /// ```
    #[inline]
    fn lerp(&self, start: &T, end: &T, alpha: f64) -> T;
}

impl<T: Tweenable> Lerp<T> for T {
    #[inline]
    fn lerp(&self, start: &T, end: &T, alpha: f64) -> T {
        *start + (*end - *start).mul_with_f64(alpha)
    }
}

/// Allow access/tweening via a Cell<T>
struct CellAccess<'a, T: 'a> {
    cell: &'a Cell<T>
}

impl<'a, T> CellAccess<'a, T> {
    #[inline]
    fn new(val: &'a Cell<T>) -> CellAccess<'a, T> {
        CellAccess {
            cell: val
        }
    }
}

impl<'a, T: Copy> Access<T> for CellAccess<'a, T> {
    #[inline]
    fn get(&self) -> T {
        self.cell.get()
    }

    #[inline]
    fn set(&self, val: T) {
        self.cell.set(val);
    }
}

impl<'a, T: Copy> Accessible<'a, T, CellAccess<'a, T>> for Cell<T> {
    #[inline]
    fn create_access(&'a self) -> CellAccess<'a, T> {
        CellAccess::new(self)
    }
}

/// Unsafe access/tweening via mutable raw pointers.
/// Added to minimize changes to your preexisting model.
/// If you can, please use the `Cell<T>` alternative.
struct PtrAccess<T> {
    val: *mut T
}

impl<T:> PtrAccess<T> {
    #[inline]
    fn new(new_val: &mut T) -> PtrAccess<T> {
        PtrAccess {
            val: unsafe {
                     mem::transmute(new_val)
            }
        }
    }
}

impl<T:> Access<T> for PtrAccess<T> {
    #[inline]
    fn get(&self) -> T {
        unsafe {
            ptr::read(mem::transmute(self.val))
        }
    }

    #[inline]
    fn set(&self, new_val: T) {
        unsafe {
            *self.val = new_val
        }
    }
}

impl<'a, T> Accessible<'a, T, PtrAccess<T>> for &'a mut T {
    #[inline]
    fn create_access(&'a self) -> PtrAccess<T> {
        unsafe {
            PtrAccess::<T>::new(mem::transmute(*self))
        }
    }
}

/// Access to anything that can't be done via the other two access modes
/// via callback functions to do what you want.
/// Also sensible if you want to avoid polling the value, but get direct
/// event-callbacks.
struct FnAccess<T, F: Fn<(), T>, G: Fn<T, ()>> {
    get: F,
    set: G
}

impl<T, F: Fn<(), T>, G: Fn<T, ()>> FnAccess<T, F, G> {
    #[inline]
    fn new(get: F, set: G) -> FnAccess<T, F, G> {
        FnAccess {
            get: get,
            set: set
        }
    }
}

impl<T, F: Fn<(), T>, G: Fn<T, ()>> Access<T> for FnAccess<T, F, G> {
    #[inline]
    fn get(&self) -> T {
        self.get()
    }

    #[inline]
    fn set(&self, new_val: T) {
        self.set(new_val)
    }
}

impl<'a, T: Copy, F: Fn<(), T>, G: Fn<T, ()>> Accessible<'a, T, FnAccess<T, F, G>> for (F, G) {
    #[inline]
    fn create_access(&self) -> FnAccess<T, F, G> {
        match *self {
            (get, set) => FnAccess::new(get, set)
        }
    }
}

impl<T: Primitive + FromPrimitive + FloatMath> Tweenable for T  {}


impl<T: ToPrimitive + FromPrimitive> MulWithF64 for T {
    #[inline]
    fn mul_with_f64(&self, rhs: f64) -> T {
        FromPrimitive::from_f64(self.to_f64().unwrap() * rhs).unwrap()
    }
}

/// A single tween, interpolating a value between two bounds
pub struct Single<'a, T, A: Access<T>, E: Ease> {
    acc: A,
    start: T,
    end: T,
    current: f64,
    duration: f64,
    ease: E,
    mode: ease::Mode
}

impl<'a, T: Tweenable, A: Access<T>, E: Ease> Single<'a, T, A, E> {
    fn new(acc: A, start: T, end: T, ease: E, mode: ease::Mode, duration: f64) -> Single<'a, T, A, E> {
        Single {
            acc: acc,
            start: start,
            end: end,
            current: 0f64,
            duration: duration,
            ease: ease,
            mode: mode
        }
    }

}

impl<'a, T: Tweenable + 'static, A: Access<T>, E: Ease> Tween for Single<'a, T, A, E> {
    fn remaining(&self) -> f64 {
        self.duration - self.current
    }

    fn reset(&mut self) {
        self.current = 0f64;
    }

    fn update(&mut self, delta: f64) -> f64 {
        let t = self.current / self.duration;
        let a = self.ease.ease(self.mode, t);
        self.acc.set(self.acc.get().lerp(&self.start, &self.end, a));
        let remain = self.remaining();
        self.current += cmp::partial_min(remain, delta).unwrap();
        -remain
    }
}

/// Interpolate between a series of data points.
/// This could be done less efficiently for `n`
/// data points with `n - 1` `Single` tweens.
pub struct Multi<'a, 'b, T> {
    acc: &'a Access<T> + 'a,
    data: Box<[(T, T, f64, Box<ease::Ease + 'static>, ease::Mode)]>,
    current: uint,
    current_time: f64 // is in user-defined duration, not [0;1]
}

impl<'a, 'b, T: Tweenable> Multi<'a, 'b, T> {
    fn new(acc: &'a Access<T>, data: Box<[(T, T, f64, Box<ease::Ease + 'static>, ease::Mode)]>) -> Multi<'a, 'b, T> {
        Multi {
            acc: acc,
            data: data,
            current: 0,
            current_time: 0.
        }
    }
}

impl <'a, 'b, T: Tweenable> Tween for Multi<'a, 'b, T> {
    fn remaining(&self) -> f64 {
        self.data.iter().skip(self.current).map(|&(_, _, b, _, _)| b).fold(0., |a, b| a + b) - self.current_time
    }

    fn reset(&mut self) {
        self.current = 0;
        self.current_time = 0.;
    }

    fn update(&mut self, mut delta: f64) -> f64 {
        let (_, _, dur, _, _) = self.data[self.current];
        //delta /= dur; // normalize from duration to [0;1]
        self.current_time += delta;

        // wrap time around till between bounds
        loop {
            let (_, _, dur, _, _) = self.data[self.current];
            if self.current_time - dur > 0. {
                self.current_time -= dur;
                self.current += 1;
            } else {
                break;
            }
        }

        let (start, end, dur, ref ease, mode) = self.data[self.current];
        let a = ease.ease(mode, self.current_time / dur);
        self.acc.set(self.acc.get().lerp(&start, &end, a));
        delta
    }

}

/// A tween that runs other tweens to completion, in order.
/// It will switch to the next tween in the vector once the current tween
/// returns `true` when `done` is called.
pub struct Sequence {
    tweens: Box<[Box<Tween + 'static>]>,
    current: uint
}

impl Sequence {
    fn new(tweens: Box<[Box<Tween + 'static>]>) -> Sequence {
        Sequence {
            tweens: tweens,
            current: 0u
        }
    }
}

impl Tween for Sequence {
    fn remaining(&self) -> f64 {
        self.tweens.iter().fold(0f64, |a, b| a + b.remaining())
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
            remain = self.tweens[self.current].update(remain);
            if self.tweens[self.current].done() {
                self.current += 1;
            }
        }
        remain
    }
}

/// A tween that updates many tweens simultaneously.
/// If this tween is updated, all it's child tweens are
/// updated by the same amount of time, if they haven't yet
/// finished.
pub struct Parallel {
    tweens: Box<[Box<Tween + 'static>]>
}

impl Parallel {
    fn new(tweens: Box<[Box<Tween + 'static>]>) -> Parallel {
        Parallel {
            tweens: tweens
        }
    }
}

impl Tween for Parallel {
    /// The max remaining time of all wrapped tweens
    fn remaining(&self) -> f64 {
        self.tweens.iter().partial_max_by(|&a| a.remaining()).unwrap().remaining()
    }

    /// Reset every wrapped tween.
    fn reset(&mut self) {
        for tw in self.tweens.mut_iter() {
            tw.reset();
        }
    }

    fn update(&mut self, delta: f64) -> f64 {
        let mut max_remain = 0f64;
        for tw in self.tweens.mut_iter() {
            let remain = tw.remaining();
            if remain > max_remain { max_remain = remain; }
            tw.update(cmp::partial_min(remain, delta).unwrap());
        }
        max_remain - delta
    }
}

/// A tween that simply does nothing for a period of time.
/// Can be used to delay a tween.
pub struct Pause {
    duration: f64,
    current: f64
}

impl Pause {
    pub fn new(duration: f64) -> Pause {
        Pause {duration: duration, current: 0f64}
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
        self.current += cmp::partial_min(remain, delta).unwrap();
        -remain
    }
}

/// A tween that executes a function when it is updated.
/// It consumes no time. If you need that, use the `Pause` tween.
pub struct Exec {
    content: fn(),
    executed: bool
}

impl Exec {
    fn new(content: fn()) -> Exec {
        Exec {content: content, executed: false}
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

/// Repeat a given tween forever.
pub struct Repeat {
    tween: Box<Tween + 'static>
}

impl Repeat {
    pub fn new(tween: Box<Tween + 'static>) -> Repeat {
        Repeat {
            tween: tween
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

/// Reverses a given tween.
/// Note that this is less powerful than reversing the tween by hand,
/// because it does not support changing durations of the tween.
pub struct Reverse {
    tween: Box<Tween + 'static>,
    current: f64,
    duration: f64
}

impl Reverse {
    pub fn new(mut tween: Box<Tween + 'static>) -> Reverse {
        let rem = tween.remaining();
        tween.update(rem);
        Reverse {
            tween: tween,
            current: 0.,
            duration: rem
        }
    }
}

impl Tween for Reverse {
    fn remaining(&self) -> f64 {
        self.duration - self.current
    }

    fn reset(&mut self) {
        self.current = 0.;
    }

    fn update(&mut self, delta: f64) -> f64 {
        self.tween.update(-delta)
    }
}


pub fn from_to<'a, T: Tweenable + 'static, A: Access<T>, E: Ease + Sized>(val: &'a Accessible<T, A>, start: T, end: T, ease: E, mode: ease::Mode, duration: f64) -> Single<'a, T, A, E> {
    Single::new(val.create_access(), start, end, ease, mode, duration)
}

pub fn to<'a, T: Tweenable + Clone + 'static, A: Access<T>, E: Ease + Sized>(val: &'a Accessible<T, A>, end: T, ease: E, mode: ease::Mode, duration: f64) -> Single<'a, T, A, E> {
    from_to(val, val.create_access().get(), end, ease, mode, duration)
}

pub fn from<'a, T: Tweenable + Clone + 'static, A: Access<T>, E: Ease + Sized>(val: &'a Accessible<T, A>, start: T, ease: E, mode: ease::Mode, duration: f64) -> Single<'a, T, A, E> {
    from_to(val, start, val.create_access().get(), ease, mode, duration)
}

pub fn series<'a, T: Tweenable + 'static, A: Access<T>>(val: &'a Accessible<T, A>, data: Box<[(T, T, f64, Box<ease::Ease + 'static>, ease::Mode)]>) -> Multi<'a, 'a, T> {
    Multi::new(val.create_access(), data)
}

pub fn seq<'a>(_tweens: Box<[Box<Tween + 'static>]>) -> Box<Tween + 'static> {
    box Sequence::new(_tweens) as Box<Tween + 'static>
}

pub fn par<'a>(_tweens: Box<[Box<Tween + 'static>]>) -> Box<Tween + 'static> {
    box Parallel::new(_tweens) as Box<Tween + 'static>
}

pub fn exec(content: fn()) -> Box<Tween + 'static> {
    box Exec::new(content) as Box<Tween + 'static>
}

pub fn pause(time: f64) -> Box<Tween + 'static> {
    box Pause::new(time) as Box<Tween + 'static>
}

pub fn rep(tween: Box<Tween + 'static>) -> Box<Tween + 'static> {
    box Repeat::new(tween) as Box<Tween + 'static>
}

pub fn rev(tween: Box<Tween + 'static>) -> Box<Tween + 'static> {
    box Reverse::new(tween) as Box<Tween + 'static>
}

pub fn yoyo(tween: Box<Tween + 'static>) -> Box<Tween + 'static> {
    rep(seq(box [tween, rev(tween)]))
}

pub fn delay(tw: Box<Tween + 'static>, time: f64) -> Box<Tween + 'static> {
    seq(box [pause(time), tw])
}

