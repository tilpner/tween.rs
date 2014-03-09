use std::f64::NAN; use std::f64::pow;
use std::f64::consts::{PI, FRAC_PI_2};
use std::num::{sin, cos, asin, sqrt};

pub enum Mode {
    In,
    Out,
    InOut
}

/// A trait for "easing" from one value to another. Easing is an interpolation
/// between two values, usually non-linear.
pub trait Ease {
    /// The only required method in this trait, it represents ???
    fn ease_in(&self, t: f64) -> f64;

    /// What does this do?
    fn ease_out(&self, t: f64) -> f64 {
        1.0 - self.ease_in(1.0 - t)
    }

    /// Ease both in and out, in for the first half, and out for the second
    /// half.
    fn ease_in_out(&self, t: f64) -> f64 {
        if t < 0.5 {
            self.ease_in(2.0 * t) / 2.0
        } else {
            0.5 + self.ease_out(2.0 * t - 1.0) / 2.0
        }
    }

    /// Do an ease with a given `Mode`.
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
