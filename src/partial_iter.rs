pub trait PartialExtremes<A>: Iterator<A> {

    fn partial_max_by<B: PartialOrd>(&mut self, f: |&A| -> B) -> Option<A> {
        self.fold(None, |max: Option<(A, B)>, x| {
            let x_val = f(&x);
            match max {
                None             => Some((x, x_val)),
                Some((y, y_val)) => if x_val > y_val {
                    Some((x, x_val))
                } else {
                    Some((y, y_val))
                }
            }
        }).map(|(x, _)| x)
    }

}

impl<T, I: Iterator<T>> PartialExtremes<T> for I {}
