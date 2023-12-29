#![allow(dead_code)]

use swipl::prelude::*;

use std::ops::Range;
use std::rc::Rc;
use std::cell::UnsafeCell;

#[derive(Clone)]
pub struct Axis {
    start: isize,
    end: isize,
    offset: isize,
    scale: usize
}

impl Axis {
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    pub fn contains(&self, x: isize) -> bool {
        if x >= self.end {
            false
        } else if x < self.start {
            false
        } else {
            true
        }
    }
}

fn intersect_range<T: Ord + Copy>(a: &Range<T>, b: &Range<T>) -> Range<T> {
    std::cmp::max(a.start, b.start) .. std::cmp::min(a.end, b.end)
}

fn range_subset<T: Ord + Copy>(big: &Range<T>, b: &Range<T>) -> bool {
    if small.begin < big.begin {
        false
    } else if small.end > big.end {
        false
    } else {
        true
    }
}

pub struct Tensor<T> {
    axes: Vec<Axis>,
    elems: Vec<T>
}

impl <T> Tensor<T> {
    pub fn alloc(axes: Vec<Axis>) -> Self {
        let mut count = 1;
        for axis in axes.iter() {
            count *= axis.len();
        }
        let mut elems = Vec::new();
        elems.reserve_exact(count);
        Tensor { axes, elems }
    }

    fn effective_index(&self, idxs: &[isize]) -> Option<usize> {
        let mut effective_idx: usize = 0;
        for (axis_idx, idx) in idxs.iter().enumerate() {
            let axis = &self.axes[axis_idx];
            if !axis.contains(*idx) {
                return None
            }
            effective_idx += ((idx - axis.start + axis.offset) as usize) * axis.scale;
        }
        Some(effective_idx)
    }

    fn bounds_check(&self, hull: &[Range<isize>], src: &Self) -> bool {
        if src.axes.len() != self.axes.len() {
            return false;
        }

        if hull.len() != src.axes.len() {
            return false;
        }

        for idx in 0..(hull.len()) {
            if !range_subset(src.axes[idx].logical_range(), hull[idx]) {
                return false;
            }
            if !range_subset(self.axes[idx].logical_range(), hull[idx]) {
                return false;
            }
        }
        return true;
    }

    fn add(&mut self, hull: &[Range<isize>], src: &Self) -> bool {
        // bounds check the whole access
        if !self.bounds_check(hull, src) { return false; }

        for axis in 0..(hull.len()) {
            let hull_axis = &hull[axis];
            let n = hull_axis.end - hull_axis.start;
        }


    }
}

pub struct TensorHandle<T>(Rc<UnsafeCell<Tensor<T>>>);

impl <T> TensorHandle<T> {
    pub fn reference_eq(a: &Self, b: &Self) -> bool{
        std::ptr::eq(Rc::as_ptr(&a.0), Rc::as_ptr(&b.0))
    }
}

#[clone_blob("tensor", defaults)]
#[derive(Clone)]
enum AnyTensor {
    TensorInt(Rc<UnsafeCell<Tensor<i64>>>),
    TensorFloat(Rc<UnsafeCell<Tensor<f64>>>),
}

unsafe impl Sync for AnyTensor {}
unsafe impl Send for AnyTensor {}

predicates! {

}


#[no_mangle]
pub extern "C" fn install() {
}
