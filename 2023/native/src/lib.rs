use swipl::prelude::*;

struct TensorHeader {
    refs: usize,
    dims_count: u32,
    dims_capacity: u32,
    elems_capacity: usize,
}

struct Tensor<T>( *mut T );

impl <T> Tensor<T> {
    fn layout(ndims: u32, nelems: usize) -> std::alloc::Layout {
        use std::alloc::*;

        let dims_layout = Layout::array::<usize>(ndims as usize).unwrap();
        let header = Layout::new::<TensorHeader>();
        let data_layout = Layout::array::<T>(nelems).unwrap();

        dims_layout.extend(header).unwrap().0.extend(data_layout).unwrap().0
    }

    fn alloc(ndims: u32, nelems: usize) -> Tensor<T> {
        use std::alloc::*;

        let begin = unsafe { alloc_zeroed(Self::layout(ndims, nelems)) };

        // println!("get {:x}", begin as usize);
        let res = Tensor(
            unsafe {
                begin.offset(
                    (std::mem::size_of::<TensorHeader>() +
                     std::mem::size_of::<usize>() * (ndims as usize) ) as isize
                ) as *mut T
            }
        );
        // println!("ret {:x}", res.0 as usize);

        res.header_mut().refs = 1;
        res.header_mut().dims_count = ndims;
        res.header_mut().dims_capacity = ndims;
        res.header_mut().elems_capacity = nelems;

        res
    }

    unsafe fn dealloc(&mut self) {
        let ndims = self.header().dims_count;
        let nelems = self.header().elems_capacity;
        unsafe {
            std::alloc::dealloc(self.0 as *mut u8, Self::layout(ndims, nelems));
        }
    }

    fn dims_mut(&self) -> &mut [usize] {
        let header = self.header();
        let size = header.dims_count as usize;
        unsafe {
            let begin = ((header as *const TensorHeader) as *mut usize).offset(-1 * (header.dims_count as isize));
            std::slice::from_raw_parts_mut(begin, size)
        }
    }

    fn dims(&self) -> &[usize] {
        self.dims_mut()
    }

    fn header_mut(&self) -> &mut TensorHeader {
        unsafe {
            let header_ptr = (self.0 as *mut u8).offset(
                -1 * (std::mem::size_of::<TensorHeader>() as isize)
            ) as *mut TensorHeader;

            // println!("header: {:x}", header_ptr as usize);

            header_ptr.as_mut().unwrap()
        }
    }

    fn header(&self) -> &TensorHeader {
        self.header_mut()
    }

    fn size(&self) -> usize {
        let mut result = 1;
        for dim in self.dims() {
            result *= dim;
        }
        result
    }

    fn elems_mut(&self) -> &mut [T] {
        unsafe {
            std::slice::from_raw_parts_mut(self.0, self.size())
        }
    }

    fn elems(&self) -> &[T] {
        self.elems_mut()
    }

    fn marshall_indices(&self, t: &Term<'_>) -> PrologResult<usize> {
        let f = t.get::<Functor>()?;

        // println!("arity {} dims {}", f.arity(), self.header().ndims);
        if (f.arity() as usize) != (self.header().dims_count as usize) {
            return Err(PrologError::Failure);
        }

        let mut result: usize = 0;
        let mut step: usize = 1;
        for (idx, dim) in self.dims().iter().enumerate() {
            let i: usize = t.get_arg_ex::<u64>(idx + 1)? as usize;
            if !(1..(dim + 1)).contains(&i) {
                return Err(PrologError::Failure);
            }
            result += step * (i - 1);
            step *= dim;
        }
        Ok(result)
    }

}

impl <T> Drop for Tensor<T> {
    fn drop(&mut self) {
        let refs = &mut self.header_mut().refs;
        assert!(*refs > 0);
        *refs -= 1;
        if *refs == 0 {
            unsafe {
                self.dealloc();
            }
        }
    }
}

impl <T> Clone for Tensor<T> {
    fn clone(&self) -> Tensor<T> {
        let refs = &mut self.header_mut().refs;
        *refs += 1;
        Tensor(self.0)
    }
}

unsafe impl <T> Send for Tensor<T> { }
unsafe impl <T> Sync for Tensor<T> { }

wrapped_clone_blob!("tensor_i64", TensorInt, Tensor<i64>, defaults);
wrapped_clone_blob!("tensor_f64", TensorFloat, Tensor<f64>, defaults);

predicates! {
    pub semidet fn iota(_ctx, x, r) {
        let n: usize = x.get::<u64>()?.try_into().unwrap();
        let result = Tensor::<i64>::alloc(1, n);
        result.dims_mut()[0] = n;
        for i in 0..n {
            result.elems_mut()[i] = (i + 1) as i64;
        }
        r.unify(TensorInt(result))
    }

    pub semidet fn get_int(_ctx, x, idxs, r) {
        let t = x.get::<TensorInt>()?;
        let i: usize = t.marshall_indices(idxs)?;
        // println!("index {}", i);
        r.unify(t.0.elems()[i])

    }

}


#[no_mangle]
pub extern "C" fn install() {
    register_iota();
    register_get_int();
}
