#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

mod poly;
mod poly_fft;

use std::time::Instant;

use ff::{Field, PrimeField};

use crate::poly::get_final_poly;
use crate::poly_fft::{fft_mul, get_final_poly_fast};
use blstrs::Scalar;
use halo2_proofs::arithmetic::best_fft;
use num_bigint::BigUint;
use num_traits::ops::bytes;
use num_traits::Num;
use rand::thread_rng;

fn main() {
    const N: u32 = 100_000;

    let mut rng = thread_rng();

    // generate vector of random scalars
    let stopwatch = Instant::now();
    let scalars: Vec<Scalar> = (0..N).map(|_| Scalar::random(&mut rng)).collect();
    let time = stopwatch.elapsed();
    println!("Time to generate scalars: {:?}", time);

    let stopwatch = Instant::now();
    // use sqrt of N size for d
    let coeff1: Vec<Scalar> = get_final_poly_fast(&scalars);
    let time = stopwatch.elapsed();
    println!("Time to generate coeff with halo2 fft: {:?}", time);

    let stopwatch = Instant::now();
    let coeff2: Vec<Scalar> = get_final_poly(&scalars);
    let time = stopwatch.elapsed();
    println!("Time to generate coeff with C bindings: {:?}", time);

    assert!(coeff1 == coeff2);
}
