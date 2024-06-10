use std::time::Instant;

use ff::Field;
use group::Group;

use blstrs::{G2Projective, Scalar};
use rand::rngs::StdRng;
use rand::{thread_rng, SeedableRng};
use rayon::prelude::*;

fn main() {
    const N: usize = 100_000;

    let mut rng = thread_rng();

    // generate vector of random scalars
    let stopwatch = Instant::now();
    let scalars: Vec<Scalar> = (0..N).map(|_| Scalar::random(&mut rng)).collect();
    let time = stopwatch.elapsed();
    println!("Time to generate scalars: {:?}", time);

    let stopwatch = Instant::now();
    // concurrently generate vector of random G2 points
    let g2_points: Vec<G2Projective> = (0..N)
        .into_par_iter()
        .map(|i| {
            let mut thread_rng = StdRng::seed_from_u64(i as u64);
            G2Projective::random(&mut thread_rng)
        })
        .collect();
    let time = stopwatch.elapsed();
    println!("Time to generate G2 points: {:?}", time);

    let stopwatch = Instant::now();
    let result: G2Projective = G2Projective::multi_exp(&g2_points, &scalars);
    println!("Result: {:?}", result);
    let time = stopwatch.elapsed();
    println!("Time to compute multiexp: {:?}", time);
}
