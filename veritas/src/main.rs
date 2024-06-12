mod poly;

use std::time::Instant;

use ff::Field;

use blstrs::Scalar;
use rand::thread_rng;

use crate::poly::get_final_poly;

// fn scalar_from_hex(hex: &str) -> Scalar {
//     let scalar = Scalar::from_bytes_be(&{
//         let mut array = [0u8; 32];
//         let bytes = hex::decode(hex).expect("Decoding failed");
//         array[32 - bytes.len()..].copy_from_slice(&bytes);
//         array
//     })
//     .expect("Conversion failed");
//     scalar
// }

fn main() {
    const N: usize = 10_000_000;

    let mut rng = thread_rng();

    // generate vector of random scalars
    let stopwatch = Instant::now();
    let scalars: Vec<Scalar> = (0..N).map(|_| Scalar::random(&mut rng)).collect();
    let time = stopwatch.elapsed();
    println!("Time to generate scalars: {:?}", time);

    let stopwatch = Instant::now();
    let _coeff: Vec<Scalar> = get_final_poly(&scalars);
    let time = stopwatch.elapsed();
    println!("Time to generate coeff: {:?}", time);
}
