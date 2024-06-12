mod poly;

use std::time::Instant;

use ff::Field;

use blstrs::Scalar;
use rand::thread_rng;

use crate::poly::{get_final_poly, Fmpz};

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
    const N: usize = 10;

    let mut rng = thread_rng();

    // generate vector of random scalars
    let stopwatch = Instant::now();
    let scalars: Vec<Scalar> = (0..N).map(|_| Scalar::random(&mut rng)).collect();
    let _fmpz_scalars: Vec<Fmpz> = scalars.iter().map(|s| Fmpz::from_scalar(*s)).collect();
    // println!(
    //     "fmpz: {:?}",
    //     fmpz_scalars
    //         .iter()
    //         .map(|f| f.to_string(10))
    //         .collect::<Vec<String>>()
    // );
    let time = stopwatch.elapsed();
    println!("Time to generate scalars: {:?}", time);

    let stopwatch = Instant::now();
    let coeff: Vec<Scalar> = get_final_poly(&scalars);
    let _fmpz_coeff: Vec<Fmpz> = coeff.iter().map(|s| Fmpz::from_scalar(*s)).collect();
    // println!(
    //     "fmpz: {:?}",
    //     fmpz_coeff
    //         .iter()
    //         .map(|f| f.to_string(10))
    //         .collect::<Vec<String>>()
    // );
    let time = stopwatch.elapsed();
    println!("Time to generate coeff: {:?}", time);
}
