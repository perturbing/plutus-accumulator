use blstrs::Scalar;
use ff::Field;
use ff::PrimeField;
use halo2_proofs::arithmetic::best_fft;
use rayon::prelude::*;

// This module implements the calculation of multiplying n binomials of the form
// (x + a_i) for i in 0..n. This is done by chunking this work in d parral calculation.
// Before that, we first calculate a root of unity s such that s^N = 1. For N = 2^s,
// the closest power of 2 to n.

// In d chunks, we calculate the FFT of each first element of the chunk and then
// fold over the remaining elements in the chunk, FFTing them and multiplying
// them pointwise with the first element. Afte the d chunks are done, we multiply
// the d results pointwise to get the final result.

// The final result is then inverse FFTed and truncated to the size of the set+1
// since n binomials will result in n+1 coefficients.

pub fn get_final_poly_halo2(scalars: &Vec<Scalar>, d: usize) -> Vec<Scalar> {
    // this is the 2^32th root of unity
    const ROOT_OF_UNITY: Scalar = Scalar::ROOT_OF_UNITY;

    // get the size of the set
    let size_set_plus_one: u32 = (scalars.len() + 1).try_into().unwrap();
    let size_set: u32 = scalars.len().try_into().unwrap();

    // calculate the number of leading zeros
    let leading_zeros = size_set.leading_zeros();

    // calculate the smallest n = 2^s such that 2^s >= size_set
    // Given the root of unity of order 2^32, we need to remove
    // t "orders" to get the root of unity of order n = 2^s.
    let s: u32 = 32 - leading_zeros;
    let t: u32 = 1 << leading_zeros;
    let n: u32 = 1 << s;

    // calc the n-th root of unity
    let omega: Scalar = ROOT_OF_UNITY.pow_vartime(&[t as u64]);
    // calc the inverse of omega
    let omega_inv: Scalar = omega.invert().unwrap();

    // Function to process a chunk of scalars
    let process_chunk = |chunk: &[Scalar]| -> Vec<Scalar> {
        // Convert the first scalar to its frequency form
        let mut final_poly = vec![chunk[0], Scalar::ONE];
        final_poly.resize(n as usize, Scalar::ZERO);
        best_fft(&mut final_poly, omega, s);

        // Process the remaining scalars in the chunk
        for scalar in chunk.iter().skip(1) {
            let mut poly = vec![*scalar, Scalar::ONE];
            poly.resize(n as usize, Scalar::ZERO);
            best_fft(&mut poly, omega, s);

            // Pointwise multiply and drop the poly out of scope
            final_poly = final_poly
                .iter()
                .zip(poly.iter())
                .map(|(a, b)| a * b)
                .collect();
        }

        final_poly
    };

    // Divide the scalars into d chunks and process them in parallel
    let chunk_size = (scalars.len() + d - 1) / d; // ceil(scalars.len() / d)
    let mut chunks: Vec<Vec<Scalar>> = scalars
        .chunks(chunk_size)
        .map(|chunk| chunk.to_vec())
        .collect();

    // Process each chunk in parallel
    let mut freq_polys: Vec<Vec<Scalar>> = chunks
        .par_iter()
        .map(|chunk| process_chunk(chunk))
        .collect();

    // Pointwise multiply the frequency final polynomials
    let mut final_poly = freq_polys.remove(0);
    for poly in freq_polys.iter() {
        final_poly = final_poly
            .iter()
            .zip(poly.iter())
            .map(|(a, b)| a * b)
            .collect();
    }

    // Calculate the inverse fft of the final polynomial
    best_fft(&mut final_poly, omega_inv, s);

    final_poly.truncate((size_set_plus_one as usize).try_into().unwrap());
    for x in &mut final_poly {
        *x = x.shr(s as usize);
    }
    final_poly
}
