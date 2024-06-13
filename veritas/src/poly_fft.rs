use blstrs::Scalar;
use ff::Field;
use ff::PrimeField;
use halo2_proofs::arithmetic::best_fft;
use rayon::prelude::*;

pub fn get_final_poly_fast(roots: &Vec<Scalar>) -> Vec<Scalar> {
    let n = roots.len();

    if n == 1 {
        return vec![roots[0], Scalar::ONE];
    }

    // recursively get the coefficients of the left and right halves
    // of the roots in parallel
    let m = n / 2;

    // Spawn parallel tasks for left and right halves
    let (left, right) = rayon::join(
        || get_final_poly_fast(&roots[0..m].to_vec()),
        || get_final_poly_fast(&roots[m..].to_vec()),
    );

    // multiply the coefficients of the left and right halves
    // to get the coefficients of the final polynomial
    fft_mul(&left, &right)
}

// a divide and conquer algorithm to get the coefficients of the polynomial
// given the roots of the polynomial. This is a recursive algorithm.
// pub fn get_final_poly_fast(roots: &Vec<Scalar>) -> Vec<Scalar> {
//     let n = roots.len();

//     if n == 1 {
//         return vec![roots[0], Scalar::ONE];
//     }

//     // recursively get the coefficients of the left and right halves
//     // of the roots
//     let m = n / 2;
//     // the left half of the roots is from 0 to m-1
//     let left = get_final_poly_fast(&roots[0..m].to_vec());
//     // the right half of the roots is from m to n-1
//     let right = get_final_poly_fast(&roots[m..].to_vec());

//     // multiply the coefficients of the left and right halves
//     // to get the coefficients of the final polynomial
//     fft_mul(&left, &right)
// }

pub fn fft_mul(left: &Vec<Scalar>, right: &Vec<Scalar>) -> Vec<Scalar> {
    // get the degree of the left and right polynomials
    let degree_left = left.len();
    let degree_right = right.len();

    // Determine the length of the image after multiplication
    let degree_image = degree_left + degree_right + 1;

    // Clone and resize the vectors
    let mut left = left.clone();
    let mut right = right.clone();

    // this is the 2^32th root of unity
    const ROOT_OF_UNITY: Scalar = Scalar::ROOT_OF_UNITY;

    // get the size of the set
    let size_set: u32 = degree_image.try_into().unwrap();

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

    left.resize(n.try_into().unwrap(), Scalar::ZERO);
    right.resize(n.try_into().unwrap(), Scalar::ZERO);

    // Perform FFT on the left and right vectors
    best_fft(&mut left, omega, s);
    best_fft(&mut right, omega, s);

    // Perform point-wise multiplication of the transformed vectors
    let mut result: Vec<Scalar> = left.iter().zip(right.iter()).map(|(a, b)| a * b).collect();

    // Perform inverse FFT
    best_fft(&mut result, omega_inv, s);

    // Remove trailing zeros from the result
    while result.last() == Some(&Scalar::ZERO) {
        result.pop();
    }

    // Normalize the result by shifting
    for x in &mut result {
        *x = x.shr(s as usize);
    }

    result
}

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
    let chunks: Vec<Vec<Scalar>> = scalars
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
