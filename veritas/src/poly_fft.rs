use blstrs::Scalar;
use ff::Field;
use ff::PrimeField;
use halo2_proofs::arithmetic::best_fft;
use rayon::prelude::*;

pub fn get_final_poly_fast(roots: &[Scalar]) -> Vec<Scalar> {
    let n = roots.len();

    if n == 1 {
        return vec![roots[0], Scalar::ONE];
    }

    let m = n / 2;

    // Spawn parallel tasks for left and right halves
    let (left, right) = rayon::join(
        || get_final_poly_fast(&roots[..m]),
        || get_final_poly_fast(&roots[m..]),
    );

    // Multiply the coefficients of the left and right halves
    fft_mul(&left, &right)
}

pub fn fft_mul(left: &[Scalar], right: &[Scalar]) -> Vec<Scalar> {
    let degree_image = left.len() + right.len() - 1;

    // This is the 2^32th root of unity
    const ROOT_OF_UNITY: Scalar = Scalar::ROOT_OF_UNITY;

    // Calculate the smallest n = 2^s such that 2^s >= degree_image
    let s: u32 = degree_image.next_power_of_two().trailing_zeros();
    let n: usize = 1 << s;

    // Calculate the n-th root of unity and its inverse
    let omega = ROOT_OF_UNITY.pow_vartime(&[(1u64 << (32 - s)) as u64]);

    // Clone and resize the vectors
    let mut left = left.to_vec();
    let mut right = right.to_vec();
    left.resize(n, Scalar::ZERO);
    right.resize(n, Scalar::ZERO);

    // Perform FFT on the left and right vectors
    best_fft(&mut left, omega, s);
    best_fft(&mut right, omega, s);

    // Perform point-wise multiplication of the transformed vectors
    let mut result: Vec<Scalar> = left
        .iter()
        .zip(right.iter())
        .map(|(a, b)| *a * *b)
        .collect();

    // Perform inverse FFT
    best_fft(&mut result, omega.invert().unwrap(), s);

    // Normalize the result by dividing by n
    let n_inv = Scalar::from(n as u64).invert().unwrap();
    result.iter_mut().for_each(|x| *x *= n_inv);

    // Remove trailing zeros
    result.truncate(degree_image);

    result
}
