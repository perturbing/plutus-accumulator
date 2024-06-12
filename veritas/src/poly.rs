use ff::PrimeField;

use blstrs::Scalar;

use flint_sys::fmpz::{fmpz, fmpz_clear, fmpz_get_str, fmpz_set_str};
use flint_sys::fmpz_mod_poly::fmpz_mod_poly_product_roots_fmpz_vec;
use flint_sys::fmpz_poly::{
    fmpz_poly_clear, fmpz_poly_get_coeff_fmpz, fmpz_poly_init, fmpz_poly_length, fmpz_poly_t,
};
use std::ffi::{CStr, CString};
use std::ops::Neg;
use std::ptr;

use std::time::Instant;

#[derive(Clone, Debug)]
pub struct Fmpz {
    pub value: fmpz,
}

impl Fmpz {
    // TODO: optimize this by not going through a string
    // the fmpz c implementation exposed a unsigned int function
    pub fn to_string(&self, base: i32) -> String {
        unsafe {
            let cstr_ptr = fmpz_get_str(ptr::null_mut(), base, &self.value);
            let cstr = CStr::from_ptr(cstr_ptr);
            let rust_str = cstr.to_str().unwrap().to_string();
            libc::free(cstr_ptr as *mut libc::c_void);
            rust_str
        }
    }

    pub fn from_string(string: String) -> Self {
        let mut value: fmpz = fmpz::default();
        let c_string = CString::new(string).unwrap();
        unsafe {
            fmpz_set_str(&mut value as *mut fmpz, c_string.as_ptr(), 16);
        }
        Fmpz { value }
    }

    pub fn from_scalar(scalar: Scalar) -> Self {
        let mut value: fmpz = fmpz::default();
        let hex_string = hex::encode(scalar.to_bytes_be());
        let c_string = CString::new(hex_string).unwrap();
        unsafe {
            fmpz_set_str(&mut value as *mut fmpz, c_string.as_ptr(), 16);
        }
        Fmpz { value }
    }

    pub fn to_scalar(&self) -> Scalar {
        let mut hex_string = self.to_string(16);

        // Ensure the hex string is padded to 64 characters (32 bytes)
        if hex_string.len() % 2 != 0 {
            hex_string.insert(0, '0');
        }
        while hex_string.len() < 64 {
            hex_string.insert(0, '0');
        }

        let bytes = hex::decode(hex_string).unwrap();
        let byte_array: [u8; 32] = bytes.try_into().expect("Slice with incorrect length");
        Scalar::from_bytes_be(&byte_array).unwrap()
    }
}

// find the polynomial (x + s_1)(x + s_2)...(x + s_n) for s_n in scalars
pub fn get_final_poly(scalars: &Vec<Scalar>) -> Vec<Scalar> {
    let stopwatch = Instant::now();
    // Convert Scalars to Fmpz and negate them as the FLINT function expects the roots to be negated
    let xs: Vec<fmpz> = scalars
        .iter()
        .map(|s| Fmpz::from_scalar(s.neg()).value)
        .collect();
    let fmpz_prime: fmpz =
        Fmpz::from_string(Scalar::MODULUS.to_string().chars().skip(2).collect()).value;
    let mut coeffs: Vec<Scalar> = Vec::new();
    let time = stopwatch.elapsed();
    println!("Time to convert scalars: {:?}", time);

    unsafe {
        let stopwatch = Instant::now();
        // Initialize the polynomial
        let mut poly: fmpz_poly_t = std::mem::zeroed();
        fmpz_poly_init(&mut poly as *mut _);

        // Call the FLINT function
        fmpz_mod_poly_product_roots_fmpz_vec(
            &mut poly as *mut _,
            xs.as_ptr(),
            xs.len() as i64,
            &fmpz_prime as *const _,
        );
        let time = stopwatch.elapsed();
        println!("Time to call FLINT: {:?}", time);

        let stopwatch = Instant::now();
        // Extract the coefficients
        let length_poly = fmpz_poly_length(&poly as *const _);
        for i in 0..length_poly {
            let mut coeff = fmpz::default();
            fmpz_poly_get_coeff_fmpz(&mut coeff as *mut _, &poly as *const _, i as i64);
            let coeff_fmpz = Fmpz { value: coeff };
            coeffs.push(coeff_fmpz.to_scalar());
            fmpz_clear(&mut coeff as *mut _);
        }

        // Clear the polynomial and context
        fmpz_poly_clear(&mut poly as *mut _);
        let time = stopwatch.elapsed();
        println!("Time to extract coeffs: {:?}", time);
    }
    coeffs
}