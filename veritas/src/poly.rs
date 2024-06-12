use ff::PrimeField;

use blstrs::Scalar;

use flint_sys::fmpz::{
    fmpz, fmpz_clear, fmpz_get_str, fmpz_get_ui_array, fmpz_set_str, fmpz_set_ui_array,
};
use flint_sys::fmpz_mod_poly::fmpz_mod_poly_product_roots_fmpz_vec;
use flint_sys::fmpz_poly::{
    fmpz_poly_clear, fmpz_poly_get_coeff_fmpz, fmpz_poly_init, fmpz_poly_length, fmpz_poly_t,
};
use std::ffi::{CStr, CString};
use std::ops::Neg;
use std::ptr;

#[derive(Clone, Debug)]
pub struct Fmpz {
    pub value: fmpz,
}

impl Fmpz {
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

        // Convert Scalar to bytes
        let bytes = scalar.to_bytes_le();

        // Interpret the bytes as an array of mp_limb_t
        let limbs = bytes.as_ptr() as *const flint_sys::deps::mp_limb_t;
        let limbs_len = (bytes.len() / std::mem::size_of::<flint_sys::deps::mp_limb_t>())
            as flint_sys::deps::mp_limb_signed_t;

        unsafe {
            fmpz_set_ui_array(&mut value as *mut fmpz, limbs, limbs_len);
        }

        Fmpz { value }
    }

    pub fn to_scalar(&self) -> Scalar {
        // Allocate space for the limbs
        // note that since our scalars are in the bls field
        // we know that they are 256 bits long, and thus have 4 limbs
        let num_limbs = 4;
        let mut limbs = vec![0 as flint_sys::deps::mp_limb_t; num_limbs as usize];

        // Get the limbs from fmpz
        unsafe {
            fmpz_get_ui_array(
                limbs.as_mut_ptr(),
                num_limbs as flint_sys::deps::mp_limb_signed_t,
                &self.value as *const fmpz,
            );
        }
        // Allocate space for the byte array directly
        let mut byte_array = [0u8; 32];

        // Convert the limbs to bytes
        unsafe {
            let byte_slice = std::slice::from_raw_parts_mut(
                byte_array.as_mut_ptr() as *mut flint_sys::deps::mp_limb_t,
                num_limbs,
            );
            byte_slice.copy_from_slice(&limbs);
        }

        // Convert the byte array to a Scalar
        Scalar::from_bytes_le(&byte_array).unwrap()
    }
}

// find the polynomial (x + s_1)(x + s_2)...(x + s_n) for s_n in scalars
pub fn get_final_poly(scalars: &Vec<Scalar>) -> Vec<Scalar> {
    // Convert Scalars to Fmpz and negate them as the FLINT function expects the roots to be negated
    let xs: Vec<fmpz> = scalars
        .iter()
        .map(|s| Fmpz::from_scalar(s.neg()).value)
        .collect();

    // Get the prime modulus
    let fmpz_prime: fmpz =
        Fmpz::from_string(Scalar::MODULUS.to_string().chars().skip(2).collect()).value;

    // initialize the vector to store the coefficients
    let mut coeffs: Vec<Scalar> = Vec::new();

    unsafe {
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
    }
    coeffs
}
