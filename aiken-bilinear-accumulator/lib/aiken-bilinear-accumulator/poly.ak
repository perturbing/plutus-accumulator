use aiken/list.{concat, foldl, map, map2}

// Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
//Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
fn get_final_poly(binomial_poly: List<Int>) {
  let go =
    fn(term: Int, acc: List<Int>) {
      let shifted_poly =
        [0, ..acc]
      let multiplied_poly = map(concat(acc, [0]), fn(x) { x * term })
      map2(shifted_poly, multiplied_poly, fn(x, y) { x + y })
    }
  foldl(binomial_poly, [1], go)
}

test test_get_final_poly() {
  get_final_poly([2, 3]) == [6, 5, 1]
}

test test_get_final_poly_large() {
  get_final_poly([2, 3, 5, 7, 11]) == [2310, 2927, 1358, 288, 28, 1]
}
