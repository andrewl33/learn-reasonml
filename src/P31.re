/**
31. Determine whether a given integer number is prime. (medium)

# not(is_prime 1);;
- : bool = true
# is_prime 7;;
- : bool = true
# not (is_prime 12);;
- : bool = true
 */

let is_prime = n => {
    let rec is_not_factor = k => {
        k * k > n || (n mod k != 0 && is_not_factor(k+1))
    }
    if (n == 1) {
        false
    } else {
        is_not_factor(2)
    }
}

Js.log(!is_prime(1) == true)
Js.log(is_prime(7) == true)
Js.log(!is_prime(12) == false)