/**
39. A list of prime numbers. (easy)

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
*/

let is_prime = n => {
    let rec aux = m => {
        m * m > n ||(n mod m != 0 && aux(m+1))
    }

    aux(2)
}

let rec all_primes = low => high => {
    if (low > high) {
        []
    } else {
        let rest = all_primes(low+1)(high)

        if(is_prime(low)) {  
            [low, ...rest]
        } else {
            rest
        } 
    }
}

Js.log(List.length(all_primes(2)(7920))==1000)