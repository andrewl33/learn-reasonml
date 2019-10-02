/**
40. Goldbach's conjecture. (medium)

Goldbach's conjecture says that every positive even number greater 
than 2 is the sum of two prime numbers. 
Example: 28 = 5 + 23. It is one of the most famous facts in 
number theory that has not been proved to be correct in the 
general case. It has been numerically confirmed up to 
very large numbers. Write a function to find the two prime numbers 
that sum up to a given even integer.
*/

let is_prime = n => {
    let n = max(n, -n)
    let rec aux = m => {
        m * m > n || (n mod m != 0 && aux(m+1))
    }

    aux(2)
}


let find_prime = n => {
    let rec aux = n => m => {
        if (is_prime(n) && is_prime(m-n)) {
            (n, m-n)
        } else {
            aux(n+1)(m)
        }
    }

    aux(2)(n)
}
Js.log(is_prime(26))
Js.log(find_prime(28))
