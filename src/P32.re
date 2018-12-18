/**
32. Determine the greatest common divisor of two positive integer numbers. (medium)

Use Euclid's algorithm.


# gcd 13 27;;
- : int = 1
# gcd 20536 7826;;
- : int = 2
*/

let rec gcd = n => m => {
    if (m==0) {
        n
    } else {
        gcd(m)(n mod m)
    }
}

Js.log(gcd(13)(27))
Js.log(gcd(20536)(7826))