/**
34. Calculate Euler's totient function φ(m). (medium)

Euler's so-called totient function φ(m) is defined as the number 
of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

Find out what the value of φ(m) is if m is a prime number. 
Euler's totient function plays an important role in one of the most 
widely used public key cryptography methods (RSA). 
In this exercise you should use the most primitive method to calculate 
this function (there are smarter ways that we shall discuss later).

# phi 10;;
- : int = 4
# phi 13;;
- : int = 12
*/

let coprime = n => m => {
    let rec aux = x => y => {
        if (x == 0) y else aux(y mod x)(x) 
    }

    aux(n)(m) == 1
}

let phi = m => {
    let rec aux = m => c => r =>{
        switch (r == m) {
            | true => c
            | false => if (coprime(m)(r)) aux(m)(c+1)(r+1) else aux(m)(c)(r+1)
        }
    }

    if (m == 1) 1 else aux(m)(1)(2)
}

Js.log(phi(10))
Js.log(phi(10) == 4)
Js.log(phi(13))
Js.log(phi(13) == 12)