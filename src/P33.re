/**
33. Determine whether two positive integer numbers are coprime. (easy)

Two numbers are coprime if their greatest common divisor equals 1.

# coprime 13 27;;
- : bool = true
# not (coprime 20536 7826);;
- : bool = true
*/

let coprime = n => m => {
    let rec aux = x => y => {
        if (x == 0) y else aux(y mod x)(x) 
    }

    aux(n)(m) == 1
}

Js.log(coprime(13)(27))
Js.log(coprime(13)(27) == true)
Js.log(!coprime(20536)(7826))
Js.log(coprime(20536)(7826) == false)