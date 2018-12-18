/**
35. Determine the prime factors of a given positive integer. (medium)

Construct a flat list containing the prime factors in ascending order.

# factors 315;;
- : int list = [3; 3; 5; 7]
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

let factors = n => {
    let rec aux = n => l => c => {
        if (c*c > n || n < c) {
            if (is_prime(n)) {
                [n, ...l]
            } else {
                l
            }

        } else {
            switch (n mod c) {
                | 0 => aux(n/c)([c, ...l])(2)
                | _ => aux(n)(l)(c+1)
            }
        }
    }
    
    switch n {
        | 0 => []
        | 1 => []
        | _ => List.rev(aux(n)([])(2))
    }
}



Js.log(factors(315) -> Array.of_list)