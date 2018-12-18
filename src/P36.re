/**
36. Determine the prime factors of a given positive integer (2). (medium)

Construct a list containing the prime factors and their multiplicity. 
Hint: The problem is similar to problem Run-length encoding of a list (direct solution).

# factors 315;;
- : (int * int) list = [(3, 2); (5, 1); (7, 1)]
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

let encode = (l) => {
    let rec aux = (~l, ~acc=0, ()) => {
        switch l {
            | [] => []
            | [x] => [(x, acc+1)]
            | [a,b] => a == b ? [(a, acc+2)] : [(a, acc+1), (b, 1)]
            | [a, b, ...rest] => a == b ? aux(~l=[b, ...rest], ~acc=acc+1, ()) : [(a, acc+1), ...aux(~l=[b, ...rest], ())]       
        }
    }

    aux(~l=l, ())
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
        | _ => encode(List.rev(aux(n)([])(2)))
    }
}

Js.log(factors(315) -> Array.of_list)