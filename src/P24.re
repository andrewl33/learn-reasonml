/**
24. Lotto: Draw N different random numbers from the set 1..M. (easy)

The selected numbers shall be returned in a list.

# lotto_select 6 49;;
- : int list = [10; 20; 44; 22; 41; 2]
 */

let lotto_select = n => m => {
    Random.init(0)
    let rec aux = n => m => acc => {
        switch n {
            | 0 => acc
            | _ => aux(n-1)(m)([Random.int(m) + 1, ...acc])
        }
    }

    aux(n)(m)([])
}

Js.log(lotto_select(6)(49) -> Array.of_list)