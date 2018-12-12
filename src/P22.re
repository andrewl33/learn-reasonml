/**
22. Create a list containing all integers within a given range. (easy)

If first argument is greater than second, produce a list in decreasing order.

# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
# range 9 4;;
- : int list = [9; 8; 7; 6; 5; 4]
*/

let a = 4;
let b = 9;
let res_a = [4, 5, 6, 7, 8, 9];


let range = a => b => {
    let rec aux = n => m => acc => {
        let x = n < m;
        switch x {
            | false => [n, ...acc]
            | true => aux(n)(m-1)([m, ...acc])
        }
    }
    let x = a == b;
    switch x {
        | true => [a]
        | false => a < b ? aux(a)(b)([]) : List.rev(aux(b)(a)([]))
    }
}

Js.log(range(a)(b) -> Array.of_list)
Js.log(range(a)(b) == res_a)
Js.log(range(b)(a) -> Array.of_list)
Js.log(range(b)(a) == List.rev(res_a))