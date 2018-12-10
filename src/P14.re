/**
14. Duplicate the elements of a list. (easy)

duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

*/

let list_a = ['a', 'b', 'c', 'c', 'd'];
let res_a = ['a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd'];

let rec duplicate = (l) => {
    switch l {
        | [] => []
        | [a] => [a,a]
        | [a, ...rest] => [a, a]@duplicate(rest)
    }
}

Js.log(duplicate(list_a) -> Array.of_list)
Js.log(duplicate(list_a) == res_a)