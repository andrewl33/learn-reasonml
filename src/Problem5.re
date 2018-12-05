/* Reverse a list */

let list_a = ['a', 'b', 'c', 'd']

let rec rev = (l) => {
    switch l {
        | [] => []
        | [a] => [a]
        | [a, ...rest] => rev(rest)@[a]
    }
}

Js.log(rev(list_a) == List.rev(list_a))