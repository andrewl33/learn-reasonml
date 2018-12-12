/**
25. Generate a random permutation of the elements of a list. (easy)
# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]
*/

let list = ["a", "b", "c", "d", "e", "f"];


Random.init(0)

let permutation = l => {
    let rec aux = l => c => acc => r => {
        switch l {
            | [] => ra(List.rev(r))(acc)
            | [a, ...rest] => c == 0 ? ra(List.rev(r)@(rest))([a, ...acc]) : aux(rest)(c-1)(acc)([a, ...r])
        }
    }
    and ra = l => acc => {
        switch l {
            | [] => acc
            | _ => aux(l)(Random.int(List.length(l)))(acc)([])
        }
    }

    ra(l)([])
}

Js.log(permutation(list) -> Array.of_list)