/* Find the last but one (last and penultimate) elements of a list. */

let list_a = ["a", "b", "c", "d"];
let list_b = ["a"];

let rec last_two = (l) => {
    switch l {
        | []|[_] => None
        | [x, y] => Some((x, y))
        | [_, ...rest] => last_two(rest)
    }
}

Js.log(last_two(list_a) == Some(("c", "d")))
Js.log(last_two(list_b) == None)