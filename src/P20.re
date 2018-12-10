/**
20. Remove the K'th element from a list. (easy)

The first element of the list is numbered 0, the second 1,...

# remove_at 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "c"; "d"]
 */

let list = ["a","b","c","d"];
let v = 1;
let res = ["a", "c", "d"];

let remove_at = n => l => {
    let rec aux = n => l => acc => {
        switch l {
            | [] => List.rev(acc)
            | [a, ...rest] => n == 0 ? List.rev(acc)@rest : aux(n-1)(rest)([a, ...acc])
        }
    }

    aux(n)(l)([])
}

Js.log(remove_at(v)(list) -> Array.of_list);
Js.log(remove_at(v)(list) == res);