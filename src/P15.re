/**

15. Replicate the elements of a list a given number of times. (medium)


replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]

*/

let list_a = ["a", "b", "c"];
let res_a = ["a", "a", "a", "b", "b", "b", "c", "c", "c"];
let input_val = 3

let rec replicate = (l) => (n) => {
    let rec aux = (char) => (n) => {
        switch n {
            | 0 => []
            | 1 => [char]
            | _ => [char, ...aux(char)(n-1)]
        }
    }

    switch l {
        | [] => []
        | [a] => aux(a)(n)
        | [a, ...rest] => List.concat([aux(a)(n),replicate(rest)(n)])
    }
}

Js.log(replicate(list_a)(input_val) -> Array.of_list)
Js.log(replicate(list_a)(input_val) == res_a)