/* 10. Run-length encoding of a list. (easy) */

/*
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*/

let list_a = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

let encode = (l) => {
    let rec aux = (~l, ~acc=0, ()) => {
        switch l {
            | [] => []
            | [x] => [(acc+1, x)]
            | [a,b] => a == b ? [(acc+2, a)] : [(acc+1, a), (1, b)]
            | [a, b, ...rest] => a == b ? aux(~l=[b, ...rest], ~acc=acc+1, ()) : [(acc+1, a), ...aux(~l=[b, ...rest], ())]       
        }
    }

    aux(~l=l, ())
}

Js.log(encode(list_a) == [(4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")]);
Js.log(encode(list_a) -> Array.of_list)