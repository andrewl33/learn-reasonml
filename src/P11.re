/**
11. Modified run-length encoding.

Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

*/

type rle('a) = 
    | One('a)
    | Many(int, 'a);

let list_a = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

let encode = (l) => {
    let rec aux = (~l, ~acc=0, ()) => {
        switch l {
            | [] => []
            | [x] => [One(x)]
            | [a,b] => a == b ? [Many(acc+2, a)] : [Many(acc, a), One(b)]
            | [a, b, ...rest] => a == b ? aux(~l=[b, ...rest], ~acc=acc+1, ()) : [acc+1 == 1 ? One(a) : Many(acc+1, a), ...aux(~l=[b, ...rest], ())]       
        }
    }

    aux(~l=l, ())
}

Js.log(encode(list_a) == [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")]);
Js.log(encode(list_a));