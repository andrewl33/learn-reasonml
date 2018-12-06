/* Find the k'th element of a list. */

let list_a = ['a', 'b', 'c', 'd']

let rec at = (~k, ~l) => {
    switch(l) {
        | [] => None
        | [h, ...rest] => k == 0 ? Some(h) : at(~k=k-1, ~l=rest)
    }
} 

Js.log(at(~k=0, ~l=list_a) == Some('a'))
Js.log(at(~k=1, ~l=list_a) == Some('b'))
Js.log(at(~k=2, ~l=list_a) == Some('c'))
Js.log(at(~k=1000, ~l=list_a) == None)