/**
Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) 

*/

let l = ["a", "b", "c", "d"];

let rec last = (l) =>
    switch l {
        | [] => None
        | [x] => Some(x)
        | [_, ...rest] => last(rest)
    };

Js.log(last(l) == Some("d"))