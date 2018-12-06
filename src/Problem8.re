/* 8. Eliminate consecutive duplicates of list elements. (medium)*/

let list_a = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

let rec compress = (l) => {
    switch l {
        | [] => []
        | [x] => [x]
        | [a,b] => a == b ? [a] : [a,b]
        | [a, b, ...rest] => a == b ? compress([b, ...rest]) : [a, ...compress([b, ...rest])]
    }
}

Js.log(compress(list_a))
Js.log(compress(list_a) == ["a", "b", "c", "a", "d", "e"])