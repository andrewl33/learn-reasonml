/* Find the number of elements of a list.  */

let list_a = ['a', 'b', 'c', 'd']

/* optionals must be placed first to support partial application */
let rec length = (~k=0, ~l, ()) => {
    switch(l) {
        | [] => k
        | [_, ...rest] => length(~l=rest,~k=k+1, ())
    }
}

Js.log(length(~l=list_a, ()) == 4)
