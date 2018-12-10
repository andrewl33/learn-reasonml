/**
18. Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the elements 
between the i'th and k'th element of the original list (both limits included). 
Start counting the elements with 0 (this is the way the List module numbers elements).

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
string list = ["c"; "d"; "e"; "f"; "g"]
*/

let list = ["a","b","c","d","e","f","g","h","i","j"];
let in_1 = 2;
let in_2 = 6;
let res = ["c","d","e","f","g"];

let slice = l => n => m => {
    let rec aux = l => i => j => e => {
        switch l {
            | [] => []
            | [a, ...rest] => if (i == 0 && j == 0) {
                List.rev(e)
            } else if (i == 0 && j > 0) {
                aux(rest)(0)(j-1)([a, ...e])
            } else {
                aux(rest)(i-1)(j)(e)
            }
        }
    }

    aux(l)(n)(m-n+1)([])
}

Js.log(slice(list)(in_1)(in_2) -> Array.of_list);
Js.log(slice(list)(in_1)(in_2) == res);