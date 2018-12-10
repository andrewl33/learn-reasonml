/**
16. Drop every N'th element from a list. (medium)

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*/

let list = ["a","b","c","d","e","f","g","h","i","j"];
let value = 3;
let res = ["a", "b", "d", "e", "g", "h", "j"];

let drop = (l) => (n) => {
    let rec aux = (l) => (i) => {
        switch l {
            | [] => []
            | [a] => i == 1 ? [] : [a]
            | [a, ...rest] => i == 1 ? aux(rest)(n) : List.concat([[a], aux([...rest])(i-1)])
         }
    }

    aux(l)(n)
}

Js.log(drop(list)(value) -> Array.of_list);
Js.log(drop(list)(value) == res);