/**
17. Split a list into two parts; the length of the first part is given. (easy)

If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
-(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])
*/

let list_a = ["a","b","c","d","e","f","g","h","i","j"];
let val_a = 3;
let res_a = (["a", "b", "c"], ["d", "e", "f", "g", "h", "i", "j"]);
let list_b = ["a","b","c","d"];
let val_b = 5;
let res_b = (["a", "b", "c", "d"], []);

let split = (l) => (n) => {
    let rec aux = (l) => (n) => (x) => {
        switch l {
            | [] => (List.rev(x),[])
            | [a, ...rest] => n == 0 ? (List.rev(x), [a, ...rest]) : aux(rest)(n-1)([a, ...x])
        }
    }

    aux(l)(n)([])
}

Js.log(split(list_a)(val_a));
Js.log(split(list_a)(val_a) == res_a);
Js.log(split(list_b)(val_b));
Js.log(split(list_b)(val_b) == res_b);