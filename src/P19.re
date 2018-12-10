/**

19. Rotate a list N places to the left. (medium)

# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*/

let list_a = ["a", "b", "c", "d", "e", "f", "g", "h"];
let val_a = 3;
let res_a = ["d", "e", "f", "g", "h", "a", "b", "c"];
let list_b = ["a", "b", "c", "d", "e", "f", "g", "h"];
let val_b = -2;
let res_b = ["g", "h", "a", "b", "c", "d", "e", "f"];

let rotate = (l) => (n) => {
    let pivot = ((List.length(l) + n) mod List.length(l)) - 1;
    let rec aux = (l) => (i) => (acc) => {
        switch l {
            | [] => List.rev(acc)
            | [a, ...rest] => i == 0 ? rest@List.rev([a, ...acc]) : aux(rest)(i-1)([a, ...acc])
        }
    }
    
    aux(l)(pivot)([])
}

Js.log(rotate(list_a)(val_a) -> Array.of_list);
Js.log(rotate(list_a)(val_a) == res_a);
Js.log(rotate(list_b)(val_b) -> Array.of_list);
Js.log(rotate(list_b)(val_b) == res_b);
