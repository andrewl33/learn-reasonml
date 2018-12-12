/**
21. Insert an element at a given position into a list. (easy)

Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)

# insert_at "alfa" 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
# insert_at "alfa" 3 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "alfa"; "d"]
# insert_at "alfa" 4 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "d"; "alfa"]
 */

let list = ["a", "b", "c", "d"];
let val_1 = 1;
let val_2 = 3;
let val_3 = 4;
let ins_str = "alfa";
let res_1 = ["a", "alfa", "b", "c", "d"];
let res_2 = ["a", "b", "c", "alfa", "d"];
let res_3 = ["a", "b", "c", "d", "alfa"];

let rec insert_at = l => s => n => {
    switch l {
        | [] => [s]
        | [a, ...rest] => n == 0 ? [s,a, ...rest] : [a]@insert_at(rest, s, n-1)
    }
}

Js.log(insert_at(list)(ins_str)(val_1) -> Array.of_list)
Js.log(insert_at(list)(ins_str)(val_1) == res_1)
Js.log(insert_at(list)(ins_str)(val_2) -> Array.of_list)
Js.log(insert_at(list)(ins_str)(val_2) == res_2)
Js.log(insert_at(list)(ins_str)(val_3) -> Array.of_list)
Js.log(insert_at(list)(ins_str)(val_3) == res_3)