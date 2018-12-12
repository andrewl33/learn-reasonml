/**
23. Extract a given number of randomly selected elements from a list. (medium)

The selected items shall be returned in a list. 
We use the Random module but do not initialize it with 
Random.self_init for reproducibility.


# rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
- : string list = ["g"; "d"; "a"]
*/

let list = ["a","b","c","d","e","f","g","h"];
let value = 3;
let res = ["g", "d", "a"]

let rand_select = l => n => {
    Random.init(0)
    let rec aux = list => m => c => n => acc => new_list => {
        switch list {
            | [] => []
            | [a, ...rest] => if (c < m) {
                aux(rest)(m)(c+1)(n)(acc)([a, ...new_list])
            } else {
                aux2(List.rev(new_list)@rest)(n-1)([a, ...acc])
            }
        }
    }
    and aux2 = list => n => acc => {
        let idx = Random.int(List.length(list))
        switch n {
            | 0 => acc
            | _ => aux(list)(0)(idx)(n)(acc)([])
        }
    }
    
    aux2(l)(n)([])

}

Js.log(rand_select(list)(value))