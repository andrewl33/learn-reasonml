/**
26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

In how many ways can a committee of 3 be chosen from a group of 12 people? 
We all know that there are C(12,3) = 220 possibilities 
(C(N,K) denotes the well-known binomial coefficients). 
For pure mathematicians, this result may be great. 
But we want to really generate all the possibilities in a list.

# extract 2 ["a";"b";"c";"d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
*/

let list = ["a", "b", "c", "d"]

let rec extract = k => l => {
    if (k <= 0) {
        [[]]
    } else {
        switch l {
            | [] => []
            | [h, ...rest] => {
                let head = List.map((l) => [h, ...l])(extract(k-1)(rest))
                let tail = extract(k)(rest)
                head @ tail
            }
        }
    }
}

Js.log(extract(2)(list) -> Array.of_list)