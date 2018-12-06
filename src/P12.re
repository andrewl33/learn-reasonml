/**
12. Decode a run-length encoded list. (medium)
Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*/

type rle('a) = 
    | One('a)
    | Many(int, 'a);

let list_a = [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")];

let decode = (l) => {

    let rec count = (~letter, ~n) => {
        switch n {
            | 1 => [letter]
            | _ => [letter, ...count(~letter=letter, ~n=n-1)] 
        }
    }

    let rec aux = (l) => {
        switch (l) {
            | [] => []
            | [a, ...rest] => 
            List.concat([
                switch a {
                    | One(b) => [b]
                    | Many(amt, letter) => count(~n=amt, ~letter=letter)
                }, 
                aux(rest)
            ])
        }
    }

    aux(l)
}


Js.log(decode(list_a) == ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]);
Js.log(decode(list_a));