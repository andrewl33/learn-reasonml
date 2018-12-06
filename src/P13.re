/*

13. Run-length encoding of a list (direct solution). (medium)
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X.

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]

*/

type rle('a) = 
    | One('a)
    | Many(int, 'a);

let list_a = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

let encode = (l) => {
    let rec aux = (~l, ~count=0, ()) => {
        switch l {
            | [] => []
            | [a] => count > 0 ? [Many(count+1, a)] : [One(a)]
            | [a,b] => a==b ? [Many(count+2, a)] : count > 0 ? [Many(count+1, a), One(b)] : [One(a), One(b)]
            | [a,b,...rest] => a==b ? aux(~l=[b, ...rest], ~count=count+1, ()) : count > 0 ? [Many(count+1, a), ...aux(~l=[b, ...rest], ())] : [One(a), ...aux(~l=[b, ...rest], ())]
        }
    }

    aux(~l=l, ())
}

Js.log(encode(list_a) == [Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")]);
Js.log(encode(list_a) -> Array.of_list);