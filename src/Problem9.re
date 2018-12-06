/*  Pack consecutive duplicates of list elements into sublists. (medium) */

let list_a = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

let pack = l => {
    let rec aux = (~acc=[], ~l, ()) => {
        switch l {
        | [] => []
        | [x] => [acc, [x]]
        | [a,b] => a==b ? [[a, b, ...acc]] : [[a, ...acc], [b]]
        | [a, b, ...rest] => a==b ? aux(~l=[b, ...rest], ~acc=[a, ...acc], ()) : [[a, ...acc], ...aux(~l=[b, ...rest], ~acc=[], ())]
        }
    }
    aux(~l=l, ())  
}

let pack_test = (list) => {
  let rec aux = (current, acc) =>
    fun
    | [] => [] /* Can only be reached if original list is empty */
    | [x] => [[x, ...current], ...acc]
    | [a, ...[b, ..._] as t] =>
      if (a == b) {
        aux([a, ...current], acc, t);
      } else {
        aux([], [[a, ...current], ...acc], t);
      };
  List.rev(aux([], [], list));
};


Js.log(pack(list_a) -> Array.of_list)

Js.log(pack_test(list_a))

Js.log(pack(list_a) == pack_test(list_a))

Js.log(pack(list_a) == [["a","a","a","a"],["b"],["c","c"],["a","a"],["d"],["e","e","e","e"]])