/* Flatten a nested list structure. (medium) */

/* Might not be typed correcty */

type node('a) =
  | None
  | One('a)
  | Many('a, node('a));

let list_a: node(char) = Many('a', Many('b', Many('c', One('d'))));

let rec flatten = (l) => {
  switch l {
    | None => []
    | One(x) => [x]
    | Many(x, rest) => [x]@flatten(rest)
  }
}

Js.log(flatten(list_a) == ['a', 'b', 'c', 'd'])