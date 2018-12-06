/* Find out whether a list is a palindrome. */

let list_a = ['a', 'b', 'c', 'd']
let list_b = ['a', 'b', 'b', 'a']
let list_c = ['a', 'b', 'c', 'b', 'a']

let s_palindrome = (l) => {
  let rev_list = List.rev(l);
  rev_list == l
}

Js.log(s_palindrome(list_a) == false)
Js.log(s_palindrome(list_b) == true)
Js.log(s_palindrome(list_c) == true)