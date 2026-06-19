open Alcotest
open Formula

let x = it 1

let y = x + (it 1)

let test_term_extract () = (check int) "same int" 1 !x
let test_form_extract () = (check int) "same int" 2 !y

let _ = x =: 3

let test_update_term = (check int) "same int" 3 !x
let test_update_form = (check int) "same int" 4 !y

let z = x * x

let test_complex_expr = (check int) "same int" 9 !z

let w = x * y

let test_term_form_expr = (check int) "same int" 12 !w

let _ = x =: 2

let test_complex_update = (check int) "same int" 4 !z
let test_term_form_expr = (check int) "same int" 6 !w

let _ = x =:: (x + it 1)

let test_simple_inc = (check int) "same int" 3 !x
let test_complex_update = (check int) "same int" 9 !z
let test_term_from_expr = (check int) "same int" 12 !w
