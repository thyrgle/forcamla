open Alcotest
open Formula

let test_term_extract () =
  let x = it 1 in
  (check int) "same int" 1 !x

let test_form_extract () =
  let x = it 1 in
  let y = x + (it 1) in
  (check int) "same int" 2 !y

let test_update_term () =
  let x = it 1 in
  x =: 3;
  (check int) "same int" 3 !x

let test_update_form () =
  let x = it 1 in
  let y = x + (it 1) in
  x =: 3;
  (check int) "same int" 4 !y

let test_complex_expr () = 
  let x = it 3 in
  let z = x * x in
  (check int) "same int" 9 !z

let test_term_form_expr () = 
  let x = it 3 in
  let y = x + (it 1) in
  let w = x * y in
  (check int) "same int" 12 !w

let test_complex_update () = 
  let x = it 3 in
  let z = x * x in
  x =: 2;
  (check int) "same int" 4 !z

let test_term_form_expr () = 
  let x = it 3 in
  let y = x + (it 1) in
  let w = x * y in
  x =: 2;
  (check int) "same int" 6 !w

let test_simple_inc () =
  let x = it 2 in
  x =:: (x + it 1);
  (check int) "same int" 3 !x

let test_complex_update () =
  let x = it 2 in
  let z = x * x in
  x =:: (x + it 1); 
  (check int) "same int" 9 !z

let test_term_form_update () =
  let x = it 2 in
  let y = x + (it 1) in
  let w = x * y in
  x =:: (x + it 1); 
  (check int) "same int" 12 !w

let () =
  run "Utils" [
      "simple", [
        test_case "Assign term and extract" `Quick test_term_extract;
        test_case "Assign formula and extract" `Quick test_form_extract;
      ];
      "reassign-term", [
        test_case "Reassign term" `Quick test_update_term;
        test_case "Update formula" `Quick test_update_form;
      ];
      "complex-formula", [
        test_case "Complex formula" `Quick test_complex_expr;
        test_case "Formula + term formula" `Quick test_term_form_expr;
        test_case "Complex update" `Quick test_complex_update;
        test_case "Formula + term update" `Quick test_term_form_update;
      ];
  ]
