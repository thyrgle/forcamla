open Alcotest
open Formula

let test_term_extract () =
  let x = t 1 in
  (check int) "same int" 1 !x

let test_term_extract_float () =
  let x = t 1.0 in
  (check (float 0.01)) "same float" 1.0 !x

let test_form_extract () =
  let x = t 1 in
  let y = x + (t 1) in
  (check int) "same int" 2 !y

let test_form_extract_float () =
  let x = t 1.0 in
  let y = x +. (t 1.0) in
  (check (float 0.01)) "same int" 2.0 !y

let test_update_term () =
  let x = t 1 in
  x =: 3;
  (check int) "same int" 3 !x

let test_update_term_float () =
  let x = t 1.0 in
  x =:. 3.0;
  (check (float 0.01)) "same int" 3.0 !x

let test_update_form () =
  let x = t 1 in
  let y = x + (t 1) in
  x =: 3;
  (check int) "same int" 4 !y

let test_update_form_float () =
  let x = t 1.0 in
  let y = x +. (t 1.0) in
  x =:. 3.0;
  (check (float 0.01)) "same int" 4.0 !y

let test_complex_expr () = 
  let x = t 3 in
  let z = x * x in
  (check int) "same int" 9 !z

let test_complex_expr_float () = 
  let x = t 3.0 in
  let z = x *. x in
  (check (float 0.01)) "same int" 9.0 !z

let test_term_form_expr () = 
  let x = t 3 in
  let y = x + (t 1) in
  let w = x * y in
  (check int) "same int" 12 !w

let test_term_form_expr_float () = 
  let x = t 3.0 in
  let y = x +. (t 1.0) in
  let w = x *. y in
  (check (float 0.01)) "same int" 12.0 !w

let test_complex_update () = 
  let x = t 3 in
  let z = x * x in
  x =: 2;
  (check int) "same int" 4 !z

let test_complex_update_float () = 
  let x = t 3.0 in
  let z = x *. x in
  x =:. 2.0;
  (check (float 0.01)) "same int" 4.0 !z

let test_term_form_expr () = 
  let x = t 3 in
  let y = x + (t 1) in
  let w = x * y in
  x =: 2;
  (check int) "same int" 6 !w

let test_term_form_expr_float () = 
  let x = t 3.0 in
  let y = x +. (t 1.0) in
  let w = x *. y in
  x =:. 2.0;
  (check (float 0.01)) "same int" 6.0 !w

let test_simple_inc () =
  let x = t 2 in
  x =: !(x + t 1);
  (check int) "same int" 3 !x

let test_complex_update () =
  let x = t 2 in
  let z = x * x in
  x =: !(x + t 1); 
  (check int) "same int" 9 !z

let test_term_form_update () =
  let x = t 2 in
  let y = x + (t 1) in
  let w = x * y in
  x =: !(x + t 1); 
  (check int) "same int" 12 !w

let test_simple_eq_no_change () =
  let x = t 1 in
  let y = x =? (t 0) in
  (check bool) "same bool" false !!y

let test_simple_eq () =
  let x = t 1 in
  let y = x =? (t 0) in
  x =: !(x - t 1);
  (check bool) "same bool" true !!y

let test_simple_sat () =
  let x = t 1 in
  let y = x =? (t 0) in
  let z = ref 1 in
  when_satisfied y (fun () : unit -> (z := 2));
  x =: !(x - t 1);
  (check int) "same int" 2 z.contents


let () =
  run "Utils" [
      "simple", [
        test_case "Assign term and extract" `Quick test_term_extract;
        test_case "Assign term and extract (float)" `Quick test_term_extract_float;
        test_case "Assign formula and extract" `Quick test_form_extract;
        test_case "Assign formula and extract (float)" `Quick test_form_extract_float;
      ];
      "reassign-term", [
        test_case "Reassign term" `Quick test_update_term;
        test_case "Reassign term (float)" `Quick test_update_term_float;
        test_case "Update formula" `Quick test_update_form;
        test_case "Update formula (float)" `Quick test_update_form_float;
      ];
      "complex-formula", [
        test_case "Complex formula" `Quick test_complex_expr;
        test_case "Complex formula (float)" `Quick test_complex_expr_float;
        test_case "Formula + term formula" `Quick test_term_form_expr;
        test_case "Formula + term formula (float)" `Quick test_term_form_expr_float;
        test_case "Complex update" `Quick test_complex_update;
        test_case "Formula + term update" `Quick test_term_form_update;
      ];
      "simple-equations", [
        test_case "Simple equation (no update)" `Quick test_simple_eq_no_change;
        test_case "Simple equation" `Quick test_simple_eq;
      ];
      "event-listeners", [
        test_case "Simple when_satisfied" `Quick test_simple_sat;
      ];
  ]
