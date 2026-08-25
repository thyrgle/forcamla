open Alcotest

let test_term_extract () =
  let open Formula in
  let x = c 1 in
  (check int) "same int" 1 !x

let test_term_extract_float () =
  let open Formula in
  let x = v 1.0 in
  (check (float 0.01)) "same float" 1.0 !x

let test_form_extract () =
  let open Formula in
  let x = v 1 in
  let y = x + (c 1) in
  (check int) "same int" 2 !y

let test_form_extract_float () =
  let open Formula in
  let x = v 1.0 in
  let y = x +. (c 1.0) in
  (check (float 0.01)) "same int" 2.0 !y

let test_update_term () =
  let open Formula in
  let x = v 1 in
  x =: 3;
  (check int) "same int" 3 !x

let test_update_term_float () =
  let open Formula in
  let x = v 1.0 in
  x =: 3.0;
  (check (float 0.01)) "same int" 3.0 !x

let test_update_form () =
  let open Formula in
  let x = v 1 in
  let y = x + (c 1) in
  x =: 3;
  (check int) "same int" 4 !y

let test_update_form_float () =
  let open Formula in
  let x = v 1.0 in
  let y = x +. (c 1.0) in
  x =: 3.0;
  (check (float 0.01)) "same int" 4.0 !y

let test_complex_expr () =
  let open Formula in
  let x = v 3 in
  let z = x * x in
  (check int) "same int" 9 !z

let test_complex_expr_float () = 
  let open Formula in
  let x = v 3.0 in
  let z = x *. x in
  (check (float 0.01)) "same int" 9.0 !z

let test_term_form_expr () = 
  let open Formula in
  let x = v 3 in
  let y = x + (c 1) in
  let w = x * y in
  (check int) "same int" 12 !w

let test_term_form_expr_float () = 
  let open Formula in
  let x = v 3.0 in
  let y = x +. (c 1.0) in
  let w = x *. y in
  (check (float 0.01)) "same int" 12.0 !w

let test_complex_update () = 
  let open Formula in
  let x = v 3 in
  let z = x * x in
  x =: 2;
  (check int) "same int" 4 !z

let test_complex_update_float () = 
  let open Formula in
  let x = v 3.0 in
  let z = x *. x in
  x =: 2.0;
  (check (float 0.01)) "same int" 4.0 !z

let test_term_form_expr () = 
  let open Formula in
  let x = v 3 in
  let y = x + (c 1) in
  let w = x * y in
  x =: 2;
  (check int) "same int" 6 !w

let test_term_form_expr_float () = 
  let open Formula in
  let x = v 3.0 in
  let y = x +. (c 1.0) in
  let w = x *. y in
  x =: 2.0;
  (check (float 0.01)) "same int" 6.0 !w

let test_simple_inc () =
  let open Formula in
  let x = v 2 in
  x =: !(x + c 1);
  (check int) "same int" 3 !x

let test_complex_update () =
  let open Formula in
  let x = v 2 in
  let z = x * x in
  x =: !(x + c 1); 
  (check int) "same int" 9 !z

let test_term_form_update () =
  let open Formula in
  let x = v 2 in
  let y = x + (c 1) in
  let w = x * y in
  x =: !(x + c 1); 
  (check int) "same int" 12 !w

let test_simple_eq_no_change () =
  let open Formula in
  let x = v 1 in
  let y = x = (c 0) in
  (check bool) "same bool" false !y

let test_simple_eq () =
  let open Formula in
  let x = v 1 in
  let y = x = (c 0) in
  x =: !(x - c 1);
  (check bool) "same bool" true !y
  
let test_simple_sat () =
  let open Formula in
  let x = v 1 in
  let y = x = (c 0) in
  let z = ref 1 in
  when_satisfied y (fun () : unit -> (z := 2));
  x =: !(x - c 1);
  (check int) "same int" 2 z.contents


let test_player_health_go () =
  let x = ref "In Play" in
  let g () = x := "Game Over" in
  let open Formula in
  let health = v 3 in
  when_satisfied (health = c 0) g;
  health =: !(health - c 1);
  health =: !(health - c 1);
  health =: !(health - c 1);
  (check string) "same string" "Game Over" x.contents

let test_player_health_ip () =
  let x = ref "In Play" in
  let g () = x := "Game Over" in
  let open Formula in
  let health = v 3 in
  when_satisfied (health = c 0) g;
  health =: !(health - c 1);
  health =: !(health - c 1);
  (check string) "same string" "In Play" x.contents

let test_source_simple_while () =
  let x = ref 0 in
  let g () = x := (!x + 1) in
  let open Formula in
  let s = make_source () in
  let y = v 0 in
  let test = (y >= c 3) in
  exec_while s test g;
  listen s;
  y =: !(y + c 1);
  listen s;
  y =: !(y + c 1);
  listen s;
  y =: !(y + c 1);
  listen s;
  y =: !(y + c 1);
  listen s;
  (check int) "same int" 2 x.contents

let test_source_simple_always () =
  let x = ref true in
  let g (b: bool) = x := b in
  let open Formula in
  let s = make_source () in
  let y = v 0 in
  let test = (y >= c 1) in
  exec_always s test g;
  (check bool) "same bool" true x.contents;
  listen s;
  (check bool) "same bool" false x.contents;
  y =: !(y + c 1);
  (check bool) "same bool" false x.contents;
  listen s;
  (check bool) "same bool" true x.contents

let test_sub_int () =
  let open Formula in
  let x = v 10 in
  let y = x - c 3 in
  (check int) "10 - 3" 7 !y;
  x =: 20;
  (check int) "20 - 3" 17 !y

let test_sub_float () =
  let open Formula in
  let x = v 10.0 in
  let y = x -. c 2.5 in
  (check (float 0.01)) "10 - 2.5" 7.5 !y;
  x =: 5.0;
  (check (float 0.01)) "5 - 2.5" 2.5 !y

let test_div_int () =
  let open Formula in
  let x = v 10 in
  let y = x / c 2 in
  (check int) "10 / 2" 5 !y;
  x =: 30;
  (check int) "30 / 2" 15 !y

let test_div_float () =
  let open Formula in
  let x = v 10.0 in
  let y = x /. c 4.0 in
  (check (float 0.01)) "10 / 4" 2.5 !y;
  x =: 30.0;
  (check (float 0.01)) "30 / 4" 7.5 !y

let test_deep_chain () =
  let open Formula in
  let a = v 1 in
  let b = v 2 in
  let c_ = v 3 in
  let deep = (a + b) * (c_ - c 1) in
  (check int) "(1+2)*(3-1)" 6 !deep;
  a =: 5;
  (check int) "(5+2)*(3-1)" 14 !deep

let test_string_concat () =
  let open Formula in
  let s1 = v "hello" in
  let s2 = v " world" in
  let s3 = s1 ^ s2 in
  (check string) "hello world" "hello world" !s3;
  s1 =: "goodbye";
  (check string) "goodbye world" "goodbye world" !s3

let test_string_concat_three () =
  let open Formula in
  let s1 = v "a" in
  let s2 = v "b" in
  let s3 = v "c" in
  let all = s1 ^ s2 ^ s3 in
  (check string) "abc" "abc" !all;
  s2 =: "X";
  (check string) "aXc" "aXc" !all

let test_string_concat_empty () =
  let open Formula in
  let s1 = v "hello" in
  let s2 = v "" in
  let s3 = s1 ^ s2 in
  (check string) "hello" "hello" !s3;
  s1 =: "";
  (check string) "empty" "" !s3

let test_concat_named () =
  let open Formula in
  let s1 = v "foo" in
  let s2 = v "bar" in
  let s3 = concat_strings s1 s2 in
  (check string) "foobar" "foobar" !s3;
  s1 =: "baz";
  (check string) "bazbar" "bazbar" !s3

let test_custom_binop () =
  let open Formula in
  let max_f = reg_bin max in
  let x = v 3 in
  let y = v 7 in
  let m = max_f x y in
  (check int) "max(3,7)" 7 !m;
  x =: 9;
  (check int) "max(9,7)" 9 !m

let test_gt_int () =
  let open Formula in
  let x = v 5 in
  let eq = x > c 3 in
  (check bool) "5 > 3" true !eq;
  x =: 2;
  (check bool) "2 > 3" false !eq

let test_gte_int () =
  let open Formula in
  let x = v 5 in
  let eq = x >= c 5 in
  (check bool) "5 >= 5" true !eq;
  x =: 4;
  (check bool) "4 >= 5" false !eq

let test_neq_int () =
  let open Formula in
  let x = v 1 in
  let eq = x <> c 2 in
  (check bool) "1 != 2" true !eq;
  x =: 2;
  (check bool) "2 != 2" false !eq

let test_lt_int_after_update () =
  let open Formula in
  let x = v 2 in
  let eq = x < c 5 in
  x =: 8;
  (check bool) "8 < 5" false !eq;
  x =: 4;
  (check bool) "4 < 5" true !eq

let test_lt_lte_initial () =
  let open Formula in
  let x = v 2 in
  (check bool) "2 < 5" true !(x < c 5);
  (check bool) "2 <= 2" true !(x <= c 2);
  let f = v 2.5 in
  (check bool) "2.5 < 3.0" true !(f < c 3.0);
  (check bool) "2.5 <= 2.5" true !(f <= c 2.5);
  x =: 7;
  (check bool) "7 < 5" false !(x < c 5);
  (check bool) "7 <= 2" false !(x <= c 2)

let test_float_cmp () =
  let open Formula in
  let x = v 2.5 in
  (check bool) "2.5 > 2.0" true !(x > c 2.0);
  (check bool) "2.5 >= 2.5" true !(x >= c 2.5);
  (check bool) "2.5 = 2.5" true !(x = c 2.5);
  (check bool) "2.5 != 3.0" true !(x <> c 3.0);
  x =: 1.5;
  (check bool) "1.5 >= 2.5" false !(x >= c 2.5)

let test_on_change_formula () =
  let open Formula in
  let changes = ref [] in
  let x = v 10 in
  let y = x + c 1 in
  on_change y (fun o n -> changes := (o, n) :: changes.contents);
  x =: 20;
  (check int) "new value" 21 !y;
  (check (list (pair int int))) "old and new" [(11, 21)] changes.contents

let test_on_change_term () =
  let open Formula in
  let changes = ref [] in
  let x = v 2 in
  on_change x (fun o n -> changes := (o, n) :: changes.contents);
  x =: 9;
  (check (list (pair int int))) "old and new" [(2, 9)] changes.contents

let test_when_satisfied_refires () =
  let open Formula in
  let count = ref 0 in
  let x = v 3 in
  when_satisfied (x = c 1) (fun () -> incr count);
  x =: 2;
  x =: 1;
  (check int) "fired first time" 1 count.contents;
  x =: 0;
  x =: 1;
  (check int) "fired again" 2 count.contents

let test_sys_and () =
  let open Formula in
  let x = v 1 in
  let sys = (x >= c 1) && (x <= c 3) in
  (check bool) "1 in [1,3]" true !sys;
  x =: 0;
  (check bool) "0 in [1,3]" false !sys;
  x =: 2;
  (check bool) "2 in [1,3]" true !sys;
  x =: 4;
  (check bool) "4 in [1,3]" false !sys

let test_sys_or () =
  let open Formula in
  let x = v 5 in
  let sys = (x = c 1) || (x = c 5) in
  (check bool) "5 is 1 or 5" true !sys;
  x =: 2;
  (check bool) "2 is 1 or 5" false !sys;
  x =: 1;
  (check bool) "1 is 1 or 5" true !sys

let test_sys_nested () =
  let open Formula in
  let x = v 1 in
  let in_range = (x >= c 1) && (x <= c 3) in
  let sys = in_range || (x = c 10) in
  (check bool) "1 in range" true !sys;
  x =: 10;
  (check bool) "10 special" true !sys;
  x =: 5;
  (check bool) "5 nothing" false !sys;
  x =: 2;
  (check bool) "2 in range" true !sys

let test_form_and () =
  let open Formula in
  let a = v true in
  let b = v false in
  let f = and_ a b in
  (check bool) "true && false" false !f;
  b =: true;
  (check bool) "true && true" true !f;
  a =: false;
  (check bool) "false && true" false !f

let test_form_or () =
  let open Formula in
  let a = v false in
  let b = v false in
  let f = or_ a b in
  (check bool) "false || false" false !f;
  b =: true;
  (check bool) "false || true" true !f;
  a =: true;
  (check bool) "true || true" true !f;
  b =: false;
  (check bool) "true || false" true !f

let test_form_and_nested () =
  let open Formula in
  let a = v true in
  let b = v true in
  let c = v false in
  let f = and_ (and_ a b) c in
  (check bool) "true && true && false" false !f;
  c =: true;
  (check bool) "true && true && true" true !f;
  a =: false;
  (check bool) "false && true && true" false !f

let test_form_or_nested () =
  let open Formula in
  let a = v false in
  let b = v false in
  let c = v false in
  let f = or_ (or_ a b) c in
  (check bool) "false || false || false" false !f;
  c =: true;
  (check bool) "false || false || true" true !f;
  c =: false;
  b =: true;
  (check bool) "false || true || false" true !f

let test_form_and_or_mixed () =
  let open Formula in
  let a = v true in
  let b = v false in
  let c = v true in
  let f = and_ (or_ a b) c in
  (check bool) "(true || false) && true" true !f;
  a =: false;
  (check bool) "(false || false) && true" false !f;
  b =: true;
  (check bool) "(false || true) && true" true !f

let test_when_satisfied_and () =
  let open Formula in
  let count = ref 0 in
  let x = v 1 in
  let sys = (x >= c 1) && (x <= c 3) in
  when_satisfied sys (fun () -> incr count);
  (check int) "no fire yet" 0 count.contents;
  x =: 2;
  (check int) "still true" 0 count.contents;
  x =: 4;
  (check int) "false now" 0 count.contents;
  x =: 2;
  (check int) "fired" 1 count.contents

let test_when_satisfied_no_refire_while_true () =
  let open Formula in
  let count = ref 0 in
  let x = v 1 in
  let eq = x = c 1 in
  when_satisfied eq (fun () -> incr count);
  x =: 1;
  x =: 2;
  x =: 1;
  (check int) "fired once" 1 count.contents

let test_assign_non_term_raises () =
  let open Formula in
  let x = v 1 in
  let y = x + c 1 in
  let raised = ref false in
  (try y =: 5 with _ -> raised := true);
  (check bool) "raises on non-term assignment" true raised.contents

let test_exec_always_values () =
  let seen = ref [] in
  let open Formula in
  let s = make_source () in
  let x = v 0 in
  let test = x >= c 2 in
  exec_always s test (fun b -> seen := b :: seen.contents);
  listen s;
  (check (list bool)) "values" [false] seen.contents;
  x =: 1;
  listen s;
  (check (list bool)) "values" [false; false] seen.contents;
  x =: 2;
  listen s;
  (check (list bool)) "values" [true; false; false] seen.contents

let test_exec_while_stops () =
  let calls = ref 0 in
  let open Formula in
  let s = make_source () in
  let x = v 0 in
  exec_while s (x >= c 3) (fun () -> incr calls);
  listen s;
  x =: 1;
  listen s;
  x =: 2;
  listen s;
  x =: 3;
  listen s;
  (check int) "calls" 1 calls.contents;
  x =: 4;
  listen s;
  (check int) "calls" 2 calls.contents

let test_source_health () =
  let status = ref "alive" in
  let open Formula in
  let s = make_source () in
  let hp = v 2 in
  exec_while s (hp = c 0) (fun () -> status := "dead");
  hp =: 1;
  listen s;
  (check string) "alive" "alive" status.contents;
  hp =: 0;
  listen s;
  (check string) "dead" "dead" status.contents

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
      "arithmetic", [
        test_case "Subtraction" `Quick test_sub_int;
        test_case "Subtraction (float)" `Quick test_sub_float;
        test_case "Division" `Quick test_div_int;
        test_case "Division (float)" `Quick test_div_float;
        test_case "Deep dependency chain" `Quick test_deep_chain;
        test_case "String concatenation" `Quick test_string_concat;
        test_case "String concatenation (three)" `Quick test_string_concat_three;
        test_case "String concatenation (empty)" `Quick test_string_concat_empty;
        test_case "Named concat_strings" `Quick test_concat_named;
        test_case "Custom binary operator" `Quick test_custom_binop;
      ];
      "comparisons", [
        test_case "Greater than" `Quick test_gt_int;
        test_case "Greater than or equals" `Quick test_gte_int;
        test_case "Not equals" `Quick test_neq_int;
        test_case "Less than (after update)" `Quick test_lt_int_after_update;
        test_case "Less than / less than or equals (initial)" `Quick test_lt_lte_initial;
        test_case "Float comparisons" `Quick test_float_cmp;
      ];
      "systems", [
        test_case "And of two equations" `Quick test_sys_and;
        test_case "Or of two equations" `Quick test_sys_or;
        test_case "Nested systems" `Quick test_sys_nested;
      ];
      "bool-formula", [
        test_case "and_ of two bool terms" `Quick test_form_and;
        test_case "or_ of two bool terms" `Quick test_form_or;
        test_case "nested and_" `Quick test_form_and_nested;
        test_case "nested or_" `Quick test_form_or_nested;
        test_case "mixed and_/or_" `Quick test_form_and_or_mixed;
      ];
      "listeners", [
        test_case "on_change on formula" `Quick test_on_change_formula;
        test_case "on_change on term" `Quick test_on_change_term;
        test_case "when_satisfied refires" `Quick test_when_satisfied_refires;
        test_case "when_satisfied no refire while true" `Quick test_when_satisfied_no_refire_while_true;
        test_case "when_satisfied on and-system" `Quick test_when_satisfied_and;
      ];
      "errors", [
        test_case "Assigning to non-term raises" `Quick test_assign_non_term_raises;
      ];
      "event-listeners", [
        test_case "Simple when_satisfied" `Quick test_simple_sat;
        test_case "Player health test (Game Over)" `Quick test_player_health_go;
        test_case "Player health test (In Play)" `Quick test_player_health_ip;
      ];
      "sources", [
        test_case "Simple source test for exec_while" `Quick test_source_simple_while;
        test_case "Simple source test for exec_always" `Quick test_source_simple_always;
        test_case "exec_always receives values" `Quick test_exec_always_values;
        test_case "exec_while stops when false" `Quick test_exec_while_stops;
        test_case "Source health monitor" `Quick test_source_health;
      ];
  ]
