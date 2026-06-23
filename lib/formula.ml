exception NotATermException of string

type 'a expr =
  Num of 'a ref
| Add of 'a expr * 'a expr
| Sub of 'a expr * 'a expr
| Mul of 'a expr * 'a expr
| Div of 'a expr * 'a expr
and 'a formula =
{
  mutable parents: 'a formula list;
  mutable pred_parents: equation list;
  mutable on_change: (unit -> unit) list;
  mutable value: 'a;
  expression: 'a expr;
  mutable needs_update: bool;
}
and equation_expr =
| EqInt of int formula * int formula
| EqFloat of float formula * float formula
| NeInt of int formula * int formula
| NeFloat of float formula * float formula
and system_expr =
| AndEq of equation * equation
| OrEq of equation * equation
| AndSys of system * system
| OrSys of system * system
| AndSE of system * equation
| OrSE of system * equation
| AndES of equation * system
| OrES of equation * system
and equation =
{
  mutable parents: system list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (unit -> unit) list;
  mutable value: bool;
  expression: equation_expr;
  mutable needs_update: bool;
}
and system =
{
  mutable parents: system list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (unit -> unit) list;
  mutable value: bool;
  expression: system_expr;
  mutable needs_update: bool;
}

let rec eval_expr_int (e: int expr): int =
  match e with
    | Add (a, b)     -> (eval_expr_int a) + (eval_expr_int b)
    | Sub (a, b)     -> (eval_expr_int a) - (eval_expr_int b)
    | Mul (a, b)     -> (eval_expr_int a) * (eval_expr_int b)
    | Div (a, b)     -> (eval_expr_int a) / (eval_expr_int b)
    | Num x          -> !x

let rec eval_expr_float (e: float expr): float =
  match e with
    | Add (a, b) -> (eval_expr_float a) +. (eval_expr_float b)
    | Sub (a, b) -> (eval_expr_float a) -. (eval_expr_float b)
    | Mul (a, b) -> (eval_expr_float a) *. (eval_expr_float b)
    | Div (a, b) -> (eval_expr_float a) /. (eval_expr_float b)
    | Num x      -> !x
 
let eval_type (eval: 'a expr -> 'a) (f: 'a formula): 'a = if f.needs_update then eval f.expression else f.value

let eval_int (f: int formula): int = eval_type eval_expr_int f
let eval_float (f: float formula): float = eval_type eval_expr_float f
let eval_equation (eq: equation): bool = match eq.expression with
| EqInt (a, b) -> (eval_int a) = (eval_int b)
| EqFloat (a, b) -> (eval_float a) = (eval_float b)
| NeInt (a, b) -> (eval_int a) <> (eval_int b)
| NeFloat (a, b) -> (eval_float a) <> (eval_float b)
let rec eval_system (s: system): bool = match s.expression with
| AndEq (a, b) -> (eval_equation a) && (eval_equation b)
| OrEq (a, b) -> (eval_equation a) || (eval_equation b)
| AndSys (a, b) -> (eval_system a) && (eval_system b)
| OrSys (a, b) -> (eval_system a) || (eval_system b)
| AndSE (a, b) -> (eval_system a) && (eval_equation b)
| OrSE (a, b) -> (eval_system a) || (eval_equation b)
| AndES (a, b) -> (eval_equation a) && (eval_system b)
| OrES (a, b) -> (eval_equation a) || (eval_system b)

let set_formula_needs_update (f: 'a formula) = f.needs_update <- true
let set_equation_needs_update (eq: equation) = eq.needs_update <- true
let set_system_needs_update (s: system) = s.needs_update <- true

let rec propegate (eval: 'a expr -> 'a) (f: 'a formula): unit =
  List.iter (fun g -> g ()) f.on_change;
  List.iter set_formula_needs_update f.parents;
  List.iter set_equation_needs_update f.pred_parents;
  List.iter (update_a_formula eval) f.parents;
  List.iter update_equation f.pred_parents
and propegate_equation (eq: equation): unit =
  List.iter (fun g -> g ()) eq.on_change;
  if eq.value = true then 
    (List.iter (fun g -> g ()) eq.when_satisfied;
    List.iter set_system_needs_update eq.parents) else ()
and propegate_system (eq: system): unit =
  List.iter (fun g -> g ()) eq.on_change;
  if eq.value = true then 
    (List.iter (fun g -> g ()) eq.when_satisfied;
    List.iter set_system_needs_update eq.parents) else ()
and update_a_term (eval: 'a expr -> 'a) (f: 'a formula) (new_val: 'a) =
  match f.expression with
  | Num t ->
      if !t <> new_val then
     (t := new_val;
      f.value <- new_val;
      propegate eval f) else ()
  | _ -> raise (NotATermException "Formula is not a term and cannot be reassigned.")
and update_a_formula (eval: 'a expr -> 'a) (f: 'a formula) =
  let new_val = eval_type eval f in
  if f.value <> new_val then
    (f.value <- new_val;
     propegate eval f)
and update_equation (eq: equation): unit =
  match eq.expression with
  | EqInt (a, b) -> let new_val = (eval_int a) = (eval_int b) in
    if new_val <> eq.value then
      (eq.value <- new_val; propegate_equation eq)
  | EqFloat (a, b) -> let new_val = (eval_float a) = (eval_float b) in
    if new_val <> eq.value then
      (eq.value <- new_val; propegate_equation eq)
  | NeInt (a, b) -> let new_val = (eval_int a) <> (eval_int b) in
    if new_val <> eq.value then
      (eq.value <- new_val; propegate_equation eq)
  | NeFloat (a, b) -> let new_val = (eval_float a) <> (eval_float b) in
    if new_val <> eq.value then
      (eq.value <- new_val; propegate_equation eq)
and update_system (s: system): unit =
  match s.expression with
  | AndEq (a, b) -> let new_val = (eval_equation a) && (eval_equation b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | OrEq (a, b) -> let new_val = (eval_equation a) || (eval_equation b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | AndSys (a, b) -> let new_val = (eval_system a) && (eval_system b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | OrSys (a, b) -> let new_val = (eval_system a) || (eval_system b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | AndSE (a, b) -> let new_val = (eval_system a) && (eval_equation b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | OrSE (a, b) -> let new_val = (eval_system a) || (eval_equation b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | AndES (a, b) -> let new_val = (eval_equation a) && (eval_system b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | OrES (a, b) -> let new_val = (eval_equation a) || (eval_system b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)




let rec update_int_term (t: int formula) (new_val: int): unit = update_a_term eval_expr_int t new_val
and update_int_formula (f: int formula) = update_a_formula eval_expr_int f

let rec update_float_term (t: float formula) (new_val: float): unit = update_a_term eval_expr_float t new_val
and update_float_formula (f: float formula) = update_a_formula eval_expr_float f

let t (value: 'a): 'a formula =
{
  parents = [];
  pred_parents = [];
  on_change = [];
  value = value;
  expression = Num (ref value);
  needs_update = false;
  }

let (=:) = update_int_term
let (=:.) = update_float_term

(* Extract values. Basically the same as (!) for reference types. *)
let (!) (f: 'a formula) = f.value
let (!!) (eq: equation) = eq.value
let (&) (s: system) = s.value

(* Arithmetic functions. *)

let formula_create (e: 'a expr) (value: 'a) =
{ 
  parents = []; 
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e; 
  pred_parents = [];
}

let bin_form_a (op: 'a -> 'a -> 'a) (mk_expr: 'a expr -> 'a expr -> 'a expr) (f1: 'a formula) (f2: 'a formula): 'a formula =
  let f = formula_create (mk_expr f1.expression f2.expression) (op f1.value f2.value) in
  f1.parents <- f :: f1.parents;
  f2.parents <- f :: f2.parents;
  f

(* Addition of int typed formula. *)
let add_form_int = bin_form_a (+) (fun a b -> Add (a, b))
let (+) = add_form_int

(* Subtraction of new types. *)
let sub_form_int = bin_form_a (-) (fun a b -> Sub (a, b))
let (-) = sub_form_int

(* Multiplication of new types. *)
let mul_form_int = bin_form_a ( * ) (fun a b -> Mul (a, b))
let ( * ) = mul_form_int

(* Division of new types. *)
let div_form_int = bin_form_a (/) (fun a b -> Div (a, b))
let (/) = div_form_int

(* Addition of float typed formula. *)
let add_form_float = bin_form_a (+.) (fun a b -> Add (a, b))
let (+.) = add_form_float

(* Subtraction of new types. *)
let sub_form_float = bin_form_a (-.) (fun a b -> Sub (a, b))
let (-.) = sub_form_float

(* Multiplication of new types. *)
let mul_form_float = bin_form_a ( *. ) (fun a b -> Mul (a, b))
let ( *. ) = mul_form_float

(* Division of new types. *)
let div_form_float = bin_form_a (/.) (fun a b -> Div (a, b))
let (/.) = div_form_float

(* Comparison operators. *)

let equation_create (e: equation_expr) (value: bool): equation =
{ 
  parents = []; 
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e; 
  when_satisfied = [];
}

(* Equality of new types. *)
let comp_form_a (comp: 'a -> 'a -> bool) 
                (mk_cmp : 'a formula -> 'a formula -> equation_expr)
                (f1: 'a formula) (f2: 'a formula): equation =
  let eq = equation_create (mk_cmp f1 f2) (comp f1.value f2.value) in
  f1.pred_parents <- eq :: f1.pred_parents;
  f2.pred_parents <- eq :: f2.pred_parents;
  eq

(* Equality of two int formulas. *)
let eq_form_int = comp_form_a (=) (fun a b -> EqInt (a, b))
let (=?) = eq_form_int

(* Not equals of two int formulas. *)
let ne_form_int = comp_form_a (<>) (fun a b -> NeInt (a, b))
let (<>?) = ne_form_int

(* Equality of two float formulas. *)
let eq_form_float = comp_form_a (=) (fun a b -> EqFloat (a, b))
let (=.) = eq_form_float

(* Not equals of two float formulas. *)
let ne_form_float = comp_form_a (<>) (fun a b -> NeFloat (a, b))
let (<>.) = ne_form_float


(* Listeners *)
let on_change (f: 'a formula) (g: unit -> unit) = f.on_change <- g :: f.on_change
let when_satisfied (f: equation) (g: unit -> unit) = f.when_satisfied <- g :: f.when_satisfied
