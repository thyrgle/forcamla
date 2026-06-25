(* Needed for trying to assign a complex formula a value. *)
exception NotATermException of string

(* Stores the arithmetic expression of a formula. *)
type 'a expr =
  Num of 'a ref
| Add of 'a expr * 'a expr
| Sub of 'a expr * 'a expr
| Mul of 'a expr * 'a expr
| Div of 'a expr * 'a expr
(* Basically 'a expr, but with some additional fields for updating. *)
and 'a formula =
{
  mutable parents: 'a formula list;
  mutable pred_parents: system list;
  mutable on_change: (unit -> unit) list;
  mutable value: 'a;
  expression: 'a expr;
  mutable needs_update: bool;
}
(* Similar to 'a expr but for equations instead of formula. *)
and equation_expr =
| EqInt of int formula * int formula
| EqFloat of float formula * float formula
| NeInt of int formula * int formula
| NeFloat of float formula * float formula
(* Similar to 'a expr and equation_expr but for systems of equations. *)
and system_expr =
| Single of equation_expr
| And of system * system
| Or of system * system
(* A wrapper around system_expr to allow for updating. *)
and system =
{
  mutable parents: system list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (unit -> unit) list;
  mutable value: bool;
  expression: system_expr;
  mutable needs_update: bool;
}

type source =
{
  mutable sys_func : (system * (unit -> unit)) list;
}

(* Evaluate what an int expr currently should be. *)
let rec eval_expr_int (e: int expr): int =
  match e with
    | Add (a, b)     -> (eval_expr_int a) + (eval_expr_int b)
    | Sub (a, b)     -> (eval_expr_int a) - (eval_expr_int b)
    | Mul (a, b)     -> (eval_expr_int a) * (eval_expr_int b)
    | Div (a, b)     -> (eval_expr_int a) / (eval_expr_int b)
    | Num x          -> !x

(* Evaluate what an float expr currently should be. *)
let rec eval_expr_float (e: float expr): float =
  match e with
    | Add (a, b) -> (eval_expr_float a) +. (eval_expr_float b)
    | Sub (a, b) -> (eval_expr_float a) -. (eval_expr_float b)
    | Mul (a, b) -> (eval_expr_float a) *. (eval_expr_float b)
    | Div (a, b) -> (eval_expr_float a) /. (eval_expr_float b)
    | Num x      -> !x

(* Evaluate a type *but* avoid unneeded updates with caching *)
let eval_type (eval: 'a expr -> 'a) (f: 'a formula): 'a = if f.needs_update then eval f.expression else f.value
let eval_equation (eval: system_expr -> 'a) (eq: system): bool = if eq.needs_update then eval eq.expression else eq.value

(* Specialization of the above "smart" evaluation. *)
let eval_int (f: int formula): int = eval_type eval_expr_int f
let eval_float (f: float formula): float = eval_type eval_expr_float f

let rec eval_expr_equation (e: equation_expr): bool =
  match e with
    | EqInt (a, b) -> (eval_int a) = (eval_int b)
    | EqFloat (a, b) -> (eval_float a) = (eval_float b)
    | NeInt (a, b) -> (eval_int a) <> (eval_int b)
    | NeFloat (a, b) -> (eval_float a) <> (eval_float b)

let rec eval_system (s: system): bool = match s.expression with
| Single a -> eval_expr_equation a
| And (a, b) -> (eval_system a) && (eval_system b)
| Or (a, b) -> (eval_system a) || (eval_system b)

let set_formula_needs_update (f: 'a formula) = f.needs_update <- true
let set_system_needs_update (s: system) = s.needs_update <- true

let rec propegate (eval: 'a expr -> 'a) (f: 'a formula): unit =
  List.iter (fun g -> g ()) f.on_change;
  List.iter (fun p -> p.needs_update <- true) f.parents;
  List.iter (fun (p: system) -> p.needs_update <- true) f.pred_parents;
  List.iter (update_a_formula eval) f.parents;
  List.iter update_system f.pred_parents;
  f.needs_update <- false
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
and update_system (s: system): unit =
  match s.expression with
  | Single a -> let new_val = eval_expr_equation a in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | And (a, b) -> let new_val = (eval_system a) && (eval_system b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)
  | Or (a, b) -> let new_val = (eval_system a) || (eval_system b) in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s)

let rec update_int_term (t: int formula) (new_val: int): unit = update_a_term eval_expr_int t new_val
and update_int_formula (f: int formula) = update_a_formula eval_expr_int f

let rec update_float_term (t: float formula) (new_val: float): unit = update_a_term eval_expr_float t new_val
and update_float_formula (f: float formula) = update_a_formula eval_expr_float f

(* Create a formula helper. *)
let formula_create (e: 'a expr) (value: 'a) =
{ 
  parents = []; 
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e;
  pred_parents = [];
}


(* Construct a formula of a single term. *)
let t (value: 'a): 'a formula = formula_create (Num (ref value)) value

(* Shorthand for update methods. *)
let (=:) = update_int_term
let (=:.) = update_float_term

(* Extract values. Basically the same as (!) for reference types. *)
let (!) (f: 'a formula) = f.value
let (!!) (s: system) = s.value

(* Arithmetic functions. *)

(* Create a binary operation that merges two formula into a more complex one. *)
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

(* Comparison operators. (Equation creation) *)

let system_create (e: system_expr) (value: bool): system =
{ 
  parents = [];
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e; 
  when_satisfied = [];
}

let equation_create (e: equation_expr) (value: bool): system = system_create (Single e) value

(* Forming new equations from formula and comparison operators. *)
let comp_form_a (comp: 'a -> 'a -> bool) 
                (mk_cmp : 'a formula -> 'a formula -> equation_expr)
                (f1: 'a formula) (f2: 'a formula): system =
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

(* System creation *)

let sys_make (op: bool -> bool -> bool)
             (mk_sys: system -> system -> system_expr)
             (s1: system) (s2: system): system =
  let sys = system_create (mk_sys s1 s2) (op s1.value s2.value) in
  s1.parents <- sys :: s1.parents;
  s2.parents <- sys :: s2.parents;
  sys

let and_eqs = sys_make (&&) (fun a b -> And (a, b))
let (&&) = and_eqs

let or_eqs = sys_make (||) (fun a b -> Or (a, b))
let (||) = or_eqs

(* Source functions. *)

let make_source () = { sys_func = []; }

let listen (s: source): unit =
  List.iter (fun pair -> if eval_system (fst pair) then (snd pair) () else ()) s.sys_func

(* Listeners *)
let on_change (f: 'a formula) (g: unit -> unit) = f.on_change <- g :: f.on_change
let system_change (f: system) (g: unit -> unit) = f.on_change <- g :: f.on_change
let when_satisfied (f: system) (g: unit -> unit) = f.when_satisfied <- g :: f.when_satisfied
let exec_while (src: source) (s: system) (g: unit -> unit) = src.sys_func <- (s, g) :: src.sys_func
