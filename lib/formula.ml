exception AssignmentError of string
exception PredicateError of string
exception NotPredicateError of string

type 'a expr =
  Num of 'a term
| Add of 'a expr * 'a expr
| Sub of 'a expr * 'a expr
| Mul of 'a expr * 'a expr
| Div of 'a expr * 'a expr
| EqInt of int expr * int expr
| EqFloat of float expr * float expr
| NeInt of int expr * int expr
| NeFloat of float expr * float expr
and 'a term =
{
  mutable parents: 'a formula list;
  mutable pred_parents: bool formula list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (unit -> unit) list;
  mutable value: 'a;
}
and 'a compound =
{
  mutable parents: 'a formula list;
  mutable pred_parents: bool formula list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (unit -> unit) list;
  mutable value: 'a;
  expression: 'a expr;
  mutable needs_update: bool;
}
and 'a formula = Compound of 'a compound | Term of 'a term

let rec eval_expr_int (e: int expr): int =
  match e with
    | Add (a, b)     -> (eval_expr_int a) + (eval_expr_int b)
    | Sub (a, b)     -> (eval_expr_int a) - (eval_expr_int b)
    | Mul (a, b)     -> (eval_expr_int a) * (eval_expr_int b)
    | Div (a, b)     -> (eval_expr_int a) / (eval_expr_int b)
    | Num x          -> x.value
    | _              -> raise (NotPredicateError "Formula is type int, not a predicate")

let rec eval_expr_float (e: float expr): float =
  match e with
    | Add (a, b) -> (eval_expr_float a) +. (eval_expr_float b)
    | Sub (a, b) -> (eval_expr_float a) -. (eval_expr_float b)
    | Mul (a, b) -> (eval_expr_float a) *. (eval_expr_float b)
    | Div (a, b) -> (eval_expr_float a) /. (eval_expr_float b)
    | Num x      -> x.value
    | _          -> raise (NotPredicateError "Formula is type float, not a predicate")
 
let eval_expr_bool (e: bool expr): bool =
  match e with
    | EqInt (a, b) -> (eval_expr_int a) = (eval_expr_int b)
    | EqFloat (a, b) -> (eval_expr_float a) = (eval_expr_float b)
    | NeInt (a, b) -> (eval_expr_int a) <> (eval_expr_int b)
    | NeFloat (a, b) -> (eval_expr_float a) <> (eval_expr_float b)
    | _ -> raise (PredicateError "Formula is a predicate cannot perform operation")

let eval_type (eval: 'a expr -> 'a) (f: 'a formula): 'a =
  match f with
  | Compound c ->
    (match c.needs_update with
    | true -> eval c.expression
    | false -> c.value)
  | Term t -> t.value

let eval_int (f: int formula): int = eval_type eval_expr_int f
let eval_float (f: float formula): float = eval_type eval_expr_float f
let eval_bool (f: bool formula): bool = eval_type eval_expr_bool f

let set_needs_update (f: 'a formula) =
  match f with
  | Compound c -> c.needs_update <- true
  | Term t -> ()

let update_formula_bool (f: bool formula) =
  match f with
  | Compound c ->
    let old_val = c.value in
    let new_val = eval_bool f in
    if old_val <> new_val then
      (c.value <- new_val;
      List.iter (fun g -> g ()) c.on_change;
      (if new_val = true then (List.iter (fun g -> g ()) c.when_satisfied) else ()))
    else ()
  | Term t -> () (* Update formula on a term should do nothing because only update_term changes it. *)

let rec update_a_term (eval: 'a expr -> 'a) (t: 'a term) (new_val: 'a) =
  t.value <- new_val;
  List.iter set_needs_update t.parents;
  List.iter set_needs_update t.pred_parents;
  List.iter (update_a_formula eval) t.parents;
  List.iter update_formula_bool t.pred_parents
and update_a_formula (eval: 'a expr -> 'a) (f: 'a formula) =
  match f with
  | Compound c ->
    (let old_val = c.value in
    let new_val = eval_type eval f in
    if old_val <> new_val then
      (c.value <- new_val;
      List.iter (fun g -> g ()) c.on_change;
      List.iter set_needs_update c.parents;
      List.iter set_needs_update c.pred_parents;
      List.iter (update_a_formula eval) c.parents;
      List.iter update_formula_bool c.pred_parents)
    else ())
  | Term t -> () (* Update formula on a term should do nothing because only update_term changes it. *)

let rec update_int_term (t: int term) (new_val: int): unit = update_a_term eval_expr_int t new_val
and update_int_formula (f: int formula) = update_a_formula eval_expr_int f

let rec update_float_term (t: float term) (new_val: float): unit = update_a_term eval_expr_float t new_val
and update_int_formula (f: float formula) = update_a_formula eval_expr_float f

let at (value: 'a): 'a formula =
  Term {
    parents = [];
    pred_parents = [];
    on_change = [];
    value = value;
    when_satisfied = [];
  }
let it (value: int): int formula = at value
let ft (value: float): float formula = at value

let (=:) (f: int formula) (value: int) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_int_term t value
let (=:.) (f: float formula) (value: float) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_float_term t value
let (=::) (f: int formula) (value: int formula) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_int_term t (eval_int value)
let (=::.) (f: float formula) (value: float formula) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_float_term t (eval_float value)

(* Extract values. Basically the same as (!) for reference types. *)
let (!) (f: 'a formula) = match f with
  | Compound c -> c.value
  | Term t -> t.value

(* Arithmetic functions. *)

let formula_create (e: 'a expr) (value: 'a) =
{ 
  parents = []; 
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e; 
  pred_parents = [];
  when_satisfied = [];
}

(* Addition of int typed formula. *)
let add_form_int (f1: int formula) (f2: int formula): int formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Add (c1.expression, c2.expression)) (c1.value + c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Add (c1.expression, Num t2)) (c1.value + t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Add (Num t1, c2.expression)) (t1.value + c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Add (Num t1, Num t2)) (t1.value + t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let (+) = add_form_int

(* Subtraction of new types. *)
let sub_form_int (f1: int formula) (f2: int formula): int formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Sub (c1.expression, c2.expression)) (c1.value - c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Sub (c1.expression, Num t2)) (c1.value - t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Sub (Num t1, c2.expression)) (t1.value - c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Sub (Num t1, Num t2)) (t1.value - t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let (-) = sub_form_int

(* Multiplication of new types. *)
let mul_form_int (f1: int formula) (f2: int formula): int formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Mul (c1.expression, c2.expression)) (c1.value * c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Mul (c1.expression, Num t2)) (c1.value * t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Mul (Num t1, c2.expression)) (t1.value * c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Mul (Num t1, Num t2)) (t1.value * t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let ( * ) = mul_form_int

(* Division of new types. *)
let div_form_int (f1: int formula) (f2: int formula): int formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Div (c1.expression, c2.expression)) (c1.value / c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Div (c1.expression, Num t2)) (c1.value / t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Div (Num t1, c2.expression)) (t1.value / c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Div (Num t1, Num t2)) (t1.value / t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let (/) = div_form_int

(* Addition of float typed formula. *)
let add_form_float (f1: float formula) (f2: float formula): float formula = 
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Add (c1.expression, c2.expression)) (c1.value +. c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Add (c1.expression, Num t2)) (c1.value +. t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Add (Num t1, c2.expression)) (t1.value +. c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Add (Num t1, Num t2)) (t1.value +. t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f


let (+.) = add_form_float

(* Subtraction of new types. *)
let sub_form_float (f1: float formula) (f2: float formula): float formula = 
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Sub (c1.expression, c2.expression)) (c1.value -. c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Sub (c1.expression, Num t2)) (c1.value -. t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Sub (Num t1, c2.expression)) (t1.value -. c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Sub (Num t1, Num t2)) (t1.value -. t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let (-.) = sub_form_float

(* Multiplication of new types. *)
let mul_form_float (f1: float formula) (f2: float formula): float formula = 
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Mul (c1.expression, c2.expression)) (c1.value *. c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Mul (c1.expression, Num t2)) (c1.value *. t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Mul (Num t1, c2.expression)) (t1.value *. c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Mul (Num t1, Num t2)) (t1.value *. t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let ( *. ) = mul_form_float

(* Division of new types. *)
let div_form_float (f1: float formula) (f2: float formula): float formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (Div (c1.expression, c2.expression)) (c1.value /. c2.value) in
      c1.parents <- Compound f :: c1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (Div (c1.expression, Num t2)) (c1.value /. t2.value) in
      c1.parents <- Compound f :: c1.parents;
      t2.parents <- Compound f :: t2.parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (Div (Num t1, c2.expression)) (t1.value /. c2.value) in
      t1.parents <- Compound f :: t1.parents;
      c2.parents <- Compound f :: c2.parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (Div (Num t1, Num t2)) (t1.value /. t2.value) in
       t1.parents <- Compound f :: t1.parents;
       t2.parents <- Compound f :: t2.parents;
       Compound f

let (/.) = div_form_float

(* Equality of new types. (Only supports two int formulas) *)
let eq_form_int (f1: int formula) (f2: int formula): bool formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (EqInt (c1.expression, c2.expression)) (c1.value = c2.value) in
      c1.pred_parents <- Compound f :: c1.pred_parents;
      c2.pred_parents <- Compound f :: c2.pred_parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (EqInt (c1.expression, Num t2)) (c1.value = t2.value) in
      c1.pred_parents <- Compound f :: c1.pred_parents;
      t2.pred_parents <- Compound f :: t2.pred_parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (EqInt (Num t1, c2.expression)) (t1.value = c2.value) in
      t1.pred_parents <- Compound f :: t1.pred_parents;
      c2.pred_parents <- Compound f :: c2.pred_parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (EqInt (Num t1, Num t2)) (t1.value = t2.value) in
       t1.pred_parents <- Compound f :: t1.pred_parents;
       t2.pred_parents <- Compound f :: t2.pred_parents;
       Compound f

let (=?) = eq_form_int

let eq_form_float (f1: float formula) (f2: float formula): bool formula =
  match (f1, f2) with
  | (Compound c1, Compound c2) -> 
      let f = formula_create (EqFloat (c1.expression, c2.expression)) (c1.value = c2.value) in
      c1.pred_parents <- Compound f :: c1.pred_parents;
      c2.pred_parents <- Compound f :: c2.pred_parents;
      Compound f
  | (Compound c1, Term t2) ->
      let f = formula_create (EqFloat (c1.expression, Num t2)) (c1.value = t2.value) in
      c1.pred_parents <- Compound f :: c1.pred_parents;
      t2.pred_parents <- Compound f :: t2.pred_parents;
      Compound f
  | (Term t1, Compound c2) ->
      let f = formula_create (EqFloat (Num t1, c2.expression)) (t1.value = c2.value) in
      t1.pred_parents <- Compound f :: t1.pred_parents;
      c2.pred_parents <- Compound f :: c2.pred_parents;
      Compound f
    | (Term t1, Term t2) ->
       let f = formula_create (EqFloat (Num t1, Num t2)) (t1.value = t2.value) in
       t1.pred_parents <- Compound f :: t1.pred_parents;
       t2.pred_parents <- Compound f :: t2.pred_parents;
       Compound f

let (=.) = eq_form_float

(* Listeners *)
let on_change (f: 'a formula) (g: unit -> unit) = match f with
  | Compound c -> c.on_change <- g :: c.on_change
  | Term t -> t.on_change <- g :: t.on_change

let when_satisfied (f: bool formula) (g: unit -> unit) = match f with
  | Compound c -> c.when_satisfied <- g :: c.when_satisfied
  | Term t -> t.when_satisfied <- g :: t.when_satisfied
