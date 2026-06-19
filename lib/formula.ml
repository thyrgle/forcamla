exception AssignmentError of string

type 'a expr =
  Num of 'a term
| Add of 'a expr * 'a expr
| Sub of 'a expr * 'a expr
| Mul of 'a expr * 'a expr
| Div of 'a expr * 'a expr
and 'a term =
{
  mutable parents: 'a formula list;
  mutable on_change: (unit -> unit) list;
  mutable value: 'a;
}
and 'a compound =
{
  mutable parents: 'a formula list;
  mutable value: 'a;
  expression: 'a expr;
  mutable on_change: (unit -> unit) list;
  mutable needs_update: bool;
}
and 'a formula = Compound of 'a compound | Term of 'a term

let rec eval_expr_int (e: int expr): int =
  match e with
    | Add (a, b) -> (eval_expr_int a) + (eval_expr_int b)
    | Sub (a, b) -> (eval_expr_int a) - (eval_expr_int b)
    | Mul (a, b) -> (eval_expr_int a) * (eval_expr_int b)
    | Div (a, b) -> (eval_expr_int a) / (eval_expr_int b)
    | Num x -> x.value

let rec eval_expr_float (e: float expr): float =
  match e with
    | Add (a, b) -> (eval_expr_float a) +. (eval_expr_float b)
    | Sub (a, b) -> (eval_expr_float a) -. (eval_expr_float b)
    | Mul (a, b) -> (eval_expr_float a) *. (eval_expr_float b)
    | Div (a, b) -> (eval_expr_float a) /. (eval_expr_float b)
    | Num x -> x.value

  
let eval_int (f: int formula): int =
  match f with
  | Compound c ->
    (match c.needs_update with
    | true -> eval_expr_int c.expression
    | false -> c.value)
  | Term t -> t.value

let eval_float (f: float formula): float =
  match f with
  | Compound c ->
    (match c.needs_update with
    | true -> eval_expr_float c.expression
    | false -> c.value)
  | Term t -> t.value

let set_needs_update (f: 'a formula) =
  match f with
  | Compound c -> c.needs_update <- true
  | Term t -> ()

let rec update_int_term (t: int term) (new_val: 'a): unit = 
  (t.value <- new_val;
  List.iter set_needs_update t.parents;
  List.iter update_formula t.parents)
and update_formula (f: int formula) =
  match f with
  | Compound c ->
    (let old_val = c.value in
    let new_val = eval_int f in
    if old_val <> new_val then
      (c.value <- new_val;
      List.iter update_formula c.parents)
    else ())
  | Term t -> () (* Update formula on a term should do nothing because only update_term changes it. *)

let rec update_float_term (t: float term) (new_val: 'a): unit = 
  (t.value <- new_val;
  List.iter set_needs_update t.parents;
  List.iter update_formula t.parents)
and update_formula (f: float formula) =
  match f with
  | Compound c ->
    (let old_val = c.value in
    let new_val = eval_float f in
    if old_val <> new_val then
      (c.value <- new_val;
      List.iter update_formula c.parents)
    else ())
  | Term t -> () (* Update formula on a term should do nothing because only update_term changes it. *)

let it (value: int): int formula = Term { parents = []; on_change = []; value = value }
let ft (value: float): float formula = Term { parents = []; on_change = []; value = value }
let bt (value: bool): bool formula = Term { parents = []; on_change = []; value = value }

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

(* Addition of int typed formula. *)
let add_form_int (f1: int formula) (f2: int formula): int formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value + c2.value;
      expression = Add (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value + t2.value;
      expression = Add (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value + c2.value;
      expression = Add (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value + t2.value;
      expression = Add (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)


let (+) = add_form_int

(* Subtraction of new types. *)
let sub_form_int (f1: int formula) (f2: int formula): int formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value - c2.value;
      expression = Sub (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value - t2.value;
      expression = Sub (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value - c2.value;
      expression = Sub (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value - t2.value;
      expression = Sub (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)


let (-) = sub_form_int

(* Multiplication of new types. *)
let mul_form_int (f1: int formula) (f2: int formula): int formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value * c2.value;
      expression = Mul (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value * t2.value;
      expression = Mul (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value * c2.value;
      expression = Mul (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value * t2.value;
      expression = Mul (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)

let ( * ) = mul_form_int

(* Division of new types. *)
let div_form_int (f1: int formula) (f2: int formula): int formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value / c2.value;
      expression = Div (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value / t2.value;
      expression = Div (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value / c2.value;
      expression = Div (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value / t2.value;
      expression = Div (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)

let (/) = div_form_int


(* Addition of float typed formula. *)
let add_form_float (f1: float formula) (f2: float formula): float formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value +. c2.value;
      expression = Add (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value +. t2.value;
      expression = Add (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value +. c2.value;
      expression = Add (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value +. t2.value;
      expression = Add (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)


let (+.) = add_form_float

(* Subtraction of new types. *)
let sub_form_float (f1: float formula) (f2: float formula): float formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value -. c2.value;
      expression = Sub (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value -. t2.value;
      expression = Sub (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value -. c2.value;
      expression = Sub (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value -. t2.value;
      expression = Sub (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)


let (-.) = sub_form_float

(* Multiplication of new types. *)
let mul_form_float (f1: float formula) (f2: float formula): float formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value *. c2.value;
      expression = Mul (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value *. t2.value;
      expression = Mul (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value *. c2.value;
      expression = Mul (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value *. t2.value;
      expression = Mul (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)

let ( *. ) = mul_form_float

(* Division of new types. *)
let div_form_float (f1: float formula) (f2: float formula): float formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = c1.value /. c2.value;
      expression = Div (c1.expression, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = c1.value /. t2.value;
      expression = Div (c1.expression, Num t2);
      on_change = [];
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)
  | Term t1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = t1.value /. c2.value;
      expression = Div (Num t1, c2.expression);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = t1.value /. t2.value;
      expression = Div (Num t1, Num t2);
      on_change = [];
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)

let (/.) = div_form_float

(* Listeners *)
let on_change (f: 'a formula) (g: unit -> unit) = match f with
  | Compound c -> c.on_change <- g :: c.on_change
  | Term t -> t.on_change <- g :: t.on_change
