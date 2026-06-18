exception CastError of string
exception UnsupportedType of string
exception ExtractionError of string

type data =
  Float of float
| Int of int
| Bool of bool
type expr =
  Num of term        
| Add of expr * expr
| Sub of expr * expr
| Mul of expr * expr
| Div of expr * expr
and term =
{
  mutable parents: formula list;
  mutable value: data;
}
and formula =
{
  mutable parents: formula list;
  mutable value: data;
  expression: expr;
  mutable needs_update: bool;
}

let add_datum (a: data) (b: data): data =
  match a with
  | Float x -> (match b with
    | Float y -> Float (x +. y)
    | _       -> raise (CastError "Terms cannot be added."))
  | Int x   -> (match b with
    | Int y   -> Int (x + y)
    | _       -> raise (CastError "Terms cannot be added."))
  | Bool _ -> raise (UnsupportedType "Cannot add booleans")


let sub_datum (a: data) (b: data): data =
  match a with
  | Float x -> (match b with
    | Float y -> Float (x -. y)
    | _       -> raise (CastError "Terms cannot be subtracted."))
  | Int x   -> (match b with
    | Int y   -> Int (x - y)
    | _       -> raise (CastError "Terms cannot be subtracted."))
  | Bool _ -> raise (UnsupportedType "Cannot subtract booleans")

let mul_datum (a: data) (b: data): data =
  match a with
  | Float x -> (match b with
    | Float y -> Float (x *. y)
    | _       -> raise (CastError "Terms cannot be multiplied."))
  | Int x   -> (match b with
    | Int y   -> Int (x * y)
    | _       -> raise (CastError "Terms cannot be multiplied."))
  | Bool _ -> raise (UnsupportedType "Cannot multiply booleans.")

let div_datum (a: data) (b: data): data =
  match a with
  | Float x -> (match b with
    | Float y -> Float (x -. y)
    | _       -> raise (CastError "Terms cannot be multiplied."))
  | Int x   -> (match b with
    | Int y   -> Int (x - y)
    | _       -> raise (CastError "Terms cannot be multiplied."))
  | Bool _ -> raise (UnsupportedType "Cannot multiply booleans.")

let rec eval_expr (e: expr): data =
  match e with
    | Add (a, b) -> add_datum (eval_expr a) (eval_expr b)
    | Sub (a, b) -> sub_datum (eval_expr a) (eval_expr b)
    | Mul (a, b) -> mul_datum (eval_expr a) (eval_expr b)
    | Div (a, b) -> div_datum (eval_expr a) (eval_expr b)
    | Num x -> x.value
  

let eval (e: formula): data =
  match e.needs_update with
  | true -> eval_expr e.expression
  | false -> e.value

let rec update_formula (f: formula) =
  let old_val = f.value in
  let new_val = eval f in
  if old_val <> new_val then
    (f.value <- new_val;
    List.iter update_formula f.parents)
  else ()

let update_term (t: term) (new_val: data): unit = 
  t.value <- new_val;
  List.iter (fun p -> p.needs_update <- true) t.parents;
  List.iter update_formula t.parents

let ft (value: float): term = { parents = []; value = Float value }
let it (value: int): term = { parents = []; value = Int value }
let bt (value: bool): term = { parents = []; value = Bool value }

let (=:) (t: term) (value: int) = update_term t (Int value)
let (=:.) (t: term) (value: float) = update_term t (Float value)
let (=:|) (t: term) (value: bool) = update_term t (Bool value)

(* Arithmetic functions. *)

(* Addition of new types. *)
let add_term_term (t1: term) (t2: term): formula = 
  let f = 
  {
    parents = [];
    value = add_datum t1.value t2.value;
    expression = Add (Num t1, Num t2);
    needs_update = false;
  } in
  t1.parents <- f :: t1.parents;
  t2.parents <- f :: t2.parents;
  f

let ($+$) = add_term_term

let add_form_term (f: formula) (t: term): formula = 
  let new_f = 
  {
    parents = [];
    value = add_datum f.value t.value;
    expression = Add (f.expression, Num t);
    needs_update = false;
  } in
  f.parents <- new_f :: f.parents;
  t.parents <- new_f :: t.parents;
  new_f

let (&+$) = add_form_term

let add_term_form (t: term) (f: formula): formula = 
  let new_f = {
    parents = [];
    value = add_datum t.value f.value;
    expression = Add (Num t, f.expression);
    needs_update = false;
  } in
  t.parents <- new_f :: t.parents;
  f.parents <- new_f :: f.parents;
  new_f

let ($+&) = add_term_form

let add_form_form (f1: formula) (f2: formula): formula = 
  let f = 
  {
    parents = [];
    value = add_datum f1.value f2.value;
    expression = Add (f1.expression, f2.expression);
    needs_update = false;
  } in 
  f1.parents <- f :: f1.parents;
  f2.parents <- f :: f2.parents;
  f

let (&+&) = add_form_form

(* Subtraction of new types. *)
let sub_term_term (t1: term) (t2: term): formula =
  let f = 
  {
    parents = [];
    value = sub_datum t1.value t2.value;
    expression = Sub (Num t1, Num t2);
    needs_update = false;
  } in
  t1.parents <- f :: t1.parents;
  t2.parents <- f :: t2.parents;
  f

let ($-$) = sub_term_term

let sub_form_term (f: formula) (t: term): formula = 
  let new_f =
  {
    parents = [];
    value = sub_datum f.value t.value;
    expression = Sub (f.expression, Num t);
    needs_update = false;
  } in
  f.parents <- new_f :: f.parents;
  t.parents <- new_f :: t.parents;
  new_f

let (&-$) = sub_form_term

let sub_term_form (t: term) (f: formula): formula = 
  let new_f =
  {
    parents = [];
    value = sub_datum t.value f.value;
    expression = Sub (Num t, f.expression);
    needs_update = false;
  } in
  t.parents <- new_f :: t.parents;
  f.parents <- new_f :: f.parents;
  new_f

let ($-&) = sub_term_form

let sub_form_form (f1: formula) (f2: formula): formula = 
  let f = 
  {
    parents = [];
    value = sub_datum f1.value f2.value;
    expression = Sub (f1.expression, f2.expression);
    needs_update = false;
  } in
  f1.parents <- f :: f1.parents;
  f2.parents <- f :: f2.parents;
  f

let (&-&) = sub_form_form

(* Multiplication of new types. *)
let mul_term_term (t1: term) (t2: term): formula = 
  let f =
  {
    parents = [];
    value = mul_datum t1.value t2.value;
    expression = Mul (Num t1, Num t2);
    needs_update = false;
  } in
  t1.parents <- f :: t1.parents;
  t2.parents <- f :: t2.parents;
  f

let ($*$) = mul_term_term

let mul_form_term (f: formula) (t: term): formula = 
  let new_f =
  {
    parents = [];
    value = mul_datum f.value t.value;
    expression = Mul (f.expression, Num t);
    needs_update = false;
  } in
  f.parents <- new_f :: f.parents;
  t.parents <- new_f :: t.parents;
  new_f

let (&*$) = mul_form_term

let mul_term_form (t: term) (f: formula): formula = 
  let new_f =
  {
    parents = [];
    value = mul_datum t.value f.value;
    expression = Mul (Num t, f.expression);
    needs_update = false;
  } in
  t.parents <- new_f :: t.parents;
  f.parents <- new_f :: f.parents;
  new_f

let ($*&) = mul_term_form

let mul_form_form (f1: formula) (f2: formula): formula = 
  let f =
  {
    parents = [];
    value = mul_datum f1.value f2.value;
    expression = Mul (f1.expression, f2.expression);
    needs_update = false;
  } in
  f1.parents <- f :: f1.parents;
  f2.parents <- f :: f2.parents;
  f

let (&*&) = mul_form_form

(* Division of new types. *)
let div_term_term (t1: term) (t2: term): formula = 
  let f =
  {
    parents = [];
    value = div_datum t1.value t2.value;
    expression = Div (Num t1, Num t2);
    needs_update = false;
  } in
  t1.parents <- f :: t1.parents;
  t2.parents <- f :: t2.parents;
  f

let ($/$) = add_term_term

let div_form_term (f: formula) (t: term): formula = 
  let new_f = 
  {
    parents = [];
    value = div_datum f.value t.value;
    expression = Div (f.expression, Num t);
    needs_update = false;
  } in
  f.parents <- new_f :: f.parents;
  t.parents <- new_f :: t.parents;
  new_f

let (&/$) = div_form_term

let div_term_form (t: term) (f: formula): formula = 
  let new_f = 
  {
    parents = [];
    value = div_datum t.value f.value;
    expression = Div (Num t, f.expression);
    needs_update = false;
  } in
  t.parents <- new_f :: t.parents;
  f.parents <- new_f :: f.parents;
  new_f

let ($/&) = div_term_form

let div_form_form (f1: formula) (f2: formula): formula = 
  let f = 
  {
    parents = [];
    value = div_datum f1.value f2.value;
    expression = Div (f1.expression, f2.expression);
    needs_update = false;
  } in
  f1.parents <- f :: f1.parents;
  f2.parents <- f :: f2.parents;
  f

let (&/&) = div_form_form

(* Extraction methods. *)

(* Integer extraction methods. *)
let int_of_formula (f: formula): int =
  match f.value with
    | Int v -> v
    | _       -> raise (ExtractionError "Formula is not int.")

let int_of_term (f: term): int =
  match f.value with
    | Int v -> v
    | _       -> raise (ExtractionError "Term is not int.")

(* Float extraction methods. *)
let float_of_formula (f: formula): float =
  match f.value with
    | Float v -> v
    | _       -> raise (ExtractionError "Formula is not float.")

let float_of_term (f: term): float =
  match f.value with
    | Float v -> v
    | _       -> raise (ExtractionError "Term is not float.")

(* Boolean extraction methods. *)
let bool_of_formula (f: formula): bool =
  match f.value with
    | Bool v -> v
    | _       -> raise (ExtractionError "Formula is not bool.")

let bool_of_term (f: term): bool =
  match f.value with
    | Bool v -> v
    | _       -> raise (ExtractionError "Term is not bool.")
