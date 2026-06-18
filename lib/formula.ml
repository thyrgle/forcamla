exception CastError of string
exception UnsupportedType of string
exception ExtractionError of string
exception AssignmentError of string

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
and compound =
{
  mutable parents: formula list;
  mutable value: data;
  expression: expr;
  mutable needs_update: bool;
}
and formula = Compound of compound | Term of term

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
  

let eval (f: formula): data =
  match f with
  | Compound c ->
    (match c.needs_update with
    | true -> eval_expr c.expression
    | false -> c.value)
  | Term t -> t.value

let set_needs_update (f: formula) =
  match f with
  | Compound c -> c.needs_update <- true
  | Term t -> ()

let rec update_term (t: term) (new_val: data): unit = 
  (t.value <- new_val;
  List.iter set_needs_update t.parents;
  List.iter update_formula t.parents)
and update_formula (f: formula) =
  match f with
  | Compound c ->
    (let old_val = c.value in
    let new_val = eval f in
    if old_val <> new_val then
      (c.value <- new_val;
      List.iter update_formula c.parents)
    else ())
  | Term t -> () (* Update formula on a term should do nothing because only update_term changes it. *)

let ft (value: float): formula = Term { parents = []; value = Float value }
let it (value: int): formula = Term { parents = []; value = Int value }
let bt (value: bool): formula = Term { parents = []; value = Bool value }

let (=:) (f: formula) (value: int) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_term t (Int value)
let (=:.) (f: formula) (value: float) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_term t (Float value)
let (=:|) (f: formula) (value: bool) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_term t (Bool value)
let (=::) (f: formula) (value: formula) = match f with
  | Compound c -> raise (AssignmentError "Cannot assign compound formula.")
  | Term t -> update_term t (eval value)

(* Arithmetic functions. *)

(* Addition of new types. *)
let add_form (f1: formula) (f2: formula): formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = add_datum c1.value c2.value;
      expression = Add (c1.expression, c2.expression);
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = add_datum c1.value t2.value;
      expression = Add (c1.expression, Num t2);
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
      value = add_datum t1.value c2.value;
      expression = Add (Num t1, c2.expression);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = add_datum t1.value t2.value;
      expression = Add (Num t1, Num t2);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)


let (+) = add_form

(* Subtraction of new types. *)
let sub_form (f1: formula) (f2: formula): formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = sub_datum c1.value c2.value;
      expression = Sub (c1.expression, c2.expression);
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = sub_datum c1.value t2.value;
      expression = Sub (c1.expression, Num t2);
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
      value = sub_datum t1.value c2.value;
      expression = Sub (Num t1, c2.expression);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = sub_datum t1.value t2.value;
      expression = Sub (Num t1, Num t2);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)


let (-) = sub_form

(* Multiplication of new types. *)
let mul_form (f1: formula) (f2: formula): formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = mul_datum c1.value c2.value;
      expression = Mul (c1.expression, c2.expression);
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = mul_datum c1.value t2.value;
      expression = Mul (c1.expression, Num t2);
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
      value = mul_datum t1.value c2.value;
      expression = Mul (Num t1, c2.expression);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = mul_datum t1.value t2.value;
      expression = Mul (Num t1, Num t2);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)

let ( * ) = mul_form

(* Division of new types. *)
let div_form (f1: formula) (f2: formula): formula = 
  match f1 with
  | Compound c1 ->
   (match f2 with
    | Compound c2 -> let f =
    {
      parents = [];
      value = div_datum c1.value c2.value;
      expression = Div (c1.expression, c2.expression);
      needs_update = false;
    } in
    c1.parents <- Compound f :: c1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = div_datum c1.value t2.value;
      expression = Div (c1.expression, Num t2);
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
      value = div_datum t1.value c2.value;
      expression = Div (Num t1, c2.expression);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    c2.parents <- Compound f :: c2.parents;
    Compound f
    | Term t2 -> let f =
    {
      parents = [];
      value = div_datum t1.value t2.value;
      expression = Div (Num t1, Num t2);
      needs_update = false;
    } in
    t1.parents <- Compound f :: t1.parents;
    t2.parents <- Compound f :: t2.parents;
    Compound f)

let (/) = div_form

(* Extraction methods. *)

(* Integer extraction method. *)
let int_of_formula (f: formula): int =
  match f with
  | Compound c -> 
    (match c.value with
     | Int v -> v
     | _     -> raise (ExtractionError "Formula does not evaluate to int."))
  | Term t ->
    (match t.value with
     | Int v -> v
     | _    -> raise (ExtractionError "Term does not evaluate to int."))

(* Float extraction method. *)
let float_of_formula (f: formula): float =
  match f with
  | Compound c -> 
    (match c.value with
     | Float v -> v
     | _     -> raise (ExtractionError "Formula does not evaluate to int."))
  | Term t ->
    (match t.value with
     | Float v -> v
     | _    -> raise (ExtractionError "Term does not evaluate to int."))


(* Boolean extraction method. *)
let bool_of_formula (f: formula): bool =
  match f with
  | Compound c -> 
    (match c.value with
     | Bool v -> v
     | _     -> raise (ExtractionError "Formula does not evaluate to int."))
  | Term t ->
    (match t.value with
     | Bool v -> v
     | _    -> raise (ExtractionError "Term does not evaluate to int."))
