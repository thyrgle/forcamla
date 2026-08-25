(* Needed for trying to assign a complex formula a value. *)
exception NotATermException of string

(* For when_satisfied applied to non-boolean type. *)
exception NotABoolException of string

type any_formula = Any : _ formula -> any_formula
(* Stores the arithmetic expression of a formula. *)
and _ formula =
  Const : { const: 'a; parent: 'b formula} -> 'a formula
| Val : 
  {
    value : 'c ref;
    mutable parents: any_formula list;
    mutable on_change: ('c -> 'c -> unit) list;
  } -> 'c formula
| ValBool :
  {
    value : bool ref;
    mutable parents: any_formula list;
    mutable on_change: (bool -> bool -> unit) list;
    mutable when_satisfied: (unit -> unit) list;
  } -> bool formula
| UnaryOp : 
  {
    op : 'e -> 'f;
    child: 'e formula;
    mutable parents: any_formula list;
    cached_val: 'f ref;
    mutable on_change: ('f -> 'f -> unit) list;
  } -> 'f formula
| UnaryBool :
  {
    op : 'h -> bool;
    child: 'h formula;
    mutable parents: any_formula list;
    cached_val: bool ref;
    mutable on_change: (bool -> bool -> unit) list;
    mutable when_satisfied: (unit -> unit) list;
  } -> bool formula
| BinOp : 
  {
    op : 'a -> 'b -> 'c;
    lhs: 'a formula;
    rhs: 'b formula;
    mutable parents: any_formula list;
    mutable cached_val: 'c ref;
    mutable on_change: ('c -> 'c -> unit) list;
  } -> 'c formula
| BinBool : 
  {
    op : 'a -> 'b -> bool; 
    lhs: 'a formula;
    rhs: 'b formula;
    mutable parents: any_formula list; 
    cached_val: bool ref;
    mutable on_change: (bool -> bool -> unit) list;
    mutable when_satisfied: (unit -> unit) list
  } -> bool formula


(* Similar to 'a expr and equation_expr but for systems of equations. *)
type source =
{
  mutable exec_while : (bool formula * (unit -> unit)) list;
  mutable exec_always : (bool formula * (bool -> unit)) list;
}

(* Evaluate what an float expr currently should be. *)
let rec eval : type h. h formula -> h = function
    | Const {const; _} -> const
    | Val  {value; _} -> !value
    | ValBool {value; _} -> !value
    | UnaryOp {op; child; _} -> op (eval child)
    | UnaryBool {op; child; _} -> op (eval child)
    | BinOp {op; lhs; rhs; _} -> op (eval lhs) (eval rhs)
    | BinBool {op; lhs; rhs; _} -> op (eval lhs) (eval rhs)

(* Construct a formula of a single term. *)
let t (value: 'n): 'n formula =
  Val { parents=[]; value=ref value; on_change=[];}

let rec propagate : type i. i formula -> unit = fun f ->
  match f with
  | Const {const; _} -> ()
  | Val {value; _} -> ()
  | ValBool {value; _} -> ()
  | UnaryOp {cached_val; parents; on_change; _} ->
      let new_val = eval f in
      if new_val <> !cached_val then
        (List.iter (fun g -> g !cached_val new_val) on_change;
         cached_val := new_val;
         List.iter (fun (Any p) -> propagate p) parents)
      else ()
  | UnaryBool {cached_val; parents; on_change; when_satisfied; _} ->
      let new_val = eval f in
      if new_val <> !cached_val then
        (List.iter (fun g -> g !cached_val new_val) on_change;
         cached_val := new_val;
         List.iter (fun (Any p) -> propagate p) parents)
      else ();
      if new_val = true then
        List.iter (fun g -> g ()) when_satisfied
      else ()
  | BinOp {cached_val; parents; on_change; _} -> 
      let new_val = eval f in
      if new_val <> !cached_val then
        (List.iter (fun g -> g !cached_val new_val) on_change;
         cached_val := new_val;
         List.iter (fun (Any p) -> propagate p) parents)
      else ()
  | BinBool {cached_val; parents; on_change; when_satisfied; _} ->
      let new_val = eval f in
      if new_val <> !cached_val then
        (List.iter (fun g -> g !cached_val new_val) on_change;
         cached_val := new_val;
         List.iter (fun (Any p) -> propagate p) parents)
      else ();
      if new_val = true then
        List.iter (fun g -> g ()) when_satisfied
      else ()


let update_term (type j) (t: j formula) (new_val: j) =
  match t with
  | Val {value; parents; on_change} ->
      let old_val = !value in
      if old_val <> new_val then
        (value := new_val;
         List.iter (fun g -> g old_val new_val) on_change;
         List.iter (fun (Any p) -> propagate p) parents)
      else ()
  | _ -> raise (NotATermException "Cannot update a non-val")

(* Shorthand for update methods. *)
let (=:) = update_term

(* Extract values. Basically the same as (!) for reference types. *)
let (!) (f: 'o formula) = eval f

let add_parent (type i j) (parent: i formula) (child: j formula) : unit =
  let boxed = Any parent in
  match child with
  | Const _ -> ()
  | Val v -> v.parents <- boxed :: v.parents
  | ValBool vb -> vb.parents <- boxed :: vb.parents
  | UnaryOp u -> u.parents <- boxed :: u.parents
  | UnaryBool ub -> ub.parents <- boxed :: ub.parents
  | BinOp b -> b.parents <- boxed :: b.parents
  | BinBool bb -> bb.parents <- boxed :: bb.parents

(* Create a binary operation. *)
let reg_bin (f: 'k -> 'l -> 'm) =
  fun lhs rhs -> 
    let node = 
      BinOp 
      {
        op=f;
        parents=[];
        lhs=lhs;
        rhs=rhs;
        cached_val=ref (f (eval lhs) (eval rhs));
        on_change=[];
      } in
    add_parent node lhs;
    add_parent node rhs;
    node

let reg_bin_bool (f: 'k -> 'l -> bool) =
  fun lhs rhs -> 
    let node = 
      BinBool
      {
        op=f;
        parents=[];
        lhs=lhs;
        rhs=rhs;
        cached_val=ref (f (eval lhs) (eval rhs));
        on_change=[];
        when_satisfied=[];
      } in
    add_parent node lhs;
    add_parent node rhs;
    node



(* Arithmetic functions. *)

(* Addition of int typed formula. *)
let add_form_int lhs rhs = reg_bin (+) lhs rhs
let (+) = add_form_int

(* Subtraction of new types. *)
let sub_form_int lhs rhs = reg_bin (-) lhs rhs
let (-) = sub_form_int

(* Multiplication of new types. *)
let mul_form_int lhs rhs = reg_bin ( * ) lhs rhs
let ( * ) = mul_form_int

(* Division of new types. *)
let div_form_int lhs rhs = reg_bin (/) lhs rhs
let (/) = div_form_int

(* Addition of float typed formula. *)
let add_form_float lhs rhs = reg_bin (+.) lhs rhs
let (+.) = add_form_float

(* Subtraction of new types. *)
let sub_form_float lhs rhs = reg_bin (-.) lhs rhs
let (-.) = sub_form_float

(* Multiplication of new types. *)
let mul_form_float lhs rhs = reg_bin ( *. ) lhs rhs
let ( *. ) = mul_form_float

(* Division of new types. *)
let div_form_float lhs rhs = reg_bin (/.) lhs rhs
let (/.) = div_form_float

(* Concatenation of string formula. *)
let concat_strings lhs rhs = reg_bin (^) lhs rhs
let (^) = concat_strings

(* Logical and of boolean formula. *)
let and_ lhs rhs = reg_bin_bool (&&) lhs rhs
let (&&) = and_

(* Logical or of boolean formula *)
let or_ lhs rhs = reg_bin_bool (||) lhs rhs
let (||) = or_

(* Equality of two int formulas. *)
let eq_form lhs rhs = reg_bin_bool (=) lhs rhs
let (=) = eq_form

let neq_form lhs rhs = reg_bin_bool (<>) lhs rhs
let (<>) = neq_form

let gt_form lhs rhs = reg_bin_bool (>) lhs rhs
let (>) = gt_form

let gte_form lhs rhs = reg_bin_bool (>=) lhs rhs
let (>=) = gte_form

let lt_form lhs rhs = reg_bin_bool (<) lhs rhs
let (<) = lt_form

let lte_form lhs rhs = reg_bin_bool (<=) lhs rhs
let (<=) = lte_form

let make_source (): source =
{
  exec_while = []; 
  exec_always = [];
}

let listen (s: source): unit =
  (* Execute the exec_while functions if the condition is true. *)
  List.iter (fun pair -> if eval (fst pair) then (snd pair) () else ()) s.exec_while;
  (* Next execute the exec_always function regardless and supply the value of the system. *)
  List.iter (fun pair -> (snd pair) (eval (fst pair))) s.exec_always

(* Listeners *)
let on_change : type z. z formula -> (z -> z -> unit) -> unit =
  fun f g ->
    match f with
    | Const _ -> ()
    | Val v -> v.on_change <- g :: v.on_change
    | ValBool vb -> vb.on_change <- g :: vb.on_change
    | UnaryOp u -> u.on_change <- g :: u.on_change
    | UnaryBool ub -> ub.on_change <- g :: ub.on_change
    | BinOp b -> b.on_change <- g :: b.on_change
    | BinBool bb -> bb.on_change <- g :: bb.on_change

let when_satisfied (f: bool formula) (g: unit -> unit) =
  match f with
  | Const _ -> ()
  | Val v -> raise (NotABoolException "Must be applied to ValBool not Val")
  | ValBool vb -> vb.when_satisfied <- g :: vb.when_satisfied
  | UnaryOp u -> raise (NotABoolException "Must be applied to UnaryBool not UnaryOp")
  | UnaryBool ub -> ub.when_satisfied <- g :: ub.when_satisfied
  | BinOp b -> raise (NotABoolException "Must be applied to BinBool not BinOp")
  | BinBool bb -> bb.when_satisfied <- g :: bb.when_satisfied

let exec_always (src: source) (s: bool formula) (g: bool -> unit) = src.exec_always <- (s, g) :: src.exec_always
let exec_while (src: source) (s: bool formula) (g: unit -> unit) = src.exec_while <- (s, g) :: src.exec_while
