(** The fundamental types: terms (ref with superpowers!) and formula (combined terms). **)

(** A ref-like type with superpowers! *)
type term

(** A combination of terms that represent a mathematical formula *)
type formula

(** Lift basic types to term types. *)

(** Create term from float. *)
val ft : float -> formula

(** Create term from int. *)
val it : int -> formula
(** Create term from bool. *)
val bt : bool -> formula

(** Update term methods. *)

(** Update an int term to a new value. *)
val (=:) : formula -> int -> unit

(** Update a float term to a new value. *)
val (=:.) : formula -> float -> unit

(** Update a bool term to a new value. *)
val (=:|) : formula -> bool -> unit

(** Update a term by evaluating a formula *)
val (=::) : formula -> formula -> unit

(** Formula creation methods. *)

(** Arithmetic for terms. *)
val add_form : formula -> formula -> formula
val sub_form : formula -> formula -> formula
val mul_form : formula -> formula -> formula
val div_form : formula -> formula -> formula

(** Shorthand arithmetic for terms. *)
val (+) : formula -> formula -> formula
val (-) : formula -> formula -> formula
val ( * ) : formula -> formula -> formula
val (/) : formula -> formula -> formula


(** Shorthand arithmetic for terms. *)
val (+) : formula -> formula -> formula
val (-) : formula -> formula -> formula
val ( * ) : formula -> formula -> formula
val (/) : formula -> formula -> formula

(** Extract raw values from either terms of formula. *)

(** Extraction methods for formulas *)
val int_of_formula : formula -> int
val float_of_formula : formula -> float
val bool_of_formula : formula -> bool

(** Event listener constructors *)
val on_change : formula -> (unit -> unit) -> unit
