(** The fundamental types: terms (ref with superpowers!) and formula (combined terms). **)

(** A ref-like type with superpowers! *)
type term

(** A combination of terms that represent a mathematical formula *)
type formula

(** Lift basic types to term types. *)

(** Create term from float. *)
val ft : float -> term

(** Create term from int. *)
val it : int -> term
(** Create term from bool. *)
val bt : bool -> term

(** Update term methods. *)

(** Update an int term to a new value. *)
val (=:) : term -> int -> unit

(** Update a float term to a new value. *)
val (=:.) : term -> float -> unit

(** Update a bool term to a new value. *)
val (=:|) : term -> bool -> unit

(** Formula creation methods. *)

(** Arithmetic for terms. *)
val add_term_term : term -> term -> formula
val sub_term_term : term -> term -> formula
val mul_term_term : term -> term -> formula
val div_term_term : term -> term -> formula

(** Shorthand arithmetic for terms. *)
val ($+$) : term -> term -> formula
val ($-$) : term -> term -> formula
val ($*$) : term -> term -> formula
val ($/$) : term -> term -> formula

(** Arithmetic for term then formula. *)
val add_term_form : term -> formula -> formula
val sub_term_form : term -> formula -> formula
val mul_term_form : term -> formula -> formula
val div_term_form : term -> formula -> formula

(** Shorthand arithmetic for terms. *)
val ($+&) : term -> formula -> formula
val ($-&) : term -> formula -> formula
val ($*&) : term -> formula -> formula
val ($/&) : term -> formula -> formula

(** Arithmetic for formula then term. *)
val add_form_term : formula -> term -> formula
val sub_form_term : formula -> term -> formula
val mul_form_term : formula -> term -> formula
val div_form_term : formula -> term -> formula

(** Shorthand arithmetic for terms. *)
val (&+$) : formula -> term -> formula
val (&-$) : formula -> term -> formula
val (&*$) : formula -> term -> formula
val (&/$) : formula -> term -> formula

(** Arithmetic for formula then formula. *)
val add_form_form : formula -> formula -> formula
val sub_form_form : formula -> formula -> formula
val mul_form_form : formula -> formula -> formula
val div_form_form : formula -> formula -> formula

(** Shorthand arithmetic for terms. *)
val (&+&) : formula -> formula -> formula
val (&-&) : formula -> formula -> formula
val (&*&) : formula -> formula -> formula
val (&/&) : formula -> formula -> formula

(** Extract raw values from either terms of formula. *)

(** Extraction methods for terms *)
val int_of_term : term -> int
val float_of_term : term -> float
val bool_of_term : term -> bool

(** Extraction methods for formulas *)
val int_of_formula : formula -> int
val float_of_formula : formula -> float
val bool_of_formula : formula -> bool

