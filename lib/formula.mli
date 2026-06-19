(** The fundamental types: terms (ref with superpowers!) and formula (combined terms). **)

(** A combination of terms that represent a mathematical formula *)
type 'a formula

(** Lift basic types to term types. *)

(** Create term from float. *)
val ft : float -> float formula

(** Create term from int. *)
val it : int -> int formula

(** Update term methods. *)

(** Update an int term to a new value. *)
val (=:) : int formula -> int -> unit
(** Update a float term to a new value. *)
val (=:.) : float formula -> float -> unit
(** Update a term by evaluating a formula *)
val (=::) : int formula -> int formula -> unit

(** Get current value of formula. Similar to (!) for ref types. *)
val (!) : 'a formula -> 'a

(** Formula creation methods. *)

(** Arithmetic for integer terms. *)
val add_form_int : int formula -> int formula -> int formula
val sub_form_int : int formula -> int formula -> int formula
val mul_form_int : int formula -> int formula -> int formula
val div_form_int : int formula -> int formula -> int formula

(** Shorthand arithmetic for terms. *)
val (+) : int formula -> int formula -> int formula
val (-) : int formula -> int formula -> int formula
val ( * ) : int formula -> int formula -> int formula
val (/) : int formula -> int formula -> int formula

(** Event listener constructors *)
val on_change : 'a formula -> (unit -> unit) -> unit
