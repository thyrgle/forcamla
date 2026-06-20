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

(** Arithmetic for float terms. *)
val add_form_float : float formula -> float formula -> float formula
val sub_form_float : float formula -> float formula -> float formula
val mul_form_float : float formula -> float formula -> float formula
val div_form_float : float formula -> float formula -> float formula

(** Shorthand arithmetic for terms. *)
val (+.) : float formula -> float formula -> float formula
val (-.) : float formula -> float formula -> float formula
val ( *. ) : float formula -> float formula -> float formula
val (/.) : float formula -> float formula -> float formula

(** Simple predicate constructors. *)
val eq_form_int : int formula -> int formula -> bool formula
val eq_form_float : float formula -> float formula -> bool formula

(** Shorthand predicate constructors. *)
val (=?) : int formula -> int formula -> bool formula
val (=.) : float formula -> float formula -> bool formula

(** Event listener constructors *)
val on_change : 'a formula -> (unit -> unit) -> unit
val when_satisfied : bool formula -> (unit -> unit) -> unit
