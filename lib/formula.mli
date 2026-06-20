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

(** Extraction of the (current) value for a term or formula. *)

(** Get current value of formula. Similar to (!) for ref types. *)
val (!) : 'a formula -> 'a

(** Formula creation methods. *)

(** Arithmetic for integer terms. *)

(** Create a formula that is the sum of two int formula. *)
val add_form_int : int formula -> int formula -> int formula

(** Create a formula that is the difference of two int formula. *)
val sub_form_int : int formula -> int formula -> int formula

(** Create a formula that is the product of two int formula. *)
val mul_form_int : int formula -> int formula -> int formula

(** Create a formula that is the quotient of two int formula *)
val div_form_int : int formula -> int formula -> int formula

(** Shorthand arithmetic for terms. *)

(** Shorthand for addition in an int formula. *)
val (+) : int formula -> int formula -> int formula

(** Shorthand for subtraction in an int formula. *)
val (-) : int formula -> int formula -> int formula

(** Shorthand for multiplication in an int formula. *)
val ( * ) : int formula -> int formula -> int formula

(** Shorthand for division in an int formula. *)
val (/) : int formula -> int formula -> int formula

(** Arithmetic for float terms. *)

(** Create a formula that is the sum of two float formula. *)
val add_form_float : float formula -> float formula -> float formula

(** Create a formula that is the differece of two float formula. *)
val sub_form_float : float formula -> float formula -> float formula

(** Create a formula that is the product of two float formula. *)
val mul_form_float : float formula -> float formula -> float formula

(** Create a formula that is the quotient of two float formula. *)
val div_form_float : float formula -> float formula -> float formula

(** Shorthand floating point arithmetic for terms. *)

(** Shorthand for addition in a float formula. *)
val (+.) : float formula -> float formula -> float formula

(** Shorthand for subtraction in a float formula. *)
val (-.) : float formula -> float formula -> float formula

(** Shorthand for multiplication in a float formula. *)
val ( *. ) : float formula -> float formula -> float formula

(** Shorthand for division in a float formula. *)
val (/.) : float formula -> float formula -> float formula

(** Simple predicate constructors. *)

(** Create a bool formula that determines if two int formula are equal. *)
val eq_form_int : int formula -> int formula -> bool formula

(** Create a bool formula that determines if two float formula are equal. *)
val eq_form_float : float formula -> float formula -> bool formula

(** Shorthand predicate constructors. *)

(** Shorthand for creating a bool formula that determines if two int formulas are equal. *)
val (=?) : int formula -> int formula -> bool formula

(** Shorthand for creating a bool formula that determines if two float formulas are equal. *)
val (=.) : float formula -> float formula -> bool formula

(** Event listener constructors *)

(** Listen and execute a function when a formula changes value. *)
val on_change : 'a formula -> (unit -> unit) -> unit

(** Listen and execute when a predicate (boolean formula) becomes true *)
val when_satisfied : bool formula -> (unit -> unit) -> unit
