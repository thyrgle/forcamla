(** Create mathematical formula, equations, and systems of equations as well as define event listeners that check for various changes in these mathematical objects. *)

(** {1 The Fundamental Type: [formula]} *)

(** A combination of terms (that is, either a variable or constant) and operations that represent a 
    mathematical formula *)
type 'a formula

(** {1 [Val] Creation} *)

(** [Val]s are of type [formula]. In particular, a {i val} refers to a [formula] with no operations. That is,
    a Val is simply a wrapped OCaml value. Note that only [Val] typed objects can have their value changed
    by the user (see [(=:)]), otherwise a [NotATermException] error is thrown!
*)

(** Lift basic types to term types. *)
val t : 'c -> 'c formula

(** {1 Update Term Methods} *)

(** Update a term to a new value. (If the supplied formula is {i not} a term, an error is thrown.) *)
val (=:) : 'd formula -> 'd -> unit

(** {1 Extraction: Get the (current) value for a [formula]} *)

(** Get current value of a formula. An analog to [(!)] for ref types. *)
val (!) : 'e formula -> 'e

(** {2 Register custom operator} *)

(** Register a (generic) unary operation. *)
val reg_unary : ('r -> 's) -> ('r formula -> 's formula)

(** Register a unary operation that returns a bool. Allows for [when_satisfied] field. *)
val reg_unary_bool : ('r -> bool) -> ('r formula -> bool formula)

(** Register a (generic) binary operation. *)
val reg_bin : ('r -> 's -> 't) -> ('r formula -> 's formula -> 't formula)

(** Register a binary operation that returns a bool. Allows for [when_satisfied] field. *)
val reg_bin_bool : ('r -> 's -> bool) -> ('r formula -> 's formula -> bool formula)

(** {1 Built-in [formula] creation methods} *)

(** {2 Arithmetic for [int formula]} *)

(** Shorthand versions are presented in the next section! *)

(** Create a formula that is the sum of two int formula. *)
val add_form_int : int formula -> int formula -> int formula

(** Create a formula that is the difference of two int formula. *)
val sub_form_int : int formula -> int formula -> int formula

(** Create a formula that is the product of two int formula. *)
val mul_form_int : int formula -> int formula -> int formula

(** Create a formula that is the quotient of two int formula *)
val div_form_int : int formula -> int formula -> int formula

(** {2 Shorthand arithmetic for [int formula]} *)

(** Shorthand for addition in an int formula. *)
val (+) : int formula -> int formula -> int formula

(** Shorthand for subtraction in an int formula. *)
val (-) : int formula -> int formula -> int formula

(** Shorthand for multiplication in an int formula. *)
val ( * ) : int formula -> int formula -> int formula

(** Shorthand for division in an int formula. *)
val (/) : int formula -> int formula -> int formula

(** {2 Arithmetic for [float formula]} *)

(** Shorthand versions are presented in the next section! *)

(** Create a formula that is the sum of two float formula. *)
val add_form_float : float formula -> float formula -> float formula

(** Create a formula that is the differece of two float formula. *)
val sub_form_float : float formula -> float formula -> float formula

(** Create a formula that is the product of two float formula. *)
val mul_form_float : float formula -> float formula -> float formula

(** Create a formula that is the quotient of two float formula. *)
val div_form_float : float formula -> float formula -> float formula

(** {2 Shorthand [float formula] arithmetic} *)

(** Shorthand for addition in a float formula. *)
val (+.) : float formula -> float formula -> float formula

(** Shorthand for subtraction in a float formula. *)
val (-.) : float formula -> float formula -> float formula

(** Shorthand for multiplication in a float formula. *)
val ( *. ) : float formula -> float formula -> float formula

(** Shorthand for division in a float formula. *)
val (/.) : float formula -> float formula -> float formula

(** {2 Concat [string formula]} *)

(** Create a formula that is the concatenation of two string formula. *)
val concat_strings : string formula -> string formula -> string formula

(** Shorthand for concatenation of two formula. *)
val (^) : string formula -> string formula -> string formula

(** {2 [bool formula] operations} (Shorthand below!) *)

(** {3 Conjugation (and) and disjunction (or)} *)

(** Logical and of two [bool formula] *)
val and_ : bool formula -> bool formula -> bool formula

(** Logical or of two [bool formula] *)
val or_ : bool formula -> bool formula -> bool formula

(** Shorthand for logical and of two [bool formula] *)
val (&&) : bool formula -> bool formula -> bool formula

(** Shorthand for logical or of two [bool formula] *)
val (||) : bool formula -> bool formula -> bool formula

(** {3 Comparison methods for formulas *)

(** Shorthand versions below! *)

(** Create an equation that determines if two int formula are equal. *)
val eq_form : 'e formula -> 'e formula -> bool formula

(** Create an equation that determines if two int formula are not equal. *)
val neq_form : 'f formula -> 'f formula -> bool formula

(** Create an equation that determines if two int formula are equal. *)
val gt_form : 'g formula -> 'g formula -> bool formula

(** Create an equation that determines if two int formula are not equal. *)
val gte_form : 'h formula -> 'h formula -> bool formula

(** Create an equation that determines if two int formula are equal. *)
val lt_form : 'i formula -> 'i formula -> bool formula

(** Create an equation that determines if two int formula are not equal. *)
val lte_form : 'j formula -> 'j formula -> bool formula

(** {3 Shorthand comparison operators} *)

(** Shorthand for creating a equation that determines if two int formulas are equal. *)
val (=) : 'e formula -> 'e formula -> bool formula

(** Shorthand for creating a equation that determines if two int formulas are not equal. *)
val (<>) : 'f formula -> 'f formula -> bool formula

(** Shorthand for creating a equation that determines if for two int formulas LHS > RHS. *)
val (>) : 'g formula -> 'g formula -> bool formula

(** Shorthand for creating a equation that determines if for two int formulas LHS >= RHS. *)
val (>=) : 'h formula -> 'h formula -> bool formula

(** Shorthand for creating a equation that determines if for two int formulas LHS < RHS. *)
val (<) : 'i formula -> 'i formula -> bool formula

(** Shorthand for creating a equation that determines if for two int formulas LHS <= RHS. *)
val (<=) : 'j formula -> 'j formula -> bool formula

(** {1 [source] Construction and Listening} *)

type source

(** Make a source to listen with. *)
val make_source : unit -> source

(** Listen with a specified source *)
val listen : source -> unit

(** Note: 
  Refining event listeners via the [source] type with [exec_while] is mentioned in the next section! 
*)

(** {2 Event listener creation} *)

(** Listen and execute a function when a [formula] changes value. *)
val on_change : 'o formula -> ('o -> 'o -> unit) -> unit

(** Listen and execute when a [bool formula] becomes true. *)
val when_satisfied : bool formula -> (unit -> unit) -> unit

(** Source event listener. Suppose [s] has registered a [eq] with type ['a formula].
    Execute function if [listen s] is called and supply the current value of [eq].
    Note: This will execute *regardless* of the value of [eq].
*)
val exec_always : source -> bool formula -> (bool -> unit) -> unit

(** Source event listener. Suppose [s] has registered a [eq] with type [bool formula].
    Execute function if the [eq] is currently [true] and [listen s] is called.
    Note: Unlike [exec_always] this will *only* execute when [eq] is [true].
*)
val exec_while : source -> bool formula -> (unit -> unit) -> unit
