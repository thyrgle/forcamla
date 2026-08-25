(** Create mathematical formula, equations, and systems of equations as well as define event listeners that check for various changes in these mathematical objects. *)

(** {1 The Fundamental Types} *)

(** A combination of terms (similar to variables) and operations that represent a mathematical formula *)
type 'a formula

(** {1 Term Creation} *)

(** Terms are of type [formula]. In particular, a {i term} refers to a [formula] with no binary operations.
    There is no term type, but the distinction is made because {i only} terms can be assigned values 
    (otherwise a [NotATermException] error is thrown!).
*)

(** Lift basic types to term types. *)
val t : 'c -> 'c formula

(** {1 Update Term Methods} *)

(** Update a term to a new value. (If the supplied formula is {i not} a term, an error is thrown.) *)
val (=:) : 'd formula -> 'd -> unit

(** {1 Extraction: Get the (current) value for a [formula] or [system]} *)

(** Get current value of a formula. Similar to [(!)] for ref types. *)
val (!) : 'e formula -> 'e

(** {2 Register custom operator} *)

val reg_bin : ('r -> 's -> 't) -> ('r formula -> 's formula -> 't formula)

(** {1 Formula creation methods} *)

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

(** Logical and of two [bool formula] *)
val and_ : bool formula -> bool formula -> bool formula

(** Logical or of two [bool formula] *)
val or_ : bool formula -> bool formula -> bool formula

(** {1 [system] Constructors} *)

(** {2 Fundamental [system] Constructors} *)

(** Fundamental [system] types have only one equation.
    Given two formula [f1] and [f2], fundamental system are of the form [f1 comp f2] where [comp] is a 
    method of {i comparing} the two formula.
    Shorthand versions are mentioned below.
*)

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

(** {2 Shorthand Fundamental [system] Constructors} *)

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

(** {2 Combine [system] types} *)

(* Connect equations via and or or. Shorthand mentioned later. *)

(** {2 Shorthand Combine [system] types} *)

(** Shorthand for anding two equations together. *)
val (&&) : bool formula -> bool formula -> bool formula

(** Shorthand for oring two equations together. *)
val (||) : bool formula -> bool formula -> bool formula

(** {1 [source] Operations} *)

type source

(** Make a source to listen with. *)
val make_source : unit -> source

(** Listen with a specified source *)
val listen : source -> unit

(** Note: 
  Refining event listeners via the [source] type with [exec_while] is mentioned in the next section! 
*)

(** {1 Event listener constructors} *)

(** Listen and execute a function when a [formula] changes value. *)
val on_change : 'o formula -> ('o -> 'o -> unit) -> unit

(** Listen and execute when a [system] becomes true. *)
val when_satisfied : bool formula -> (unit -> unit) -> unit

(** Source event listener. Suppose [s] has registered a system [eq].
    Execute function if [listen s] is called and supply the current value of [eq].
*)
val exec_always : source -> bool formula -> (bool -> unit) -> unit

(** Source event listener. Suppose [s] has registered a system [eq].
    Execute function if the [eq] is currently [true] and [listen s] is called.
*)
val exec_while : source -> bool formula -> (unit -> unit) -> unit
