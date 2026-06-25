(** The fundamental types: [formula]e (combined refs), and [system]s (collections of equations where each equation is two formula joined by a comparison operator.). *)

(** A combination of terms that represent a mathematical formula *)
type 'a formula

(** A collecction of equations that are joined by && or ||. *)
type 'a system

(** Lift basic types to term types. *)
val t : 'a -> 'a formula

(** Update term methods. *)

(** Update an int term to a new value. *)
val (=:) : 'a formula -> 'a -> unit

(** Extraction of the (current) value for a term or formula. *)

(** Get current value of a formula. Similar to (!) for ref types. *)
val (!) : 'a formula -> 'a

(** Get current value of a system. Similar to (!) for ref types. *)
val (!!) : 'a system -> bool

(** Formula creation methods. *)

(** Arithmetic for integer terms. (Shorthand versions are mentioned below.) *)

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

(** Arithmetic for float terms. (Shorthand versions are mentioned below.) *)

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

(** Simple predicate constructors. (Shorthand versions are mentioned below.) *)

(** Create an equation that determines if two int formula are equal. *)
val eq_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are equal. *)
val eq_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are not equal. *)
val ne_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are not equal. *)
val ne_form_float : float formula -> float formula -> float system

(** Shorthand predicate constructors. *)

(** Shorthand for creating a equation that determines if two int formulas are equal. *)
val (=?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas are equal. *)
val (=.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if two int formulas are not equal. *)
val (<>?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas are not equal. *)
val (<>.) : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are equal. *)
val gt_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are equal. *)
val gt_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are not equal. *)
val gte_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are not equal. *)
val gte_form_float : float formula -> float formula -> float system

(** Shorthand predicate constructors. *)

(** Shorthand for creating a equation that determines if for two int formulas LHS > RHS. *)
val (>?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas LHS > RHS. *)
val (>.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if for two int formulas LHS >= RHS. *)
val (>=?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if for two float formulas LHS >= RHS. *)
val (>=.) : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are equal. *)
val lt_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are equal. *)
val lt_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are not equal. *)
val lte_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are not equal. *)
val lte_form_float : float formula -> float formula -> float system

(** Shorthand predicate constructors. *)

(** Shorthand for creating a equation that determines if for two int formulas LHS < RHS. *)
val (<?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas LHS < RHS. *)
val (<.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if for two int formulas LHS <= RHS. *)
val (<=?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if for two float formulas LHS <= RHS. *)
val (<=.) : float formula -> float formula -> float system

(** Combine equations *)

(** And two equations together *)
val and_eqs : 'a system -> 'a system -> 'a system

(** Or two equations together *)
val or_eqs : 'a system -> 'a system -> 'a system

(** Shorthand for anding two equations together. *)
val (&&) : 'a system -> 'a system -> 'a system

(** Shorthand for oring two equations together. *)
val (||) : 'a system -> 'a system -> 'a system

(** Source utilities *)

type 'a source

(** Make a source to listen with. (With int formulas) *)
val make_int_source : unit -> int source

(** Make a source to listen with (With float formulas) *)
val make_float_source : unit -> int source

(** Listen with a specified source *)
val listen : 'a source -> unit

(** Event listener constructors *)

(** Listen and execute a function when a formula changes value. *)
val on_change : 'a formula -> (unit -> unit) -> unit

(** Listen and execute a function when a formula changes value. *)
val system_change : 'a system -> (unit -> unit) -> unit

(** Listen and execute when an equation becomes true *)
val when_satisfied : 'a system -> (unit -> unit) -> unit

(** Source event listener. Execute function if the predicate is true and listen is called. *)
val exec_while : 'a source -> 'a system -> (unit -> unit) -> unit
