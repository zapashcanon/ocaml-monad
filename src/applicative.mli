(** Applicative functors.
    @author Phil Scott
*)

(** {6 Base Modules} *)

(** Applicative functors *)  
module type Base =
sig
  type 'a m
  val return : 'a -> 'a m
  val ap : ('a -> 'b) m -> 'a m -> 'b m
end
  
(** Applicatives with additional monoid structure. *)
module type BasePlus =
sig
  include Base
  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m    

  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *) 
  val null : 'a m -> bool
end

(** LazyPlus provides a plus operation which is non-strict in its second
    argument. *)
module type BaseLazyPlus =
sig
  include Base
  val zero  : unit -> 'a m

  val lplus : 'a m -> 'a m Lazy.t -> 'a m

  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *) 
  val null  : 'a m -> bool
end

(** {6 Library Types } *)

(** Library functions for applicatives *)
module type Applicative =
sig
  include Base

  val lift1 : ('a -> 'b) -> 'a m -> 'b m
  val lift2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val sequence : 'a m list -> 'a list m
end

module Make(A : Base) : Applicative with type 'a m = 'a A.m

(** {6 Transformer } *)
module Transform(A : Base)(Inner : Base) : Base with type 'a m = 'a Inner.m A.m
