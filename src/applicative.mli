(** Applicative functors.

    An applicative functor (or idiom) is more general than a monad, in as much as there are many more applicative functors out there than there are monads. That means they should be easier to find! Applicative functors have a nice library, and you can do some interesting things with them, and you get to assume more constraints on how they behave than you do with monads.

    @author Phil Scott

*)

(** {6 Base Modules} *)

(** Applicative functors *)
module type Base =
sig
  type 'a m

  (** The function [return] is much the same as that for the monad. We just lift a single value. *)
  val return : 'a -> 'a m

  (** The function [ap] sends a computed function to a function of computations. *)
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

  (** We can lift functions of arbitrary arity. *)
  val lift1 : ('a -> 'b) -> 'a m -> 'b m
  val lift2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  (** We can send a list of computations to a computation of a list. *)
  val sequence : 'a m list -> 'a list m

  (** We can lift map. A function which computes ['b] from ['a] can compute a list of ['b] from a list of ['a]. *)
  val map_a : ('a -> 'b m) -> 'a list -> 'b list m
end

module Make(A : Base) : Applicative with type 'a m = 'a A.m

(** {6 Transformer } *)
module Transform(A : Base)(Inner : Base) : Base with type 'a m = 'a Inner.m A.m
