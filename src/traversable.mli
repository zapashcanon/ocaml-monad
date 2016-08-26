(** Traversables.

    We implement a traversable as a functor of applicatives.

    @author Phil Scott
 *)

(** {6 Base Modules}*)

(** A partially applied basic traversable. *)
module type TraversableBaseA =
  sig
    type 'a t
    include Applicative.Base

    val traverse : ('a -> 'b m) -> 'a t -> 'b t m
  end

(** A basic traversable.  *)
module type Base =
  sig
    type 'a t
    module BaseOfA : functor(A : Applicative.Base) ->
                     TraversableBaseA with type 'a m = 'a A.m
                                       and type 'a t = 'a t
  end

(** {6 Library Types } *)

(** A partially applied traversable library. *)
module type TraversableA =
  sig
    include TraversableBaseA
    val sequence : 'a m t -> 'a t m
  end

(** A traversable library *)
module type Traversable =
  sig
    type 'a t
    val lift1 : ('a -> 'b) -> 'a t -> 'b t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val void : 'a t -> 'b t option
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val all : bool t -> bool
    val any : bool t -> bool
    val forall : ('a -> bool) -> 'a t -> bool
    val forany : ('a -> bool) -> 'a t -> bool
    val sum_ints : int t -> int
    val product_ints : int t -> int
    val sum_floats : float t -> float
    val product_floats : float t -> float
    module OfA : functor(A : Applicative.Base) ->
                   TraversableA with type 'a m = 'a A.m
                                 and type 'a t = 'a t
  end

(** {6 Library Creation} *)
module Make : functor(B : Base) -> Traversable with type 'a t = 'a B.t

module List : Traversable with type 'a t = 'a list                                                                   
