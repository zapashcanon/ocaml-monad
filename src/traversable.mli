(** Traversables.

    We implement a traversable as a functor of applicatives.

    @author Phil Scott
 *)

(** {6 Base Modules}*)

(** A partially applied basic traversable. *)
module type TraversableBaseA =
  sig
    type 'a t
    type 'a m

    val traverse : ('a -> 'b m) -> 'a t -> 'b t m end

(** A basic traversable.  *)
module type Base =
  sig
    type 'a t
    module BaseWithA : functor(A : Applicative.Applicative) ->
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

(** A traversable library *)<
module type Traversable =
  sig
    type 'a t
    val lift1 : ('a -> 'b) -> 'a t -> 'b t
    val foldMap : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    module WithA : functor(A : Applicative.Applicative) ->
                   TraversableA with type 'a m = 'a A.m
                                 and type 'a t = 'a t
  end

(** {6 Library Creation} *)
module Make : functor(B : Base) -> Traversable with type 'a t = 'a B.t
