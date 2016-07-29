(** Scope.

    @author Phil Scott
 *)

(** {6 Variables} *)

(** Variables are either bound or free. In terms of DeBruijn
indices, B is zero and F is successor. *)

type ('b,'a) var =
  | B of 'b
  | F of 'a

(** Functions to wrap the two constructors. *)
val bound : 'b -> ('b,'a) var
val free : 'a -> ('b,'a) var

(** {6 Library }*)
module type Scope =
  sig
    type ('b,'a) scope

    type 'a m
    module Inner : BatInterfaces.Monad with type 'a m = 'a m

    (** The scope monad. *)
    module Monad : functor (B : sig type b end) ->
                   Monad.Monad with type 'a m = (B.b,'a) scope

    (** Conceptually, a scope is a monad applied to a choice between
    bound variables 'b and free variables 'a. *)
    val toScope : ('b,'a) scope -> ('b, 'a) var Inner.m

    (** However, when interpreted as a successor, free can be factored
    over whole subterms, not just variables. *)
    val scope : ('b, 'a Inner.m) var Inner.m -> ('b,'a) scope
    val unscope : ('b,'a) scope -> ('b, 'a Inner.m) var Inner.m

    (** Instantiate bound variables and free variables.*)
    val splat : ('b -> 'c Inner.m) -> ('a -> 'c Inner.m)
                -> ('b,'a) scope -> 'c Inner.m

    (** Abstract some of the free variables and instantiate the others. *)
    val abstractInst :
      ('a -> ('b, 'c Inner.m) var) -> 'a Inner.m -> ('b,'c) scope

    (** Abstract some of the free variables. *)
    val abstract : ('a -> 'b option) -> 'a Inner.m -> ('b,'a) scope

    (** Change bound variables. *)
    val mapBound : ('b -> 'c) -> ('b,'a) scope -> ('c,'a) scope

    val lift : 'a Inner.m -> ('b,'a) scope
  end

module Make(M : Monad.Monad) : Scope with type 'a m = 'a M.m
