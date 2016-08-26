(** Tagged logic trees which are simplified when combined. When the contents
    are interpreted as a bag conditional values, should be a monad.

    We provide a separate applicative interface which is less strict
    than the derived definition for the monad. When we evaluate
    [lift2 f t1 t2], the function f will be applied across all possible
    arguments from [t1] and [t2], after which, results will be discarded by
    simplification.

    These results do not need to be computed if the tree
    simplification does not depend on the individual computations, as
    is the case with the applicative interface.

    @author Phil Scott
*)

type ('tag,'a) node =
    Node of 'a * ('tag * ('tag,'a) node) LazyList.t

module type Tree =
sig
  type tag
  include Monad.BaseCollectionM
  include Applicative.Base with type 'a m := 'a m
  val count      : 'a m -> int
  val to_list    : 'a m -> 'a LazyList.t
  val collapse   : 'a m -> 'a m
  val with_tags  : 'a m -> (tag list * 'a) m
  val branch     : tag LazyList.t -> tag m
  val print      : ('a -> unit) -> 'a m -> unit
  val to_forced  : 'a m -> 'a m
end

module Make : functor(Tag:sig
  type t
  val print : t -> unit
  val cmp   : t -> t -> bool
end) -> Tree with type tag = Tag.t and type 'a m = (Tag.t, 'a LazyList.t) node
