module Ll = LazyList
let (^:^) = Ll.(^:^)

module type Base =
sig
  type 'a m
  val return : 'a -> 'a m
  val (<*>) : ('a -> 'b) m -> 'a m -> 'b m
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

module type Applicative =
sig
  include Base

  val lift1 : ('a -> 'b) -> 'a m -> 'b m
  val lift2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val sequence : 'a m list -> 'a list m
  val map_a : ('a -> 'b m) -> 'a list -> 'b list m

  val (<*) : 'a m -> 'a m -> 'a m
  val (>*) : 'a m -> 'a m -> 'a m
end

module Make(A : Base) =
struct
  include A

  let lift1 f x     = return f    <*> x
  let lift2 f x y   = lift1 f x   <*> y
  let lift3 f x y z = lift2 f x y <*> z

  let (<*) x y = lift2 (fun x _ -> x) x y
  let (>*) x y = lift2 (fun _ y -> y) x y

  let sequence ms = List.fold_left (lift2 (fun xs x -> x :: xs))
    (return []) ms

  let map_a f xs = sequence (List.map f xs)
end

module Transform(A : Base)(Inner : Base) =
struct
  module A = Make(A)
  type 'a m = 'a Inner.m A.m
  let return x = A.return (Inner.return x)
  let (<*>) f x = A.lift2 Inner.(<*>) f x
end
