type ('b,'a) var =
  | B of 'b
  | F of 'a

val bound : 'b -> ('b,'a) var
val free : 'a -> ('b,'a) var

module type Scope =
  sig
    type ('b,'a) scope
                 
    module Inner : Monad.Monad
    module Monad : functor (B : sig type b end) ->
                   Monad.Monad with type 'a m = (B.b,'a) scope
    val unscope : ('b,'a) scope -> ('b, 'a Inner.m) var Inner.m
    val splat : ('b ->  'c Inner.m) -> ('a -> 'c Inner.m)
                -> ('b,'a) scope -> 'c Inner.m
    val abstractInst :
      ('a -> 'b option) -> ('a -> 'c Inner.m) -> 'a Inner.m -> ('b,'c) scope
    val abstract : ('a -> 'b option) -> 'a Inner.m -> ('b,'a) scope
    val mapBound : ('b -> 'c) -> ('b,'a) scope -> ('c,'a) scope
    val lift : 'a Inner.m -> ('b,'a) scope
    val toScope : ('b,'a) scope -> ('b, 'a) var Inner.m
    val fromScope : ('b, 'a) var Inner.m -> ('b,'a) scope
  end