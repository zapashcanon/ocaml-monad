open BatPervasives

type ('b,'a) var =
  | B of 'b
  | F of 'a

let bound x = B x
let free x = F x

let bifold f g = function
  | B x -> f x
  | F x -> g x

module type Scope =
  sig
    type ('b,'a) scope
    type 'a m
                 
    module Inner : BatInterfaces.Monad with type 'a m = 'a m
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

module Make(M : Monad.Monad) =
  struct
    type 'a m = 'a M.m
    module Inner = M
    type ('b,'a) scope = Scope of ('b,'a Inner.m) var Inner.m

    let unscope scope = let Scope body = scope in body
                                          
    module Monad(B : sig type b end) =
      Monad.Make(struct
                    type 'a m = (B.b,'a) scope
                    let return x = Scope (Inner.return (F (Inner.return x)))
                    let bind scope f =
                      let open M in
                      Scope (unscope scope >>= function
                             | B b -> Inner.return (B b)
                             | F free -> free >>= (unscope % f))
                  end)

    let abstractInst f g t =
      Scope (Inner.lift1 (fun x ->
                      match f x with
                      | None -> F (g x)
                      | Some y -> B y) t)
    let abstract f = abstractInst f Inner.return
    let splat f g scope =
      let open M in
      unscope scope >>= function
      | B b -> f b
      | F f -> f >>= g
    let mapBound f scope = Scope (Inner.lift1 (function
                                            | B b -> B (f b)
                                            | F x -> F x)
                                          (unscope scope))
    let lift x = Scope (Inner.return (F x))
    let toScope scope =
      let open M in
      unscope scope >>= function
      | B b -> Inner.return (B b)
      | F f -> Inner.lift1 (fun x -> F x) f
    let fromScope x =
      Scope (Inner.lift1 (function
                       | B b -> B b
                       | F x -> F (Inner.return x)) x)

    module Traversable(T : Traversable.Traversable with type 'a t = 'a Inner.m)
                      (B : sig type b end) : Traversable.Traversable =
      Traversable.Make(
          struct
            type 'a t = (B.b, 'a) scope
            module BaseOfA(A : Applicative.Applicative) =
              struct
                type 'a t = (B.b, 'a) scope
                include A
                module TA = T.OfA(A)
                let traverse f x =
                  TA.traverse
                    (bifold (fun b -> A.return (B b))
                            (A.lift1 (fun x -> F x) % TA.traverse f))
                    (unscope x)
                  |> A.lift1 (fun body -> Scope body)
              end
          end)
  end
