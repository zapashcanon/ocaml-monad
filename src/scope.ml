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
    type 'a scope
    type 'a m
    type b
    val unscope : 'a scope -> (b, 'a m) var m
    val abstractSubst :
      ('a -> b option) -> ('a -> 'c m) -> 'a m -> 'c scope
    val inst : (b -> 'a m) -> 'a scope -> 'a m
    val abstract : ('a -> b option) -> 'a m -> 'a scope
    val lift : 'a m -> 'a scope
    val toScope : 'a scope -> (b, 'a) var m
    val fromScope : (b, 'a) var m -> 'a scope
  end

module Make(M : Monad.Monad)(B : sig type b end) : Scope
       with type 'a m = 'a M.m and type b = B.b =
  struct
    type 'a scope = Scope of (B.b,'a M.m) var M.m
    type 'a m = 'a M.m
    type b = B.b
                   
    let unscope scope = let Scope body = scope in body
    let abstractSubst f g t =
      Scope (M.lift1 (fun x ->
                      match f x with
                      | None -> F (g x)
                      | Some y -> B y) t)
    let inst f scope =
      M.(>>=) (unscope scope) (bifold f identity)
    let abstract f t =
      Scope (M.lift1 (fun x ->
                      match f x with
                      | None -> F (M.return x)
                      | Some y -> B y) t)
    let lift x = Scope (M.return (F x))
    let toScope scope =
      let open M in
      unscope scope >>= function
      | B b -> M.return (B b)
      | F f -> M.lift1 (fun x -> F x) f
    let fromScope x =
      Scope (M.lift1 (function
                       | B b -> B b
                       | F x -> F (M.return x)) x)

    module Monad =
      Monad.Make(struct
                    type 'a m = 'a scope
                    let return x = Scope (M.return (F (M.return x)))
                    let bind scope f =
                      let open M in
                      Scope (unscope scope >>= function
                             | B b -> M.return (B b)
                             | F free -> free >>= (unscope % f))
                  end)

    module Traversable(T : Traversable.Traversable with type 'a t = 'a m) :
    Traversable.Traversable =
      Traversable.Make(
          struct
            type 'a t = 'a scope
            module BaseOfA(A : Applicative.Applicative) =
              struct
                include A
                module TA = T.OfA(A)
                type 'a t = 'a scope
                let traverse f x =
                  TA.traverse
                    (bifold (fun b -> A.return (B b))
                            (A.lift1 (fun x -> F x) % TA.traverse f))
                    (unscope x)
                  |> A.lift1 (fun body -> Scope body)
              end
          end)
  end
