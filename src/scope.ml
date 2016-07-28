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
    val abstractSubst :
      ('a -> b option) -> ('a -> 'c m) -> 'a m -> 'c scope
    val inst : (b -> 'a m) -> 'a scope -> 'a m
    val abstract : ('a -> b option) -> 'a m -> 'a scope
  end

module Make(M : Monad.Monad)(B : sig type b end) : Scope =
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

    module Monad =
      Monad.Make(struct
                    type 'a m = 'a scope
                    let return x = Scope (M.return (F (M.return x)))
                    let bind scope f =
                      let open M in
                      Scope (unscope scope >>=
                               bifold (fun b -> M.return (B b))
                                      (fun free -> free >>= (unscope % f)))
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
