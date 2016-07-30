open BatPervasives

module type TraversableBaseA =
  sig
    type 'a t
    include Applicative.Base
    val traverse : ('a -> 'b m) -> 'a t -> 'b t m
  end

module type Base =
  sig
    type 'a t
    module BaseOfA : functor(A : Applicative.Applicative) ->
                     TraversableBaseA with type 'a m = 'a A.m
                                         and type 'a t = 'a t
  end

module type TraversableA =
  sig
    include TraversableBaseA
    val sequence : 'a m t -> 'a t m
  end

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
    module OfA : functor(A : Applicative.Applicative) ->
                 TraversableA with type 'a m = 'a A.m
                               and type 'a t = 'a t
  end

module Make(B : Base) : Traversable with type 'a t = 'a B.t =
  struct
    include B
    module TId = B.BaseOfA(Monad.Id)
    let lift1 = TId.traverse
    let (<$>) = TId.traverse
    let foldMap (type b) f p b =
      let module A = Applicative.Const(struct
                                          type t = b
                                          let zero () = b
                                          let plus = p
                                        end) in
      let module TA : TraversableBaseA
                 with type 'a m = b
                  and type 'a t = 'a B.t = B.BaseOfA(A) in
      TA.traverse f
    let foldr (type b) f b t =
      let module A = Applicative.Const(struct
                                          type t = b -> b
                                          let zero () = identity
                                          let plus = (%)
                                        end) in
      let module TA : TraversableBaseA
                 with type 'a m = b -> b
                  and type 'a t = 'a B.t = B.BaseOfA(A) in
      TA.traverse f t b
    module TOpt = B.BaseOfA(Monad.Option)
    let void t = TOpt.traverse (const (Monad.Option.zero ())) t
    let is_empty t = BatOption.is_some (void t)
    let length t = foldr (fun _ n -> n + 1) 0 t
    let all = foldr (&&) true
    let any = foldr (||) true
    let forall p = foldr ((&&) % p) true
    let forany p = foldr ((||) % p) false
    let sum_ints = foldr (+) 0
    let product_ints = foldr (fun x y -> x * y) 1
    let sum_floats = foldr (+.) 0.
    let product_floats = foldr (fun x y -> x *. y) 1.
    module OfA(A : Applicative.Applicative) =
      struct
        include B.BaseOfA(A)
        let sequence t = traverse identity t
      end
  end

module List =
  Make(struct
          type 'a t = 'a list
          module BaseOfA(A : Applicative.Applicative) =
            struct
              include A
              type 'a t = 'a list
              let rec traverse f = function
                | [] -> A.return []
                | x::xs -> A.lift2 (fun y ys -> y :: ys) (f x) (traverse f xs)
            end
        end)

module Option =
  Make (struct
           type 'a t = 'a option
           module BaseOfA(A : Applicative.Applicative) =
            struct
              include A
              type 'a t = 'a option
              let rec traverse f = function
                | None -> A.return None
                | Some x -> (fun x -> Some x) <$> f x
            end
        end)
