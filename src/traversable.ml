open BatPervasives

module type TraversableBaseA =
  sig
    type 'a t
    type 'a m
    val traverse : ('a -> 'b m) -> 'a t -> 'b t m
  end

module type Base =
  sig
    type 'a t
    module BaseWithA : functor(A : Applicative.Applicative) ->
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
    val foldMap : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    module WithA : functor(A : Applicative.Applicative) ->
		   TraversableA with type 'a m = 'a A.m
				 and type 'a t = 'a t
  end

module Make(B : Base) : Traversable with type 'a t = 'a B.t =
  struct
    include B
    module TId = B.BaseWithA(Monad.Id)
    let lift1 = TId.traverse
    let foldMap (type b) f p b t =
      let module A = Applicative.Const(struct
					  type t = b
					  let zero () = b
					  let plus = p
					end) in
      let module TA : TraversableBaseA
		 with type 'a m = b
		  and type 'a t = 'a B.t = B.BaseWithA(A) in
      TA.traverse f t
    let foldr f b t = foldMap f (%) identity t b
    module WithA(A : Applicative.Applicative) =
      struct
	include B.BaseWithA(A)
	let sequence t = traverse identity t
      end
  end
