open LazyList

module type BasePlus =
sig
  include BatMonad.S
  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null : 'a m -> bool
end

module type BaseLazyPlus =
sig
  include BatMonad.S
  val zero : unit -> 'a m
  val lplus : 'a m -> 'a m Lazy.t -> 'a m
  val null : 'a m -> bool
end

module type Monad =
sig
  include BatMonad.S 
  include Applicative.Applicative with type 'a m := 'a m

  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  val join     : 'a m m -> 'a m
end

module type MonadPlus =
sig
  include Monad
  val zero  : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null  : 'a m -> bool
  val filter : ('a -> bool) -> 'a m -> 'a m    
  val of_list   : 'a list -> 'a m
  val mconcat   : 'a m list -> 'a m    
end

module type LazyPlus =
sig
  include MonadPlus
  val lplus     : 'a m -> 'a m Lazy.t -> 'a m
  val of_llist  : 'a LazyList.t -> 'a m
  val sum       : 'a LazyList.t m -> 'a m
  val msum      : 'a m LazyList.t -> 'a m
  val lsequence : 'a m LazyList.t -> 'a LazyList.t m
  val transpose : 'a LazyList.t m -> 'a m LazyList.t
end

module Make(M : BatMonad.S) =
struct
  include M

  let (>>=)  = bind

  let lift1 f x     = x >>= fun x -> return (f x)
  let lift2 f x y   = x >>= fun x -> lift1 (f x) y

  module Ap =
    Applicative.Make(struct
      include M
      let ap f x  = lift2 (fun f x -> f x) f x
    end)
      
  include (Ap : Applicative.Applicative with type 'a m := 'a m)

  let join m = m >>= (fun x -> x)
end

module MakePlus(M : BasePlus) =
struct
  include Make(M)
  let zero ()  = M.zero ()
  let plus     = M.plus
  let null     = M.null
  let filter p xs =
    xs >>= fun x -> if p x then return x else zero ()
  let of_list xs = List.fold_left
    (fun x y -> plus x (return y))
    (zero  ()) xs
  let mconcat xs = List.fold_left plus
    (zero  ()) xs
end

module MakeLazyPlus(M : BaseLazyPlus) =
struct
  include MakePlus(struct
                     include M
                     let plus x y = M.lplus x (lazy y)
                   end)
  let lplus = M.lplus
  let sum xs =
    xs >>=
      (fun xs -> fold_right lplus (zero ())
        (map return xs))

  let of_llist xs =
    fold_right lplus (zero ())
      (map return xs)

  let lsequence ms = fold_left (lift2 (fun xs x -> x ^:^ xs))
    (return nil) ms

  let msum xs = fold_right lplus (zero ()) xs

  let filter p xs =
    xs >>= fun x -> if p x then return x else zero ()
      
  let rec transpose xs =
    lazy (let hds = sum (lift1 (take 1) xs) in
            if null hds then Nil else
              Cons(hds, transpose (lift1 (drop 1) xs)))
end

module MakeStream(M : sig
    include BaseLazyPlus
    include Applicative.Base with type 'a m := 'a m
end) =
struct
  module ML = MakeLazyPlus(M)

  type 'a t = 'a ML.m LazyList.node_t
  type 'a m = 'a t Lazy.t

  let zero ()        = nil
    
  let rec lplus xss yss =
    let hd_yss = lazy (match next (Lazy.force yss) with
	Nil -> ML.zero ()
      | Cons (ys,_) -> ys)
    and tl_yss = lazy (match next (Lazy.force yss) with
	Nil -> nil
      | Cons (_,yss) -> yss)
    in lazy (match next xss with
        Nil -> next (Lazy.force yss)
      | Cons (xs,xss) ->                     
        Cons (ML.lplus xs hd_yss, lplus xss tl_yss))
    
  let plus xss yss = lplus xss (lazy yss)

  let null = is_empty
    
  let return x  = singleton (ML.return x)
    
  let delay xs = M.zero () ^:^ xs

  let bind xs f =
    let join xs =
      let rec shift xs =
    	lazy (match next xs with
    	    Nil -> Nil
          | Cons (x', xs') ->
            next (plus
                    (map ML.join (ML.transpose x'))
                    (delay (shift xs')))) in
      shift xs
    in join (map (ML.lift1 f) xs)

  let ap fs xs =
    let rec shift fs =
      lazy (match next fs with
          Nil -> Nil
        | Cons (f', fs') ->
          next
            (* Without the null check, we'll have a space leak as
               the empty generation is uselessly applied across xs. *)
            (if ML.null f' then delay (shift fs')
             else plus
                (map (M.ap f') xs)
                (delay (shift fs')))) in
    shift fs
      
  let to_depth n = LazyList.take n
           
  let rec iterate f xs =
    plus xs (delay (lazy (next (iterate f (f xs)))))

end

module type BaseCollectionM =
sig
  include BaseLazyPlus
  val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m
  val unique : ('a -> 'a -> bool) -> 'a m -> 'a m
  val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m
  val nub : ('a -> 'a -> bool) -> 'a m -> 'a m    
end

module type Stream =
sig
  type 'a t
  include BaseLazyPlus with type 'a m = 'a t Lazy.t
  val iterate : ('a m -> 'a m) -> 'a m -> 'a m
  val delay : 'a m -> 'a m
  val to_depth : int -> 'a m -> 'a m
end

module type StreamC =
sig
  include Stream
  include BaseCollectionM with type 'a m := 'a m
  val lift_cmp : ('a -> 'a -> bool) -> ('a m -> 'a m) -> 'a m -> 'a m
end

module MakeStreamC(M : sig
    include BaseCollectionM
    include Applicative.Base with type 'a m := 'a m
end) =
struct
  include MakeStream(M)
  module Mm = MakeLazyPlus(M)

  let nub p xs =
    let nub maxs x =
      let x' = M.nub p (M.difference p x maxs) in
        M.lplus (M.difference p maxs x') (lazy x'), x'
    in map_accum_l nub (M.zero ()) xs

  let maxima p xs =
    let maxima maxs x =
      let x' = M.maxima p (M.difference p x maxs) in
        M.lplus (M.difference p maxs x') (lazy x'), x'
    in map_accum_l maxima (M.zero ()) xs
    
  let unique p xs =
    let unique uniques x =
      let x' = M.difference p (M.unique p x) uniques
      in M.lplus (M.difference p uniques x') (lazy x'), x'
    in map_accum_l unique (M.zero ()) xs

  let difference p xss yss =
    let plus xs ys = M.lplus xs (lazy ys) in
      zip_with (M.difference p)
        xss
        (LazyList.drop 1 (scan plus (M.zero ()) (yss ^@^ repeat (M.zero ()))))

  let rec lift_cmp p f xs =
    let rec ys = lazy
      (let fys = delay (f ys) in
       let xs = difference p xs fys in
       let fys = difference p fys xs in
       next (plus xs (delay fys)))
    in ys

end

module LazyM =
struct
  type 'a m = 'a Lazy.t
  let return x = lazy x
  let bind x f =
    lazy (Lazy.force (f (Lazy.force x)))
end

module Lazyt(M : BatMonad.S) =
struct
  module M  = Make(M)
  type 'a m = 'a Lazy.t M.m
  let return x = M.return (lazy x)
  let bind x f =
    M.bind x (fun x ->
      f (Lazy.force x))
end

module List =
struct
  type 'a m = 'a list
  let return x  = [x]
  let bind xs f = List.concat (List.map f xs)
  let zero () = []
  let plus xs ys = xs @ ys
  let null = function
      [] -> true
    | _  -> false
end

module Listt(M : BatMonad.S) =
struct
  module M = Make(M)
  type 'a m = 'a list M.m
  let return x  = M.return [x]
  let bind xs f =
    M.bind xs (fun x ->
      M.lift1 BatList.concat
        (M.sequence (BatList.map f x)))
end

module LazyListM =
struct
  type 'a m = 'a LazyList.t
  let return x  = singleton x
  let bind xs f = concat (map f xs)
  let zero () = nil
  let rec lplus xs ys =
    lazy (match next xs with
        Nil        -> next (Lazy.force ys)
      | Cons(x,xs) -> Cons(x, lplus xs ys))
  let null = is_empty
end

module Option =
struct
  type 'a m = 'a option
  let return x  = Some x
  let bind x f  = match x with
      None   -> None
    | Some y -> f y
  let zero ()   = None
  let plus x y  =
    match x,y with
	None, x        -> x
      | x, None        -> x
      | Some x, Some _ -> Some x
  let null      = BatOption.is_none
end

module Optiont(M : BatMonad.S) =
struct
  type 'a m = 'a option M.m
  let return x  = M.return (Some x)
  let bind xs f =
    M.bind xs
      (function
      None   -> M.return None
        | Some x -> f x)
end

module State(T : sig type s end) =
struct
  type 'a m = T.s -> (T.s * 'a)
  let return x s  = (s, x)
  let bind xf f s =
    let s',x = xf s in
    f x s'
  let read s    = (s,s)
  let write x s = (x,())
end

module Statet(T : sig type s end)(M : BatMonad.S) =
struct
  type 'a m = T.s -> (T.s * 'a) M.m
  let return x s  = M.return (s, x)
  let bind xf f s =
    M.bind (xf s) (fun (s',x) -> (f x) s')
  let read  s   = M.return (s,s)
  let write x s = M.return (x,())
end
  
module type Monoid =
sig
  type t
  val zero : unit -> t
  val plus : t -> t -> t
end

module type Writer =
sig
  type t
  include BatMonad.S
  val listen : 'a m -> (t * 'a) m
  val run   : 'a m -> t * 'a
  val write : t -> unit m
end

module MakeWriter(M : Monoid) =
struct
  include M
  type 'a m = M.t * 'a
  let return x     = M.zero (), x
  let bind (m,x) f =
    let m',y = f x in
    M.plus m m', y
  let listen (x,y) = (x,(x,y))
  let run (x,y)    = (x,y)
  let write x      = (x,())
end
  
module Writert(W : Writer)(M : BatMonad.S) =
struct
  module M = Make(M)
  type t = W.t
  type 'a m = 'a W.m M.m

  module WM = Make(W)

  let return   x = M.return (W.return x)
  let bind x f =
    M.bind x (fun x ->
      let v,x = W.run x in
      M.lift1 (WM.lift2
                 (fun () y -> y)
                 (W.write v))
        (f x))

  let listen x = M.lift1 W.listen x
  let write x = M.return (W.write x)
  let pass xs = M.lift1 (fun x -> W.return x) xs
  let run xs = M.lift1 (fun x -> let (v,x) = W.run x in
                                 v,x) xs
end

module CollectionOpt(C : BaseCollectionM) =
struct
  module Option = Make(Option)
  let liftp2 p x y =
    match Option.lift2 p x y with
        Some b -> b
      | None   -> false

  include Optiont(C)
  let zero () = C.zero ()
  let lplus xs ys = C.lplus xs ys
  let null xs = C.null xs

  let cmp_on p = liftp2 p
  
  let difference p = C.difference (cmp_on p)
  let unique p = C.unique (cmp_on p)
  let nub p = C.nub (cmp_on p)
  let maxima p = C.maxima (cmp_on p)
end

module CollectionWriter(W : sig
  include Writer
  val cmp : t -> t -> bool
end)(C : BaseCollectionM) =
struct
  include Writert(W)(C)
  let zero () = C.zero ()
  let lplus xs ys = C.lplus xs ys
  let null xs = C.null xs

  let cmp_on p x y =
    let (vx,x) = W.run x in
    let (vy,y) = W.run y in
    W.cmp vx vy && p x y

  let difference p = C.difference (cmp_on p)
  let unique p = C.unique (cmp_on p)
  let maxima p = C.maxima (cmp_on p)
  let nub p = C.nub (cmp_on p) 
end

module CollectionState(T :
  sig
    type s
    val cmp : s -> s -> bool
  end)(C : BaseCollectionM) =
struct
  include Statet(T)(C)
  let zero () _ = C.zero ()
  let lplus xs ys s = C.lplus (xs s) (lazy (Lazy.force ys s))
  let null _ = false

  let cmp_on p (s,x) (t,y) = T.cmp s t && p x y
    
  let difference p xs ys s = C.difference (cmp_on p) (xs s) (ys s)
  let unique p xs s = C.unique (cmp_on p) (xs s)
  let maxima p xs s = C.maxima (cmp_on p) (xs s)
  let nub p xs s = C.nub (cmp_on p) (xs s)

  let read s    = C.return (s,s)
  let write x s = C.return (x,())
end
