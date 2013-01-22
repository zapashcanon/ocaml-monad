(** Monads with additional structure.
    @author Phil Scott
*)

(** {6 Base Modules} *)

module type Monoid =
sig
  type t
  val zero : unit -> t
  val plus : t -> t -> t
end

(** Monads with additional monoid structure. *)
module type BasePlus =
sig
  include BatInterfaces.Monad
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
  include BatInterfaces.Monad
  val zero  : unit -> 'a m

  val lplus : 'a m -> 'a m Lazy.t -> 'a m

  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *) 
  val null  : 'a m -> bool
end

(** A writer allows an additional output value to be written during
    computations and accumulated into a final result.*)
module type Writer =
sig
  type t
  include BatInterfaces.Monad

  (** Listen to the written value. *)
  val listen : 'a m -> (t * 'a) m
  
  (** Run the writer, extracting both outputs. *)
  val run   : 'a m -> t * 'a

  (** Write a value. *)
  val write : t -> unit m
end

(** Monads for collections. *)
module type BaseCollectionM =
sig
  include BaseLazyPlus
  val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m
  val unique : ('a -> 'a -> bool) -> 'a m -> 'a m
  val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m
  val nub : ('a -> 'a -> bool) -> 'a m -> 'a m
end

(** {6 Library Types } *)

(** Library functions for monads. *)  
module type Monad =
sig
  include BatInterfaces.Monad 
  include Applicative.Applicative with type 'a m := 'a m

  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  val join     : 'a m m -> 'a m
end

(** Library functions for monads with additional structure.. *)
module type MonadPlus =
sig
  include Monad
  val zero  : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null  : 'a m -> bool
  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val mconcat : 'a m list -> 'a m
end

(** Library functions for lazy monads. *)  
module type LazyPlus =
sig
  include MonadPlus
  val lplus     : 'a m -> 'a m Lazy.t -> 'a m

  val of_llist : 'a LazyList.t -> 'a m
    
  (** Sum the elements inside the monad. *)
  val sum       : 'a LazyList.t m -> 'a m
  val msum      : 'a m LazyList.t -> 'a m    

  val lsequence : 'a m LazyList.t -> 'a LazyList.t m 
    
  (** Sum the list of first elements, the list of second elements, and so
      on. This is matrix transpose for the list monad. *)
  val transpose : 'a LazyList.t m -> 'a m LazyList.t
end

(** Streams supporting unbounded search. Technically, all streams
    should be infinite, but for efficiency, we allow them to be
    truncated. As such, it is impossible to define a general null
    predicate for streams. Here, we have null return true for the
    particular case that the stream is an empty list. *)
module type Stream =
sig
  type 'a t
  include BaseLazyPlus with type 'a m = 'a t Lazy.t

  (** The sum of the stream [\[f x, f (f x), f (f (f x)),...\]] *)
  val iterate : ('a m -> 'a m) -> 'a m -> 'a m

  (** Delay to prevent deadlock. *)
  val delay : 'a m -> 'a m

  (** Search to some depth. *)
  val to_depth : int -> 'a m -> 'a m

end
  
(** Streams whose generations are themselves collections *)
module type StreamC =
sig
  include Stream
  include BaseCollectionM with type 'a m := 'a m

  val lift_cmp : ('a -> 'a -> bool) -> ('a m -> 'a m) -> 'a m -> 'a m
end
  
(** {6 Library Creation} *)

(** Monad library. *)  
module Make(M : BatInterfaces.Monad) : Monad with type 'a m = 'a M.m

(** MonadPlus library. *)
module MakePlus (M : BasePlus) : MonadPlus with type 'a m = 'a M.m

(** LazyPlus library. *)  
module MakeLazyPlus (M : BaseLazyPlus) : LazyPlus with type 'a m = 'a M.m

(** Defined Monads *)
    
module LazyM : BatInterfaces.Monad with type 'a m = 'a Lazy.t

module Lazyt(M : BatInterfaces.Monad) : BatInterfaces.Monad
  with type 'a m = 'a Lazy.t M.m

module List : BasePlus with type 'a m = 'a list

module LazyListM : BaseLazyPlus with type 'a m = 'a LazyList.t

module Option : BasePlus with type 'a m = 'a option

module State(T : sig type s end) :
sig
  include BatInterfaces.Monad 

  val read  : T.s m
  val write : T.s -> unit m
end

(** A writer which combines the output values using some plus
    operation. *)
module MakeWriter(M : Monoid) :
sig
  include Writer with type t = M.t
end

(** {6 Transformers} *)

(** List monad transformer. *)
module Listt(M : BatInterfaces.Monad) : BatInterfaces.Monad with type 'a m = 'a list M.m

(** Option monad transformer *)  
module Optiont(M : BatInterfaces.Monad) : BatInterfaces.Monad
  with type 'a m = 'a option M.m

(** State monad transformer *)
module Statet(T : sig type s end)(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad 

  val read  : T.s m
  val write : T.s -> unit m
end

(** A monad transformer for Writer. *)  
module Writert(W : Writer)(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad
  val listen : 'a m -> (W.t * 'a) m
  val write  : W.t -> unit m
  val pass   : 'a M.m -> 'a m    
  val run    : 'a m -> (W.t * 'a) M.m
end

(** The plus operation of the inner monoid should be [i commutative],
    or alternatively, values should only be extracted by an embedding
    into a commutative monoid such as sort.
*)  
module MakeStream(M : sig
    include BaseLazyPlus
    include Applicative.Base with type 'a m := 'a m
end) :
sig
  include Stream with type 'a t = 'a M.m LazyList.node_t
                 and type 'a m = 'a M.m LazyList.t
  include Applicative.Base with type 'a m := 'a m
end

(** Creation of a stream with underlying type ['a CollectionM.m]. *)
module MakeStreamC(M : sig
    include BaseCollectionM
    include Applicative.Base with type 'a m := 'a m
end) :
sig
  include StreamC with type 'a t = 'a M.m LazyList.node_t
                  and type 'a m = 'a M.m LazyList.t
  include Applicative.Base with type 'a m := 'a m
end

(** Transformer allowing optional values in a collection. *)  
module CollectionOpt(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = 'a option C.m

  (** Lift a comparison function to the writer. *)
  val cmp_on : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
end

(** Transformer for writing data inside a collection. *)  
module CollectionWriter(W : sig
  include Writer
  val cmp : t -> t -> bool
end)(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = 'a W.m C.m
  val write : W.t -> unit m

  (** Promote the inner monad without writing any value. *)
  val pass  : 'a C.m -> 'a m

  (** Recover the inner monad with the final output value. *)
  val run   : 'a m -> (W.t * 'a) C.m

  (** Lift a comparison function to the writer. *)
  val cmp_on : ('a -> 'a -> bool) -> 'a W.m -> 'a W.m -> bool
end

(** State inside a collection. *)  
module CollectionState(T :
  sig
    type s
    val cmp : s -> s -> bool
  end)(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = T.s -> (T.s * 'a) C.m
  val read   : T.s m
  val write  : T.s -> unit m

  (** Lift a comparison function to the writer. *)
  val cmp_on : ('a -> 'a -> bool) -> (T.s * 'a) -> (T.s * 'a) -> bool
end
