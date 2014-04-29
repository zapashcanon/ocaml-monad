(** The monad library.

    {b Introduction}

    Monads are often defined narrowly in terms of a bind and return function. This
    misses the {i abstraction}, which is to be found in the libraries which give us
    general functions that apply to {i all} monads. For example, if we aren't
    automatically deriving a {! Monad.join} function for all of our monads, then
    we've missed something important.

    {b Terminology }

    There's some confusing terminology when it comes to monads, which I'll try
    to clarify for the purposes of the documentation.

    Formally, a monad is a three-tuple: a type-constructor, and two functions. So,
    for instance, the list monad can be defined by three things:

    - the constructor [list] which appears in type expressions such as ['a list]; the
    - function [return]; the function [bind].

    The fact that monads abstract over a type-constructor explains why a monad cannot
    be represented nicely by a simple tuple. We need to package them into a module
    instead.

    Terminology now gets a bit hairy, because as pointed out by
    {{:http://blog.plover.com/prog/haskell/monad-terminology.html} Mark
    Dominus}, we need to talk about many different things in the context of
    monads, which leads to overloading and an (overused) use of the adjective
    "monadic."

    We have our three tuple above, we have the type-constructor [list], we have
    types such as [int list], and we have values such as [\[1;2;3\] : int
    list]. In general, I shall reserve "monad" for the type-constructor
    [list], and I'll read values such as [\[1;2;3\]] as "the computation in the
    [list] monad which returns 1, 2 or 3." I will also read values of type [m
    ()] as "the computation in the [m] monad which returns nothing."

    I'm not convinced that "computation" is the right word for what monads are about,
    and I worry that if this metaphor gets taken too seriously, we'll miss some
    interesting monads. See
    {{:http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html}
    Why Free Monads matter}, for an example. For the purposes of this documentation,
    the metaphor is just a convenience.

    @author Phil Scott
*)

(** {6 Base Modules}*)

(** This is your usual monoid. The [zero] should act as the identity for
    [plus], and [plus] should be associative. *)
module type Monoid =
sig
  type t
  val zero : unit -> t
  val plus : t -> t -> t
end

(** Monads with additional monoid structure.

    This is a useful base module, which provides a whole slew of library
    functions. I recommend interpreting the zero function as indicating [i
    failure], while plus indicates a [i choice].

    For a concrete example, consider the [list] monad. Each list represents a
    set of simultaneous values, and [bind] runs computations
    non-deterministically. The zero is just the empty list, which tells us that
    on failure, no values get generated. The plus operation is [@], which
    allows us to choose from elements from either of two computations. *)
module type BasePlus =
sig
  include BatInterfaces.Monad
  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m

  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. I have
      provided this so that streams can be implemented more efficiently. *)
  val null : 'a m -> bool
end

(** LazyPlus is another base module useful when the monad is a lazy data
    structure. We then allow the plus operation to be non-strict in its second
    argument, which makes it possible to use functions such as {!
    Applicative.lsequence} lazily. This is what you want for lazy lists. *)
module type BaseLazyPlus =
sig
  include BatInterfaces.Monad
  val zero  : unit -> 'a m

  val lplus : 'a m -> 'a m Lazy.t -> 'a m

  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *)
  val null  : 'a m -> bool
end

(** {6 Some general monads.} *)

(** Readers are monads which allow computations to depend on a fixed
    environment. You can use this environment to look up data that
    your computation depends on, such as keyed values pulled from a
    configuration file.

    The environment can be any type. You might consider using a single value,
    or perhaps something more complex like a hash-table of assignments.

    This is effectively a read-only version of {! Monad.State}. If you only
    need read access to data, why not enforce the matter at the type-level by
    choosing a reader? *)
module type Reader =
sig
  type t
  include BatInterfaces.Monad

  (** A computation which reads the environment. *)
  val read : t m

  (** Run a reader computation with a given environment, extracting the final
  value. *)
  val run  : t -> 'a m -> 'a
end

(** A writer allows an additional output value to be written during
    computations and accumulated into a final result. You could might consider
    writing strings, or perhaps a hash-table with merges on each write. It can
    be useful for logging, say.

    This is effectively a write-only version of {! Monad.State}. If you only
    need write access to data, why not enforce the matter at the type-level by
    choosing a writer? *)
module type Writer =
sig
  type t
  include BatInterfaces.Monad

  (** Transform a writer computation into one which returns the same value with
  the final written value. *)
  val listen : 'a m -> (t * 'a) m

  (** Run the writer, extracting the final written value and return value. *)
  val run   : 'a m -> t * 'a

  (** A computation which writes a value and returns nothing. *)
  val write : t -> unit m
end

(** State monads. This is a way to fake state in pure code by threading a state
    variable through your function calls. The monad takes care to hide the
    wrapping from you. *)
module State(T : sig type s end) :
sig
  include BatInterfaces.Monad

  (** A computation which returns the current state. *)
  val read   : T.s m

  (** A computation which writes a new state and returns nothing. *)
  val write  : T.s -> unit m

  (** A computation which modifies the state using a mutator function and
  returns nothing. *)
  val modify : (T.s -> T.s) -> unit m

  (** Run the computation with a given initial value. *)
  val run    : 'a m -> T.s -> (T.s * 'a)
end

(** Error monads. This is a way to fake exceptions in pure code. Here, our
    computations might fail and throw some exceptional value, which can be
    caught in a pattern-match. Ocaml exceptions are obviously more powerful
    than this, but as with {! Monad.Option}, the advantage here is that our
    errors are now first-class values!

    Like {! Monad.Option}, the plus operation discards the right-hand
    computation if neither throws an error.
 *)
module Error(E : sig type e val defaultError : e end) :
sig
  include BasePlus

  (** A computation which just throws an error. *)
  val throw : E.e -> 'a m

  (** Add exception handling to a computation. *)
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
end

(** Monads for collections. Stream is the current use-case for this, since it
    is parameterised on a collection monad (like [list]). *)
module type BaseCollectionM =
sig
  include BaseLazyPlus
  (** [difference p xs ys] removes all elements from [xs] which are less than or
    equal to some element in [ys], according to the partial order [p]. *)
  val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m

  (** [unique eq xs] removes duplicates according to the function [cmp]. *)
  val unique : ?cmp:('a -> 'a -> bool) -> 'a m -> 'a m

  (** [maxima p xs] leaves only the maximal elements according to the
  partial order [p]. *)
  val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m

  (** [nub p xs ys] is the same as maxima, but some values might be treated as
  greater than others depending on their position in the collection.

  For instance, with lists, we treat elements that occur earlier in the list
  as always greater than elements that occur later, otherwise we use [p]. This
  is really just an optimisation: in order to retrieve any value from [maxima p
  xs], the [maxima] function will have had to seen every value. Not so with
  [nub].*)
  val nub : ('a -> 'a -> bool) -> 'a m -> 'a m
end

(** Streams supporting fair and unbounded search.

    Spivey has a nice and authoratitive paper on this, understanding streams as
    a breadth-first search. If that description appeals to you, I recommend his
    paper. Personally, I found it helpful to understand streams a bit
    differently, as concurrent processes that go off and discover data.

    First, we have {i generations}, which are collections of zero or more
    data. The data in each generation is discovered simultaneously, and
    therefore, it doesn't make much sense to have any ordering imposed on the
    generations. In fact, the lack of an ordering is essential for getting the
    associativity laws of the monad, and is why Spivey's paper works with bags.

    Now a stream is just a lazy list of generations. We want to think of the
    indices into these streams as temporal indices. So the first generation in
    the lazy list was discovered at time 0. The second was discovered at time
    1. The third was discovered at time 2. And so on.

    Thinking this way helped me figure out [return] and [join]. Here,
    [return x] gives you the process which immediately discovers [x] and then
    terminates.

    With [join xss], we should interpret the [xss] as a process which discovers
    {i other processes}. When we join them, we ask the outer process to fork
    each inner process as soon as it is discovered, and then merge in all the
    values found by the forked processes. For instance, if [xs] discovers a
    process [p] at time 5, and in turn, [p] discovers the string "hello world!"
    at time 13, then [join xs] discovers "hello world!" at time 5 + 13 = 18.

    Now we can understand [bind], realising that it is just the result of doing
    a [map] and then a [join]. The expression [bind xs f] forks processes [f]
    which depend on a discovered value [x], and then merges back all their values.

    Technically, all our streams should be infinite, and the processes should
    run forever, but this gave me sad performance in my theorem proving
    code. So for efficiency, our streams can be truncated, which represents a
    process that terminates.

    Note that it is impossible to define a general {! BaseLazyPlus.null}
    predicate for streams, because we would have to be able to decide whether
    an infinite stream of values consists entirely of the empty generation. Turing
    says that's impossible, and I believe him. As a crude approximation, then,
    we have it that {! BaseLazyPlus.null} returns [true] just for the special case that
    its input is an empty lazy list. *)
module type Stream =
sig
  type 'a t
  include BaseLazyPlus with type 'a m = 'a t Lazy.t

  (** The sum of the stream [\[f x, f (f x), f (f (f x)),...\]] *)
  val iterate : ('a m -> 'a m) -> 'a m -> 'a m

  (** Delay a stream by one time step. This is needed when you write recursive
  streams and you have to avoid deadlock. The nice thing about Ocaml here is that
  it will generally detect deadlock for you, announcing to you that you're
  writing viciously circular lists! *)
  val delay : 'a m -> 'a m

  (** Terminate discovery at some depth. *)
  val to_depth : int -> 'a m -> 'a m

end

(** The union of streams and collections. *)
module type StreamC =
sig
  include Stream
  include BaseCollectionM with type 'a m := 'a m
end

(** {6 Library Types } *)

(** Your basic library functions for monads. *)
module type Monad =
sig
  include BatInterfaces.Monad
  include Applicative.Applicative with type 'a m := 'a m

  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m

  (** Join unwraps one layer of computation: if you can compute computations,
  then you have those computations.

      For a concrete example, think of lists. Here, values of type ['a list
      list] are computations of non-deterministic computations. Each inner
      computation returns possible values. If we [join] the outer computation,
      then we get all possible values from all inner computations.*)
  val join    : 'a m m -> 'a m

  (** If we can compute satisfiability of a predicate on ['a], then we can
  compute a filtering of ['a list]. *)
  val filter_m : ('a -> bool m) -> 'a list -> 'a list m
end

(** Library functions for monads with additional monoid structure. *)
module type MonadPlus =
sig
  include BasePlus
  include Monad with type 'a m := 'a m

  (** Not to be confused with filter_m. Here, we have an ordinary predicate on
  ['a] with which we can filter out values from a computation. *)
  val filter : ('a -> bool) -> 'a m -> 'a m

  (** Turn a list of values into a computation of those possible values. *)
  val of_list : 'a list -> 'a m

  (** Turn a computation of a list into a computation of its possible values. *)
  val sum  : 'a list m -> 'a m

  (** fold the [plus] across a list. *)
  val msum : 'a m list -> 'a m

  (** Run a computation if a given condition holds, otherwise fail. *)
  val guard : bool -> 'a m -> 'a m

  (** Generalises matrix transposition: given a computation of lists, return
  a list containing, in order: all possible first elements; all possible
  second elements; all possible third elements, and so on. This will loop
  infinitely if {! BasePlus.null} cannot answer [true] for [zero]es. *)
  val transpose : 'a list m -> 'a m list

end

(** This is the counterpart for the lazy version of {! BasePlus}. *)
module type LazyPlus =
sig
  include MonadPlus
  val lplus     : 'a m -> 'a m Lazy.t -> 'a m

  val of_llist  : 'a LazyList.t -> 'a m

  (** Turn a computation of a lazy list into a computation of its possible
  values. *)
  val lsum      : 'a LazyList.t m -> 'a m

  (** fold the [plus] across a lazy list. *)
  val lmsum     : 'a m LazyList.t -> 'a m

  (** Generalises matrix transposition: given a computation of lists, return
  a list containing, in order: all possible first elements; all possible
  second elements; all possible third elements, and so on. You don't have to
  worry so much about correctly implementing {! BaseLazyPlus.null} for this function,
  since the return value can happily be infinite. *)
  val ltranspose : 'a LazyList.t m -> 'a m LazyList.t
end

(** {6 Library Creation} *)
(** Use these functors with the base modules to generate the monad library functions. *)

(** Monad library. *)
module Make(M : BatInterfaces.Monad) : Monad with type 'a m = 'a M.m

(** MonadPlus library. *)
module MakePlus (M : BasePlus) : MonadPlus with type 'a m = 'a M.m

(** LazyPlus library. *)
module MakeLazyPlus (M : BaseLazyPlus) : LazyPlus with type 'a m = 'a M.m

(** {6 Specific monads} *)
(** The lazy monad. You might use this so that you don't have to keep delaying and
    forcing lazy values in computations. *)
module LazyM : BatInterfaces.Monad with type 'a m = 'a Lazy.t

(** The venerable list monad. Lists can be understood as computations which
    return zero or more possible values, and where further computation on
    these values is non-deterministic. *)
module List : BasePlus with type 'a m = 'a list

(** The lazy list monad. See {! List}  *)
module LazyListM : BaseLazyPlus with type 'a m = 'a LazyList.t

(** Options can be understood as computations which might optionally fail. You
    might ordinarily use exceptions for this, but options have the nice
    advantage that they are data! You can package them up into lists, and use
    generic library functions such as {! Applicative.sequence} to compute
    either a list of successful return values, or a failure if any of the individual
    computations fails.

    There is already precedent for moving away from exceptions and towards
    options. See the Exceptionless modules in batteries, and consider that
    it is becoming popular to wrap nullable values in F# and Scala with options
    and thus warn against the dreaded null-pointer exception.

    The plus operation of option will discard the right-hand computation if
    both return values.
*)
module Option : BasePlus with type 'a m = 'a option

(** The continuation monad. I'm told that it is a good idea to be afraid of
    this one, and generally avoid it! But for the persistent:

    A continuation of type 'a has type
    [('a -> 'r) -> 'r]
    The first argument to this continuation is a final value of type 'r which
    depends on some intermediate value of type 'a. In other words, we can
    understand the continuation as a result of type 'r which depends on the {i
    future} of an intermediate value.

    The function [return] just {i throws} its return value to the future in
    order to produce the final result. The [bind] intercedes in the future,
    inserting a computation.

    Things get bizarre with the function call-with-current-continuation. This
    allows one to effectively reflect into one's own future. That is, callCC is
    a computation which depends on another computation taking the future as a
    first-class value. One can store this future, and at any time, throw it a
    return value to reinstate it.

    If you are into the Curry-Howard Isomorphism,
    call-with-current-continuation has a type which corresponds to a law of
    classical logic (Pierce's Law). Writing your code in the continuation
    monad corresponds to embedding classical logic intuitionistically. Allowing
    callCC corresponds to assuming a classical hypothesis. 
*)
module Continuation(T : sig type r end) :
sig
  include BatInterfaces.Monad with type 'a m = ('a -> T.r) -> T.r

  val callCC : (('a -> 'r m) -> 'a m) -> 'a m
end

(** Create a reader from an arbitrary type of environment. *)
module MakeReader(T : sig type t end) :
sig
  include Reader with type t = T.t
end

(** Create a writer from an arbitrary type of written value. *)
module MakeWriter(M : Monoid) :
sig
  include Writer with type t = M.t
end

(** Create a stream monad from an arbitrary inner monad, which computes the
    values discovered in each generation. This is pretty abstract, since we're
    not requiring that the inner monad is a {i collection}. There is a
    constraint here, since we're strictly supposed to disallow a monad
    collection where order of elements matters. I think we can characterise
    this abstractly by saying that the plus operation on the inner monad must
    be commutative, but don't take my word for it!
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

(** Here, we create a stream monad from a definite collection monad {!
    Monad.BaseCollectionM}. The inner monad will be used to represent the
    generations in the stream. The order of elements in each generation should
    not matter, so you might want to use a set or a bag. If you want to live
    life on the edge, just remember that your code should not depend on the
    order of elements within generations (you can, of course, depend on the
    order that generations appear in the stream). You can enforce this
    constraint by performing, say, a sort on each generation.  *)
module MakeStreamC(M : sig
    include BaseCollectionM
    include Applicative.Base with type 'a m := 'a m
end) :
sig
  include StreamC with type 'a t = 'a M.m LazyList.node_t
                  and type 'a m = 'a M.m LazyList.t
  include Applicative.Base with type 'a m := 'a m
end

(** {6 Transformers} *)
(** Monads support a certain amount of composition, allowing one to create a
    single monad which combines the properties of other monads. For instance,
    we might want a monad which reads from an environment and writes to a
    log. We can do this by transforming the reader monad with the writer
    transformer, or vice versa.
*)
(*    Every transformer comes with a [lift] function to promote computations in
    the inner monad to computations in the transformed monad.*)

(** The lazy monad transformer will wrap computated values in a thunk. *)
module LazyT(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad with type 'a m = 'a Lazy.t M.m
(*  val lift   : 'a M.m -> 'a m*)
end

(** The list monad transformer will add non-determinism to computations. We
    have not provided a transformer for lazy lists, since I'm not yet sure how
    to implement it. It would probably need a lazy version of map_m, but it's
    not clear to me how to write this, since whether the computations are
    strict will determine whether the argument has to be completely forced. *)
module ListT(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad with type 'a m = 'a list M.m
(*  val lift   : 'a M.m -> 'a m*)
end

(** The option monad transformer will allow computations to fail. *)
module OptionT(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad with type 'a m = 'a option M.m
(*  val lift   : 'a M.m -> 'a m*)
end

(** The state monad transformer will allow computations to read and write to a state. *)
module StateT(T : sig type s end)(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad

  val read   : T.s m
  val write  : T.s -> unit m
  val modify : (T.s -> T.s) -> unit m
  val run    : 'a m -> T.s -> (T.s * 'a) M.m
(*  val lift   : 'a M.m -> 'a m*)
end

(** The writer monad transformer will allow computations to write values. *)
module WriterT
  (W : Writer)(M : BatInterfaces.Monad) :
sig
  include BatInterfaces.Monad
  val listen : 'a m -> (W.t * 'a) m
  val write  : W.t -> unit m
  val pass   : 'a M.m -> 'a m
  val run    : 'a m -> (W.t * 'a) M.m
(*  val lift   : 'a M.m -> 'a m*)
end

(** {6 Transformers on Collections } *)
(** Sometimes, you might want to transform a collection monad, in such a way
    that functions such as {! BaseCollectionM.unique} behave in a sensible
    way by regarding None as smaller than any Some. Each transformer provides
    a function [cmp_on] with which the collection functions such as
    [BaseCollectionM.difference] are implemented. We also provide the [list]
    function for transformers. *)

(** Transformer allowing optional values in a collection. *)
module CollectionOpt(C : BaseCollectionM) :
sig
  include BaseCollectionM with type 'a m = 'a option C.m

  (** [cmp_on p] is a partial order satisfying:
         [None   = None]
         [None   < Some _]
         [Some x < Some y <=> x < y]
  *)
  val cmp_on : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

(*  val lift   : 'a C.m -> 'a m*)
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

  (** [cmp_on p] is the usual product of the ordering on ['a] with the ordering
  on [W.t]. *)
  val cmp_on : ('a -> 'a -> bool) -> 'a W.m -> 'a W.m -> bool

(*  val lift   : 'a C.m -> 'a m*)
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

  (** [cmp_on p] is the usual product of the ordering on ['a] with the ordering
  on [T.s]. *)
  val cmp_on : ('a -> 'a -> bool) -> (T.s * 'a) -> (T.s * 'a) -> bool

(*  val lift   : 'a C.m -> 'a m*)
end
