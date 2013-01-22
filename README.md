ocaml-monad
===========

Description
-----------

A starter monad library in Ocaml. Monad library functions are generated from implementations of BatInterfaces.Monad.

We support monads with the additional "plus" structure, and provide a lazy variant useful for monads such as LazyList. 

The lazy lists here are lazier than those of BatLazyList. If you're writing really recursive lazy lists, you'll probably find you need these versions of the functions. Simple example: as of writing, concat in BatLazyList is implemented as a left fold with append. This makes it strict in the outer list, which isn't always what you want.

There are some usual suspect monads in the library. Basic monads are provided, as well as transformers for many of those monads. Warning: transformer code is verbose in Ocaml compared with Haskell.

Included is an implementation of Spivey's stream monad based on an abstract inner "bag" collection. Use lazy lists for a simple implementation. For a more complex example useful in, say, theorem proving, use TagTree. (See "A Combinator Language for Theorem Discovery (2012)" for the motivation.)

Applicatives are provided and the implementation of applicative functions overrides the implementation for monad functions. This means that functions such as sequence get defined in terms of pure and ap, not in terms of bind and return. I think this is the correct approach. For example, a TagTree is faster in its derived applicative functions than the equivalent derived monad functions, and it seems to me that's going to be true more often than it's false.

Installation
------------

Depends on oasis.

    $ oasis setup
    $ ocaml setup.ml -configure
    $ ocaml setup.ml -build
    $ ocaml setup.ml -install

For documentation:

    $ ocaml setup.ml -doc

