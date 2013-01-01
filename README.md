ocaml-monad
===========

Some monad stuff in Ocaml. This currently contains stuff that I need for my HOL Light work, and I've avoided certain bits of Ocaml which would have made things nicer, such as substituting inside a signature (7.16 of the manual). These do not currently play nicely with HOL Light's preprocessor.

My lazy lists are lazier than those of BatLazyList. If you're writing really recursive lazy lists, you'll probably find you need these versions of the functions. Simple example: as of writing, concat in BatMonad is implemented as a left fold with append. This makes it strict in the outer list, which isn't always what you want.

There are some usual suspect monads in the library, but it's currently all transformers, because that's what I needed for my stuff. I don't necessary like transformers, and they gave me headaches in Ocaml.

I've also implemented Spivey's stream monad, but I've generalised it a bit, and provided a data structure in labelled trees. See "A Combinator Language for Theorem Discovery (2012)" for the motivation.

Applicatives are in there too, and monads inherit the interface, and the implementation of applicative functions overrides the implementation for monad functions. This means that functions such as sequence get defined in terms of pure and ap, not in terms of bind and return. I think this is probably the correct approach. For example, trees.ml is faster in its derived applicative functions than the equivalent derived monad functions, and it seems to me that's going to be true more often than its false.

