open OUnit

module type EquivMonad =
sig
  include BatMonad.S 
  val equiv : 'a m -> 'a m -> bool
end

module type EquivPlus =
sig
  include Monad.BasePlus 
  val equiv : 'a m -> 'a m -> bool
end

module BuildMonadTests(M : EquivMonad) =
struct
  module B = Monad.Make(M)
  let law1 x     = M.equiv (B.lift1 (fun x -> x) x) x
  let law2 f g x = M.equiv
    (B.lift1 f (B.lift1 g x))
    (B.lift1 (fun x -> f (g x)) x)
  let law3 f x   = M.equiv (B.lift1 f (B.return x)) (B.return (f x))
  let law4 f x   =
    M.equiv (B.lift1 f (B.join x))
      (B.join (B.lift1 (B.lift1 f) x))
  let law5 x     = M.equiv (B.join (B.return x)) x
  let law6 x     = M.equiv (B.join (B.lift1 B.return x)) x
  let law7 x     = M.equiv (B.join (B.join x)) (B.join (B.lift1 B.join x))
end

  
module BuildPlusTests(M : EquivPlus) =
struct
  module B = Monad.MakePlus(M)
  let law1 x = M.equiv (B.plus (B.zero ()) x) x
  let law2 x = M.equiv x (B.plus (B.zero ()) x)

end
