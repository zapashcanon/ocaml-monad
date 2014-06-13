type 'a prog =
  | Return   of 'a
  | TryLater of 'a t
 and 'a t = string option list -> 'a prog * string option list

let rec map_async f x ss =
  match x ss with
  | Return x,ss    -> Return (f x),ss
  | TryLater x,ss  -> TryLater (map_async f x),ss

let return x ss = Return x, ss

let rec (<*>) f x ss =
  match f ss with
  | Return f,ss   -> map_async ((@@) f) x ss
  | TryLater f,ss -> match x ss with
                     | Return x,ss   -> TryLater (map_async ((|>) x) f),ss
                     | TryLater x,ss -> TryLater (fun ss -> (f <*> x) ss), ss

let rec (>>=) x f ss =
  match x ss with
  | Return x,ss   -> f x []
  | TryLater x,ss -> TryLater (fun ss -> (x >>= f) ss),ss

module App = Applicative.Make(
                 struct
                   type 'a m = 'a t
                   let return = return
                   let (<*>) = (<*>)
                 end)

include Monad.Make(struct
                    type 'a m = 'a t
                    let return = return
                    let bind = (>>=)
                  end)

include (App : Applicative.Applicative with type 'a m := 'a m)
