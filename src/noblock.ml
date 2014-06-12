type 'a prog =
  | Return   of 'a
  | Block    of (string -> 'a async)
  | TryLater of 'a async
 and 'a async = string option list -> 'a prog * string option list

let rec map_async f x ss =
  match x ss with
  | Return x,ss    -> Return (f x),ss
  | Block bx,ss    -> (match ss with
                       | Some s :: ss -> map_async f (bx s) ss
                       | _            -> TryLater (map_async f x), ss)
  | TryLater x,ss  -> TryLater (map_async f x),ss

let rec map_prog f = function
  | Return x   -> Return (f x)
  | Block g    -> Block (fun s -> map_async f (g s))
  | TryLater a -> TryLater (map_async f a)

let return x ss = Return x, ss

let rec (>>=) x f ss =
  match x ss with
  | Return x,ss   -> f x [], ss
  | Block x',ss   -> (match ss with
                      | Some s :: ss -> let y,_ = (x' s >>= f) [] in y,ss
                      | _            -> TryLater (x >>= f), BatList.drop 1 ss)
  | TryLater x,ss -> TryLater (fun ss -> (x >>= f) ss),ss

let rec (<*>) f x ss =
  match f ss with
  | Return f,ss   -> map_async ((@@) f) x ss
  | Block f',ss   ->
     (match ss with
      | Some s :: ss -> ((fun ss -> let y,_ = f' s [] in y,ss) <*> x) ss
      | ss           -> (match x (BatList.drop 1 ss) with
                         | Return x,ss ->
                            TryLater (fun ss -> map_async ((|>) x) f ss),ss
                         | Block x',ss ->
                            TryLater (fun ss -> (f <*> x) ss),ss
                         | TryLater x,ss ->
                            TryLater (fun ss' -> (f <*> x) ss'),ss))
  | TryLater f,ss -> match x ss with
                     | Return x,ss        -> TryLater (map_async ((|>) x) f),ss
                     | Block x',ss ->
                        (match ss with
                         | Some s :: ss ->
                            (f <*> (fun ss -> let x,_ = x' s [] in x,ss)) ss
                         | ss ->
                            TryLater (fun ss -> (f <*> x) ss),(BatList.drop 1 ss))
                     | TryLater x,ss -> TryLater (fun ss -> (f <*> x) ss), ss
