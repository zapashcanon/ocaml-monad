type fd = Unix.file_descr

type out_update =
  | AddWrite of int * fd * string
  | RemoveWrite of int

type in_update =
  | AddRead of int * fd
  | RemoveRead of int

type s = (int * (int * string) list) * (int * int list)
type w = in_update list * out_update list

let zero_w = [],[]
let plus_w (inps,outs) (inps',outs')
  = inps @ inps', outs @ outs'

let with_w w (x,s,w') = x,s,plus_w w w'

type 'a prog =
  | Return   of 'a
  | TryLater of 'a t
 and 'a t = s -> 'a prog * s * w

let rec map_async f x s =
  match x s with
  | Return x,s,w   -> Return (f x),s,w
  | TryLater x,s,w -> TryLater (map_async f x),s,w

let return x s = Return x, s, zero_w

let rec (<*>) f x s =
  match f s with
  | Return f,s,w   -> with_w w (map_async ((@@) f) x s)
  | TryLater f,s,w -> match x s with
                         | Return x,s,w'   ->
                            TryLater (map_async ((|>) x) f),s,plus_w w w'
                         | TryLater x,s,w' ->
                            TryLater (fun ss -> (f <*> x) s),s,plus_w w w'

let rec (>>=) x f s =
  match x s with
  | Return x,s,w   -> with_w w (f x s)
  | TryLater x,s,w -> TryLater (fun ss -> (x >>= f) s),s,w

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

let read_line fd =
  let rec read = function
    | (pos,(pos',line)::inps), outs when pos = pos'
      -> Return line, ((pos+1,inps),outs), ([RemoveRead pos],[])
    | (pos,inps), outs
      -> TryLater read, ((0,inps),outs), zero_w in
  fun ((pos,_),_ as s) -> TryLater read, s, ([AddRead (pos,fd)],[])

let write_line fd str =
  let rec write = function
    | inps, (pos,pos'::outs) when pos = pos'
      -> Return (), (inps,(0,outs)), ([],[RemoveWrite pos])
    | inps, (pos,outs)
      -> TryLater write, (inps,(pos+1,outs)), zero_w in
  fun (_,(pos,_) as s) -> TryLater write, s, ([],[AddWrite (pos,fd,str)])

let fds_to_select fds =
  let _,_,fds = BatList.fold_right
                  (fun fd (i,seen,fds) ->
                   if BatList.mem fd seen then
                     i+1,seen,fds
                   else
                     i+1,fd::seen,((i,fd)::fds))
                  fds (0,[],[]) in
  fds

let select in_fds out_fds =
  BatList.(
    let in_fds', out_fds', _ =
      Unix.select (map snd in_fds) (map (fun (_,(_,fd)) -> fd) out_fds) [] (-1.0) in
    filter (fun (_,fd) -> mem fd in_fds') in_fds,
    filter (fun (_,(_,fd)) -> mem fd out_fds') out_fds)

let rec read_all = function
  | []                -> []
  | (p,in_fd)::in_fds -> try let buf = BatString.make 10 (char_of_int 0) in
                             let n = Unix.read in_fd buf 0 10 in
                             (p,BatString.sub buf 0 n)::read_all in_fds
                         with Unix.Unix_error (Unix.EAGAIN,_,_) -> read_all in_fds

let rec write_all = function
  | []                      -> []
  | (p,(str,out_fd))::out_fds ->
     try let _ = Unix.write out_fd str 0 (BatString.length str) in
         p::write_all out_fds
     with Unix.Unix_error (Unix.EAGAIN,_,_) -> write_all out_fds

let rec update_ctx x = update_ctx x

let run x =
  let ctx = [],[] in
  let rec run ctx x s =
    match x s with
    | Return y,((_,[]),(_,[])),([],[]) -> y
    | Return _,(_,(_,[])),_            -> raise (Failure "dangling registrations")
    | Return _,((_,[]),_),_            -> raise (Failure "dangling registrations")
    | Return _,_,_                     -> raise (Failure "unused blocks")
    | TryLater x,((_,[]),(_,[])),(inps,outs) ->
       let in_fds,out_fds as ctx = update_ctx ctx inps outs in
       let in_fds,out_fds = select (fds_to_select in_fds) (fds_to_select out_fds) in
       run ctx x ((0,read_all in_fds),(0,write_all out_fds))
    | TryLater _,_,_ -> raise (Failure "unused blocks") in
  run ([],[]) x ((0,[]),(0,[]))
