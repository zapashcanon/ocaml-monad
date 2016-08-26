open BatPervasives

module rec TermScope : Scope.Scope
           with type 'a m = 'a BaseM.m = Scope.Make(Monad.Make(BaseM))
   and BaseM :
         sig
           type 'a m =
             | Var of 'a
             | App of 'a m * 'a m
             | Abs of (string,'a) TermScope.scope
           include BatInterfaces.Monad with type 'a m := 'a m
         end =
     struct
       module TermScopeM : Monad.Monad
              with type 'a m = (string,'a) TermScope.scope
                  = TermScope.Monad(struct type b = string end)

       type 'a m =
         | Var of 'a
         | App of 'a m * 'a m
         | Abs of (string,'a) TermScope.scope

       let return x = Var x
       let rec bind x f =
         match x with
         | Var x -> f x
         | App (s,t) -> App (bind s f, bind t f)
         | Abs body -> Abs (TermScopeM.(>>=) body (TermScope.lift % f))
     end

module TermTraversable =
  Traversable.Make(
      struct
        type 'a t = 'a BaseM.m
        module BaseOfA(A : Applicative.Base) =
          struct
            include Applicative.Make(A)
            type 'a t = 'a BaseM.m

            let bifold f g = function
              | Scope.B x -> f x
              | Scope.F x -> g x

            (* This is duplicated from scope.ml, but I'm not sure it's possible to
            abstract this properly: the Traversable needs to be a recursive, but this
            is not safe since Traversable contains a functor. *)
            let rec traverse : 'a 'b. ('a -> 'b m) -> 'a t -> 'b t m =
              fun f -> function
                    | BaseM.Var x -> lift1 (fun x -> BaseM.Var x) (f x)
                    | BaseM.App (s,t) ->
                       lift2 (fun s t -> BaseM.App (s,t))
                             (traverse f s)
                             (traverse f t)
                    | BaseM.Abs bod ->
                       traverse (bifold (fun b -> A.return (Scope.B b))
                                        (lift1 (fun x -> Scope.F x) % traverse f))
                                (TermScope.unscope bod)
                       |> lift1 (fun body -> BaseM.Abs (TermScope.scope body))

          end
      end)

module Term = struct
    include TermScope
    include (BaseM : BatInterfaces.Monad with type 'a m := 'a m)

    let bracket cond s = if cond then "(" ^ s ^ ")" else s

    let app_prec = 1
    let print_term =
      let rec print : 'a. int -> int -> (int -> int -> 'a -> string)
                      -> 'a m -> string =
        fun i outer_prec print_v ->
        function
        | BaseM.Var x -> print_v i outer_prec x
        | BaseM.App (s,t) ->
           bracket (outer_prec <= app_prec)
                   (print i (app_prec+1) print_v s
                    ^ " "
                    ^ print i app_prec print_v t)
        | BaseM.Abs body ->
           "λ. "
           ^ print 0 10 (fun j outer_prec ->
                         function
                         | Scope.B b -> b ^ "_" ^ string_of_int j
                         | Scope.F tm -> print (i+1) outer_prec print_v tm)
                   (TermScope.unscope body)
      in
      print 0 10 (fun i _ -> identity)
  end
