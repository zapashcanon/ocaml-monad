open BatPervasives

module rec TermScope : Scope.Scope
           with type 'a m = 'a BaseM.m = Scope.Make(Monad.Make(BaseM))
   and BaseM :
         sig
           type 'a m =
             | Var of 'a
             | App of 'a m * 'a m
             | Abs of (string,'a) TermScope.scope
           val print_term : string m -> string
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

       let bracket cond s = if cond then "(" ^ s ^ ")" else s

       let app_prec = 1
       let print_term =
         let rec print : 'a. int -> int -> (int -> int -> 'a -> string)
                         -> 'a m -> string =
           fun i outer_prec print_v -> function
           | Var x -> print_v i outer_prec x
           | App (s,t) ->
              bracket (outer_prec <= app_prec)
                      (print i (app_prec+1) print_v s
                       ^ " "
                       ^ print i app_prec print_v t)
           | Abs body ->
              "λ. "
              ^ print 0 10 (fun j outer_prec ->
                            function
                            | Scope.B b -> b ^ "_" ^ string_of_int j
                            | Scope.F tm -> print (i+1) outer_prec print_v tm)
                      (TermScope.unscope body)
         in
         print 0 10 (fun i _ -> identity)
     end
