open Reflect

module Refl (O : Op) : sig
  val reflect : 'a Term(O).t -> (Handler(O).t @ local -> 'a)
end = struct
  open Term(O)
  open Reflection(O)

  let rec reflect t h =
    match t with
    | Return x -> x
    | Op(op, k) -> reflect (k (perform op h)) h

end

module State = struct
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end

module State_eff = Reflection(State)

let handle_state (f : Handler(State).t @ local -> 'a) (i : int) : 'a =
  let rec loop (s : int) : 'a Term(State).t @ local -> 'a  = function
    | Return x -> x
    | Op(Get, k) -> loop s (k s)
    | Op(Set s', k) -> loop s' (k ())
  in
  loop i (State_eff.reify_local f) [@nontail]

let get (h : Handler(State).t @ local) : int =
  State_eff.perform Get h

let set (h : Handler(State).t @ local) (i : int) : unit =
  State_eff.perform (Set i) h

module M : sig

  type handler

  val handle_state : int -> (handler @ local -> 'a) @ local -> 'a
  val get : handler @ local -> int
  val set : handler @ local -> int -> unit

end = struct

  open Term(State)

  let handle_state i f =
    let rec loop (s : int) = function
      | Return x -> x
      | Op(Get, k) -> loop s (k s)
      | Op(Set s', k) -> loop s' (k ())
    in
    loop i (State_eff.reify_local f) [@nontail]

  type handler = Handler(State).t

  let get h =
    State_eff.perform Get h

  let set h i =
    State_eff.perform (Set i) h

end

module N : sig

  type handler

  val handle_state : int -> (handler @ local -> 'a) @ local -> 'a
  val get : handler @ local -> int
  val set : handler @ local -> int -> unit

end = struct

  type handler = int ref

  let handle_state i f = f (ref i)
  let get r = !r
  let set r i = r := i

end

open M

let rec iter f = function
  | [] -> ()
  | x :: xs -> f x; iter f xs

let total l =
  handle_state 0 (fun h ->
    iter (fun x -> set h (get h + x)) l;
    get h)

module Gen = struct
  type 'a t =
    | Gen : int -> unit t
end

type ints =
  | Finished
  | More of int * (unit -> ints)

module O : sig
  val handle_gen : (Handler(Gen).t @ local -> unit) -> ints
end = struct
  module Gen_eff = Reflection(Gen)
  open Term(Gen)

  let handle_gen f =
    let rec loop = function
      | Return () -> Finished
      | Op(Gen i, k) -> More(i, fun () -> loop (k ()))
    in
    loop (Gen_eff.reify f)
end

module Deferred = struct
  type 'a t = 'a

  let return x = x

  let bind x f = f x
end

module File = struct

  let read _ = "hello"

  let write _ _ = ()

end

module Await = struct
  type 'a t =
    | Await : 'a Deferred.t -> 'a t
end

module P = struct
  module Await_eff = Reflection(Await)
  open Term(Await)

  let handle_await f =
    let rec loop = function
      | Return x -> Deferred.return x
      | Op(Await d, k) ->
          Deferred.bind d (fun x -> loop (k x))
    in
    loop (Await_eff.reify f)

  let await h d =
    Await_eff.perform (Await d) h

  let copy_file h src dst =
    let s = await h (File.read src) in
    await h (File.write dst s)
end

open O
open P

let foo i files =
  handle_await (fun ch ->
    handle_state i (fun sh ->
      iter (fun fl ->
        let s = await ch (File.read fl) in
        let i = int_of_string s in
        set sh (i + get sh)) files;
      get sh)[@nontail])

module H : sig

  module Half_term (L : Op) (R : Op) : sig
    type 'a t =
      | Return : 'a @@ global -> 'a t
      | Op : 'r R.t @@ global * ('r -> Handler(L).t @ local -> 'a t) -> 'a t
  end

  module Half_reflection (L : Op) (R : Op) : sig
    val reify :
      (Handler(L).t @ local -> Handler(R).t @ local -> 'a)
      -> Handler(L).t @ local -> 'a Half_term(L)(R).t

    val reflect :
      (Handler(L).t @ local -> 'a Half_term(L)(R).t)
      -> Handler(L).t @ local -> Handler(R).t @ local -> 'a
  end

end = struct

  module Half_term (L : Op) (R : Op) = struct
    type 'a t =
      | Return : 'a @@ global -> 'a t
      | Op : 'r R.t @@ global * ('r -> Handler(L).t @ local -> 'a t) -> 'a t
  end

  module Half_reflection (L : Op) (R : Op) = struct
    module L_eff = Reflection(L)
    module R_eff = Reflection(R)
    module Sum_eff = Reflection(Sum(L)(R))
    module Proj = Project(L)(R)
    open Term(Sum(L)(R))
    module HT = Half_term(L)(R)

    let reify (f : Handler(L).t @ local -> Handler(R).t @ local -> 'a) lh =
      let rec loop lh = function
        | Return v ->
            HT.Return v
        | Op(Left op, k) ->
            loop lh (k (L_eff.perform op lh))
        | Op(Right op, k) ->
            HT.Op(op, (fun res lh -> loop lh (k res)))
      in
      loop lh (Sum_eff.reify (fun h -> f (Proj.outl h) (Proj.outr h) [@nontail]))

    let rec reflect (f : Handler(L).t @ local -> 'a Half_term(L)(R).t) lh rh =
      match f lh with
      | Return x -> x
      | Op(op, k) -> reflect (k (R_eff.perform op rh)) lh rh
  end

end

open H

type aints =
  | Finished
  | More of int * (Handler(Await).t @ local -> aints)

module Q : sig

  val handle_gen_in_await :
    (Handler(Await).t @ local -> Handler(Gen).t @ local -> unit)
    -> Handler(Await).t @ local
    -> aints

end = struct

  module Await_gen_eff = Half_reflection(Await)(Gen)
  open Half_term(Await)(Gen)

  let handle_gen_in_await f ah =
    let rec loop = function
      | Return () -> Finished
      | Op(Gen i, k) -> More(i, fun ah -> loop (k () ah))
    in
    loop (Await_gen_eff.reify f ah)

end


module R : sig

  type 's handler

  val handle_state : 's -> ('s handler @ local -> 'a) @ local -> 'a
  val get : 's handler @ local -> 's
  val set : 's handler @ local -> 's -> unit

end = struct
  module State = struct
    type ('a, 's) t =
      | Get : ('s, 's) t
      | Set : 's -> (unit, 's) t
  end

  module State_eff = Reflection1(State)
  open Term1(State)

  let handle_state (type s) i f =
    let rec loop (s : s) : (_, s) Term1(State).t @ local -> _ = function
      | Return x -> x
      | Op(Get, k) -> loop s (k s)
      | Op(Set s', k) -> loop s' (k ())
    in
    loop i (State_eff.reify_local f) [@nontail]

  type 's handler = 's Handler1(State).t

  let get h =
    State_eff.perform Get h

  let set h i =
    State_eff.perform (Set i) h

end
