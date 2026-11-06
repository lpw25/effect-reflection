open Reflect

module State = struct
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end

module State_eff = Reflection(State)

(* The paper's handler uses [reify], which the released OxCaml runtime
   cannot yet execute (see the paper's implementation section). This
   version uses [reify_global] instead, which is preferred here because
   it is runnable in the artifact. The price is that the handled
   function must be global. *)
let handle_state i f =
  let rec loop (s : int) : 'a Term(State).t -> 'a = function
    | Return x -> x
    | Perform(Get, k) -> loop s (State_eff.continue_global k s)
    | Perform(Set s', k) -> loop s' (State_eff.continue_global k ())
  in
  loop i (State_eff.reify_global f)

(* The handler exactly as in the paper. It type-checks, but [reify] is
   a stub in the released runtime so it cannot be run. It is needed by
   the two-state example in [swap.ml], whose inner handled computation
   closes over the local capability of the outer handler. *)
let handle_state_local i f =
  let rec loop (s : int) : 'a Term(State).t @ local -> 'a = function
    | Return x -> x
    | Perform(Get, k) -> loop s (State_eff.continue k s) [@nontail]
    | Perform(Set s', k) -> loop s' (State_eff.continue k ()) [@nontail]
  in
  loop i (State_eff.reify f) [@nontail]

let get h =
  State_eff.perform Get h

let set h i =
  State_eff.perform (Set i) h
