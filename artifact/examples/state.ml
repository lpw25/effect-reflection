open Reflect

module State = struct
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end

module State_eff = Reflection(State)

let handle_state i f =
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
