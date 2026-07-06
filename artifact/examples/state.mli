open Reflect

module State : sig
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end

val handle_state : int -> (Handler(State).t @ local -> 'a) @ local -> 'a

val get : Handler(State).t @ local -> int

val set : Handler(State).t @ local -> int -> unit
