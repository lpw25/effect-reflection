open Reflect

module State : sig
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end

(** Runnable variant of the paper's handler, using [reify_global]. *)
val handle_state : int -> (Handler(State).t @ local -> 'a) -> 'a

(** The paper's handler, using [reify]; type-checks but is not
    executable with the released runtime. *)
val handle_state_local : int -> (Handler(State).t @ local -> 'a) @ local -> 'a

val get : Handler(State).t @ local -> int

val set : Handler(State).t @ local -> int -> unit
