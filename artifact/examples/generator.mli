open Reflect

module Gen : sig
  type 'a t =
    | Gen : int -> unit t
end

type ints =
  | Finished
  | More of int * (unit -> ints)

val handle_gen : (Handler(Gen).t @ local -> unit) -> ints

val gen : Handler(Gen).t @ local -> int -> unit
