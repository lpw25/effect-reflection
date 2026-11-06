open Reflect

module rec Coop : sig
  type 'a t =
    | Fork : (Handler(Coop).t @ local -> unit) -> unit t
    | Yield : unit t
end

val run : (Handler(Coop).t @ local -> unit) -> unit

val fork : Handler(Coop).t @ local -> (Handler(Coop).t @ local -> unit) -> unit

val yield : Handler(Coop).t @ local -> unit
