open Reflect
open Async

module Await : sig
  type 'a t =
    | Await : 'a Deferred.t -> 'a t
end

val handle_await : (Handler(Await).t @ local -> 'a) -> 'a Deferred.t

val await : Handler(Await).t @ local -> 'a Deferred.t -> 'a
