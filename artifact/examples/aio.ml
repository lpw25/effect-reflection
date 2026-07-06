open Async
open Reflect

module Await = struct
  type 'a t =
    | Await : 'a Deferred.t -> 'a t
end

module Await_eff = Reflection(Await)

let handle_await f =
  let rec loop : 'a Term(Await).t -> 'a Deferred.t = function
    | Return x -> Deferred.return x
    | Perform(Await d, k) ->
        Deferred.bind d ~f:(fun x -> loop (Await_eff.continue_global k x))
  in
  loop (Await_eff.reify_global f)

let await h d =
  Await_eff.perform (Await d) h
