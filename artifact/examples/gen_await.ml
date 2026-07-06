open Reflect
open Generator
open Aio

module Await_eff = Reflection(Await)
module Eff = Reflection(Sum(Await)(Gen))
module Sel = Select(Await)(Gen)

type aints =
  | Finished
  | More of int * (Handler(Await).t @ local -> aints)

let handle_gen_in_await f ah =
  let rec loop ah : unit Term(Sum(Await)(Gen)).t -> aints = function
    | Return () -> Finished
    | Perform(Left op, k) ->
        loop ah (Eff.continue_global k (Await_eff.perform op ah))
    | Perform(Right(Gen i), k) ->
        More(i, fun ah -> loop ah (Eff.continue_global k ()))
  in
  loop ah (Eff.reify_global (fun h -> f (Sel.outl h) (Sel.outr h) [@nontail]))
