open Reflect

(* The argument of [Fork] mentions the capability type of the effect
   being defined, so the signature is a recursive module. *)
module rec Coop : sig
  type 'a t =
    | Fork : (Handler(Coop).t @ local -> unit) -> unit t
    | Yield : unit t
end = Coop

module Coop_eff = Reflection(Coop)

let run (main : Handler(Coop).t @ local -> unit) : unit =
  let queue = Queue.create () in
  let next () =
    match Queue.take_opt queue with
    | None -> ()
    | Some thread -> thread ()
  in
  let rec step : unit Term(Coop).t -> unit = function
    | Return () -> next ()
    | Perform(Yield, k) ->
        Queue.push
          (fun () -> step (Coop_eff.continue_global k ())) queue;
        next ()
    | Perform(Fork f, k) ->
        Queue.push (fun () -> step (Coop_eff.reify_global f)) queue;
        step (Coop_eff.continue_global k ())
  in
  step (Coop_eff.reify_global main)

let fork h f = Coop_eff.perform (Fork f) h

let yield h = Coop_eff.perform Yield h
