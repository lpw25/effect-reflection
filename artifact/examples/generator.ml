open Reflect

module Gen = struct
  type 'a t =
    | Gen : int -> unit t
end

module Gen_eff = Reflection(Gen)

type ints =
  | Finished
  | More of int * (unit -> ints)

let handle_gen f =
  let rec loop : unit Term(Gen).t -> ints = function
    | Return () -> Finished
    | Perform(Gen i, k) ->
        More(i, fun () -> loop (Gen_eff.continue_global k ()))
  in
  loop (Gen_eff.reify_global f)

let gen h i =
  Gen_eff.perform (Gen i) h
