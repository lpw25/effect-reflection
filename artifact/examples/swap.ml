open Reflect
open State

(* Two pieces of state used by one function: the capabilities are
   ordinary parameters, in whatever order we choose. *)
let swap (h1 : Handler(State).t @ local) (h2 : Handler(State).t @ local) : unit =
  let x = get h1 in
  set h1 (get h2);
  set h2 x

(* The inner handled computation closes over the local capability [h1]
   of the outer handler, so this needs the paper's [reify]-based
   handler. It type-checks, but cannot be run with the released
   runtime. *)
let swapped a b =
  handle_state_local a (fun h1 ->
    handle_state_local b (fun h2 ->
      swap h1 h2;
      (get h1, get h2)) [@nontail])
