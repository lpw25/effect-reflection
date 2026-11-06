(* The paper assumes a [List.iter] whose function parameter is local,
   as in Jane Street's Base library. OxCaml's copy of the OCaml
   standard library does not annotate [List.iter] that way, so the
   examples use this version instead. *)
let rec iter (f @ local) l =
  match l with
  | [] -> ()
  | x :: l -> f x; iter f l
