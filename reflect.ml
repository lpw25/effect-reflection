let local_match_with (f @ local) (x @ local) (h : _ Effect.Deep.handler) =
  assert false
(* Don't actually support handlers closing over local values in the
   runtime yet because our GC can't deal with pointers between
   stacks. Stub out a local version of [Effect.Deep.match_with] for
   now. *)

module type Op = sig type 'a t end

module Term (O : Op) = struct
  type 'a t =
    | Return : 'a @@ global -> 'a t
    | Op : 'r O.t @@ global * ('r -> 'a t) -> 'a t
end

module Handler (O : Op) = struct

  type t = { perform : 'r. 'r O.t -> 'r }

end

module Reflection (O : Op) = struct
  open Term(O)
  open Handler(O)

  let reify f =
    let module Eff =
      struct type 'a Effect.t += C : 'a O.t -> 'a Effect.t end
    in
    let handler =
      { perform = fun o -> Effect.perform (Eff.C o) }
    in
    Effect.Deep.match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc = (fun eff ->
                  match eff with
                  | Eff.C o ->
                      Some (fun k ->
                              Op(o, fun x -> Effect.Deep.continue k x))
                  | _ -> None); }

  let reify_local f = exclave_
    let module Eff =
      struct type 'a Effect.t += C : 'a O.t -> 'a Effect.t end
    in
    let handler = { perform = fun o -> Effect.perform (Eff.C o) } in
    local_match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc = (fun eff ->
                  match eff with
                  | Eff.C o ->
                      Some (fun k ->
                              Op(o, fun x -> Effect.Deep.continue k x))
                  | _ -> None); }

  let perform t =
   fun h -> h.perform t

end

module Sum (L : Op) (R : Op) = struct
  type 'a t =
    | Left : 'a L.t -> 'a t
    | Right : 'a R.t -> 'a t
end

module Project (L : Op) (R : Op) = struct
  open Sum(L)(R)

  let outl ({perform} : Handler(Sum(L)(R)).t) : Handler(L).t = exclave_
    {perform = fun op -> perform (Left op)}

  let outr ({perform} : Handler(Sum(L)(R)).t) : Handler(R).t = exclave_
    {perform = fun op -> perform (Right op)}

end

module type Op1 = sig type ('a, 'e) t end

module Term1 (O : Op1) = struct
  type ('a, 'e) t =
    | Return : 'a @@ global -> ('a, 'e) t
    | Op : ('r, 'e) O.t @@ global * ('r -> ('a, 'e) t) -> ('a, 'e) t
end

module Handler1(O : Op1) = struct
  type 'e t = { perform : 'r. ('r, 'e) O.t -> 'r }
end

module Reflection1 (O : Op1) = struct
  open Term1(O)
  open Handler1(O)

  let reify (type e) f =
    let module Eff =
      struct type 'a Effect.t += C : ('a, e) O.t -> 'a Effect.t end
    in
    let handler =
      { perform = fun o -> Effect.perform (Eff.C o) }
    in
    Effect.Deep.match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc = (fun eff ->
                  match eff with
                  | Eff.C o ->
                      Some (fun k ->
                              Op(o, fun x -> Effect.Deep.continue k x))
                  | _ -> None); }

  let reify_local (type e) f = exclave_
    let module Eff =
      struct type 'a Effect.t += C : ('a, e) O.t -> 'a Effect.t end
    in
    let handler = { perform = fun o -> Effect.perform (Eff.C o) } in
    local_match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc = (fun eff ->
                  match eff with
                  | Eff.C o ->
                      Some (fun k ->
                              Op(o, fun x -> Effect.Deep.continue k x))
                  | _ -> None); }

  let perform t =
   fun h -> h.perform t

end
