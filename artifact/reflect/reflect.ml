module type Sig = sig type 'a t end

module Term (E : Sig) = struct
  type 'a t =
    | Return : 'a @@ global -> 'a t
    | Perform : 'r E.t @@ global * ('r -> 'a t) -> 'a t
end

module Continuation (E : Sig) = struct
  type ('a, 'b) t = 'a -> 'b Term(E).t
end

module Handler (E : Sig) = struct
  type t = { perform : 'r. 'r E.t -> 'r }
end

module Reflection (E : Sig) = struct
  open Term(E)
  open Handler(E)

  let reify _ =
    failwith
      "Reflect.Reflection().reify: the released OxCaml runtime does not \
       yet support effect handlers that close over local values, so \
       local reification is not executable; use reify_global where \
       possible"

  let reify_global f =
    let module Eff =
      struct type 'a Effect.t += C : 'a E.t -> 'a Effect.t end
    in
    let handler = { perform = fun o -> Effect.perform (Eff.C o) } in
    Effect.Deep.match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc = (fun eff ->
                  match eff with
                  | Eff.C o ->
                      Some (fun k ->
                              Perform(o, fun x -> Effect.Deep.continue k x))
                  | _ -> None); }

  let perform t =
    fun h -> h.perform t

  let continue (k @ local) x = k x

  let continue_global k x = k x

end

module Sum (L : Sig) (R : Sig) = struct
  type 'a t =
    | Left : 'a L.t -> 'a t
    | Right : 'a R.t -> 'a t
end

module Select (L : Sig) (R : Sig) = struct
  open Sum(L)(R)

  let outl ({perform} : Handler(Sum(L)(R)).t) : Handler(L).t = exclave_
    {perform = fun op -> perform (Left op)}

  let outr ({perform} : Handler(Sum(L)(R)).t) : Handler(R).t = exclave_
    {perform = fun op -> perform (Right op)}

end
