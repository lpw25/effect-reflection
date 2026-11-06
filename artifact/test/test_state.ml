open Examples

let () =
  let total =
    State.handle_state 0 (fun h ->
      Local_list.iter (fun x -> State.set h (State.get h + x)) [1; 2; 3; 4];
      State.get h)
  in
  Printf.printf "state: %d\n" total
