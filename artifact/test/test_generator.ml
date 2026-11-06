open Examples

let () =
  let rec print_ints = function
    | Generator.Finished -> ()
    | Generator.More (i, k) -> Printf.printf "%d " i; print_ints (k ())
  in
  print_string "generator: ";
  print_ints (Generator.ints_of_list [1; 2; 3]);
  print_newline ()
