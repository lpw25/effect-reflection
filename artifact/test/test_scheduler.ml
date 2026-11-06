open Examples

let () =
  print_endline "scheduler:";
  Scheduler.run (fun h ->
    Scheduler.fork h (fun h ->
      print_endline "  a1"; Scheduler.yield h; print_endline "  a2");
    Scheduler.fork h (fun h ->
      print_endline "  b1"; Scheduler.yield h; print_endline "  b2");
    print_endline "  main")
