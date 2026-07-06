open Reflect

type aints =
  | Finished
  | More of int * (Handler(Aio.Await).t @ local -> aints)

val handle_gen_in_await :
  (Handler(Aio.Await).t @ local -> Handler(Generator.Gen).t @ local -> unit)
  -> Handler(Aio.Await).t @ local -> aints
