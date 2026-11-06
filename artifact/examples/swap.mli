open Reflect
open State

val swap : Handler(State).t @ local -> Handler(State).t @ local -> unit

val swapped : int -> int -> int * int
