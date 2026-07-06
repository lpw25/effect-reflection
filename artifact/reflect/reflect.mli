module type Sig = sig type 'a t end

module Continuation (E : Sig) : sig
  type ('a, 'b) t
end

module Term (E : Sig) : sig
  type 'a t =
    | Return : 'a @@ global -> 'a t
    | Perform : 'r E.t @@ global * ('r, 'a) Continuation(E).t -> 'a t
end

module Handler (E : Sig) : sig
  type t
end

module Reflection (E : Sig) : sig
  val reify :
    (Handler(E).t @ local -> 'a) @ local -> 'a Term(E).t @ local

  val reify_global : (Handler(E).t @ local -> 'a) -> 'a Term(E).t

  val perform : 'a E.t -> (Handler(E).t @ local -> 'a)

  val continue :
    ('a, 'b) Continuation(E).t @ local -> 'a -> 'b Term(E).t @ local

  val continue_global : ('a, 'b) Continuation(E).t -> 'a -> 'b Term(E).t
end

module Sum (L : Sig) (R : Sig) : sig
  type 'a t =
    | Left : 'a L.t -> 'a t
    | Right : 'a R.t -> 'a t
end

module Select (L : Sig) (R : Sig) : sig
  val outl : Handler(Sum(L)(R)).t @ local -> Handler(L).t @ local

  val outr : Handler(Sum(L)(R)).t @ local -> Handler(R).t @ local
end
