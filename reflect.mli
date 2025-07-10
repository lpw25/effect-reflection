module type Op = sig type 'a t end

module Term (O : Op) : sig
  type 'a t =
    | Return : 'a @@ global -> 'a t
    | Op : 'r O.t @@ global * ('r -> 'a t) -> 'a t
end

module Handler(O : Op) : sig
  type t
end

module Reflection (O : Op) : sig
  val reify : (Handler(O).t @ local -> 'a) -> 'a Term(O).t

  val reify_local :
    (Handler(O).t @ local -> 'a) @ local -> 'a Term(O).t @ local

  val perform : 'a O.t -> (Handler(O).t @ local -> 'a)
end

module Sum (L : Op) (R : Op) : sig
  type 'a t =
    | Left : 'a L.t -> 'a t
    | Right : 'a R.t -> 'a t
end

module Project(L : Op) (R : Op) : sig
  val outl : Handler(Sum(L)(R)).t @ local -> Handler(L).t @ local

  val outr : Handler(Sum(L)(R)).t @ local -> Handler(R).t @ local

end

module type Op1 = sig type ('a, 'e) t end

module Term1 (O : Op1) : sig
  type ('a, 'e) t =
    | Return : 'a @@ global -> ('a, 'e) t
    | Op : ('r, 'e) O.t @@ global * ('r -> ('a, 'e) t) -> ('a, 'e) t
end

module Handler1(O : Op1) : sig
  type 'e t
end

module Reflection1 (O : Op1) : sig
  val reify : ('e Handler1(O).t @ local -> 'a) -> ('a, 'e) Term1(O).t

  val reify_local :
    ('e Handler1(O).t @ local -> 'a) @ local -> ('a, 'e) Term1(O).t @ local

  val perform : ('a, 'e) O.t -> ('e Handler1(O).t @ local -> 'a)
end

