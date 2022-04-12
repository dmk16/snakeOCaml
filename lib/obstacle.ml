open! Base

type t = { (* [direction] represents the orientation of the snake's head. *)
mutable obstacle : Position.t
}
[@@deriving sexp_of, fields]