open! Base

type t =
  | Left
  | Up
  | Right
  | Down
[@@deriving sexp_of]

let next_position t { Position.row; col } : Position.t =
  match t with
  | Left -> { row; col = col - 1 }
  | Right -> { row; col = col + 1 }
  | Up -> { row = row + 1; col }
  | Down -> { row = row - 1; col }
;;

let of_key key =
  match key with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None
;;

module Exercises = struct
  let exercise02a = of_key
end
