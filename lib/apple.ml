open! Base

module Color = struct
  type t =
    | Red
    | Gold
  [@@deriving sexp_of]
end

type t =
  { location : Position.t
  ; mutable color : Color.t
  }
[@@deriving sexp_of]

let location t = t.location
let color t = t.color

let amount_to_grow t =
  match color t with
  | Red -> 2
  | Gold -> 2
;;

let rec create ~board ~snake ~consumed =
  let boardSquares = Board.all_locations board in
  let snakeSquares = Snake.all_locations snake in
  let filteredBoardSquares =
    List.filter boardSquares ~f:(fun square ->
        not (List.mem snakeSquares square ~equal:Position.equal))
  in
  if List.is_empty filteredBoardSquares
  then None
  else if (consumed + 1) % 5 <> 0
  then (
    let color = Color.Red in
    Some { location = List.random_element_exn filteredBoardSquares; color })
  else (
    let color = Color.Gold in
    Some { location = List.random_element_exn filteredBoardSquares; color })
;;

module Exercises = struct
  let exercise05 = create
  let create_with_location location = { location; color = Color.Red }
end
