open! Base

type t =
  { mutable snake : Snake.t
  ; mutable game_state : Game_state.t
  ; mutable apple : Apple.t
  ; mutable consumed : int
  ; board : Board.t
  ; mutable obstacles : Position.t list
  }
[@@deriving sexp_of]

let to_string { snake; game_state; apple; board; consumed ; obstacles} =
  Core.sprintf
    !{|Game state: %{sexp:Game_state.t}
Apple: %{sexp:Apple.t}
Board: %{sexp:Board.t}
Consumed: %d
Snake:
%s |}
    game_state
    apple
    board
    consumed
    (Snake.to_string ~indent:2 snake)
;;

let create ~height ~width ~initial_snake_length =
  let board = Board.create ~height ~width in
  let snake = Snake.create ~length:initial_snake_length in
  let apple = Apple.create ~board ~snake ~consumed:0 in
  let consumed = 0 in
  let obstacles = [] in
  match apple with
  | None -> failwith "unable to create initial apple"
  | Some apple ->
    let t = { snake; apple; game_state = In_progress; board; consumed ; obstacles} in
    if List.exists (Snake.all_locations snake) ~f:(fun pos ->
           not (Board.in_bounds t.board pos))
    then failwith "unable to create initial snake"
    else t
;;

let snake t = t.snake
let apple t = t.apple
let game_state t = t.game_state
let consumed t = t.consumed
let obstacles t = t.obstacles

let handle_key t key =
  let newdirection = Direction.of_key key in
  match newdirection with
  | Some Up -> Snake.set_direction t.snake Up
  | Some Down -> Snake.set_direction t.snake Down
  | Some Left -> Snake.set_direction t.snake Left
  | Some Right -> Snake.set_direction t.snake Right
  | None -> ()
;;

let check_for_collisions t =
  let head = Snake.head t.snake in
  if Board.in_bounds t.board head then (
    if (List.mem t.obstacles head ~equal:Position.equal)
      then t.game_state <- Game_over "Obstacle Hit!"
  ) 
  else t.game_state <- Game_over "Out of bounds!"
;;

let new_obstacle t = 
  let boardSquares = Board.all_locations t.board in
  let snakeSquares = Snake.all_locations t.snake in
  let filteredBoardSquares = 
    List.filter boardSquares ~f:(fun square ->
        not (List.mem snakeSquares square ~equal:Position.equal))
  in
  if List.is_empty filteredBoardSquares || List.length filteredBoardSquares < 10 
  then None
  else Some (List.random_element_exn filteredBoardSquares)
;;

let maybe_consume_apple t =
  if Position.equal (Snake.head t.snake) (Apple.location t.apple)
  then (
    t.obstacles <- List.filter_opt [ new_obstacle t; new_obstacle t; new_obstacle t];
    t.consumed <- t.consumed + 1;
    Snake.grow_over_next_steps t.snake (Apple.amount_to_grow t.apple);
    match Apple.create ~board:t.board ~snake:t.snake ~consumed:t.consumed with
    | None -> t.game_state <- Win
    | Some apple -> t.apple <- apple)
;;

let step t =
  if Snake.step t.snake
  then (
    check_for_collisions t;
    maybe_consume_apple t)
  else t.game_state <- Game_over "Self collision!"
;;

module Exercises = struct
  let exercise02b = handle_key

  let exercise03b t snake =
    let t = { t with snake } in
    check_for_collisions t;
    t.game_state
  ;;

  let exercise04b t snake =
    let t = { t with snake } in
    step t;
    t.snake, t.game_state
  ;;

  let exercise06b = maybe_consume_apple
  let set_apple t apple = t.apple <- apple
  let set_snake t snake = t.snake <- snake
end
