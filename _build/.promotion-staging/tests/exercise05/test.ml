open! Base
open! Snake_lib

let height = 10
let width = 10

let test locations =
  Random.init 42;
  let board = Board.create ~height ~width in
  let locations = List.map locations ~f:(fun (row, col) -> Position.{ row; col }) in
  let snake = Snake.Exercises.create_of_locations locations in
  let apple = Apple.Exercises.exercise05 ~board ~snake in
  Stdio.printf !"%{sexp: Position.t option}\n%!" (Option.map ~f:Apple.location apple)
;;

let%expect_test "simple" =
  test [ 0, 1; 0, 2; 0, 3 ];
  [%expect {| (((col 9) (row 2))) |}]
;;

let%expect_test "only one valid location" =
  let locations = List.cartesian_product (List.range 0 height) (List.range 0 width) in
  let snake_locations =
    List.filter locations ~f:(fun square ->
        not ([%compare.equal: int * int] (4, 5) square))
  in
  test snake_locations;
  [%expect {| (((col 5) (row 4))) |}]
;;

let%expect_test "no valid locations" =
  let snake_locations =
    List.cartesian_product (List.range 0 height) (List.range 0 width)
  in
  test snake_locations;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "List.random_element_exn: empty list")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Snake_lib__Apple.create in file "lib/apple.ml", line 61, characters 25-69
  Called from Snake_exercise05__Test.test in file "tests/exercise05/test.ml", line 12, characters 14-54
  Called from Snake_exercise05__Test.(fun) in file "tests/exercise05/test.ml", line 35, characters 2-22
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
;;
