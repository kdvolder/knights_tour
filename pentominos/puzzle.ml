open Knights_tour
type t = {
    (** pieces remaining to be placed *)
    pieces : Polyomino.t list;
    (** board upon which to place the pieces *)
    board : Board.t;
}

let classic = {
  pieces = Polyomino.of_order 5;
  board = Board.classic;
}

let classic_no_symmetric_solutions = {
  classic with pieces = classic.pieces |> Polyomino.pin_symmetry 
}

let lowest_point board = 
  Board.vacant board |> PointSet.min_elt

type placement = {
  piece: Polyomino.t;
  points: PointSet.t;
}

let move_to_target target points = 
  let open Searchspace in
  Searchspace.of_seq (PointSet.to_seq points)
  |-> (fun point -> PointSet.translate (Point.diff target point) points)

let place_one piece board target = 
  let open Searchspace in
  Polyomino.variants piece |> Searchspace.of_list
  |=> move_to_target target
  |?> (fun points ->
    points |> PointSet.for_all (fun point -> Board.(get board point = Vacant))
  )
  |-> (fun points -> {points;piece})

let smallest measure xs =
  let keep_smallest (sx,x) (sy,y) =
    if Int.compare sx sy <= 0 then
      (sx, x)
    else 
      (sy, y) in
  match List.map (fun x -> (measure x, x)) xs with
  | [] -> failwith "An empty list doesn't have a smallest element"
  | fst::rst -> List.fold_left keep_smallest fst rst |> snd

let%expect_test "smallest string" =
  let cases = [
    ["Hello"; "World"; "Very long"; "a"];
    ["The"; "Quick"; "Brown fox"; "jumps"];
    ["Short"; "in"; "the"; "middle"];
    ["all"; "the"; "sam"]
  ] in
  cases |> List.iter (fun words -> 
    words |> List.iter (Printf.printf "%s; ");
    smallest String.length words |> Printf.printf " => %s\n"
  )
  ;[%expect{|
    Hello; World; Very long; a;  => a
    The; Quick; Brown fox; jumps;  => The
    Short; in; the; middle;  => in
    all; the; sam;  => all |}]
  
(** Finds all valid placements for the most constrained target.*)
let most_constrained_point {pieces; board} targets =
  let open Searchspace in
  let placements_per_target = targets 
    |> PointSet.to_seq 
    |> Seq.map (fun target -> 
      pieces |> Searchspace.of_list 
      |=> (fun piece -> place_one piece board target) 
      |> Searchspace.to_seq
      |> List.of_seq
    ) 
    |> List.of_seq
  in 
    smallest List.length placements_per_target
    |> of_list

let%expect_test "most_constrained_point" =
  let board = Board.of_string "
    ########
    ########
    ########
    ###..###
    ###..###
    ########
    #######.
    ########
  " in
  let puzzle = {board; pieces=Polyomino.of_order 5} in
  let vacancies = Board.vacant board in
  let most_constrained = most_constrained_point puzzle vacancies in
  most_constrained |> Searchspace.to_seq
  |> Seq.iter (fun {piece;points} ->
    Board.put puzzle.board points piece 
    |> Board.to_string
    |> Printf.printf "%s\n"
  )
  ;[%expect{|
    ........
    ........
    ........
    ...##...
    ...##...
    ........
    .......#
    ...AAAAA

    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ....B..#
    ....BBBB

    ........
    ........
    ........
    ...##...
    ...##.B.
    ......B.
    ......B#
    ......BB

    ........
    ........
    ........
    ...##...
    ...##...
    ........
    .....C.#
    ....CCCC

    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ......C#
    ....CCCC

    ........
    ........
    ........
    ...##...
    ...##...
    ........
    .....DD#
    .....DDD

    ........
    ........
    ........
    ...##...
    ...##...
    ......EE
    ......E#
    ......EE

    ........
    ........
    ........
    ...##...
    ...##...
    .....F..
    .....F.#
    .....FFF

    ........
    ........
    ........
    ...##...
    ...##...
    ......G.
    ......G#
    .....GGG

    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ....HHH#
    ......HH

    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ....HH.#
    .....HHH

    ........
    ........
    ........
    ...##...
    ...##...
    ......I.
    .....II#
    ......II

    ........
    ........
    ........
    ...##...
    ...##...
    .....J..
    .....JJ#
    ......JJ

    ........
    ........
    ........
    ...##...
    ...##...
    .....KK.
    ......K#
    ......KK |}]

let%expect_test "most constrained hexo point" =
    let board = Board.of_string "
      .........#.........  
      1224567890987654321
      2##################
      3##################
      4##################
      5##################
      6##################
      7##################
      8##################
      9##################
      0##################
      1##################
    " in
  let puzzle = {board; pieces=Polyomino.of_order 6} in
  let vacancies = Board.vacant board in
  let most_constrained = most_constrained_point puzzle vacancies in
  most_constrained |> Searchspace.to_seq
  |> Seq.take 4
  |> Seq.iter (fun {piece;points} ->
    Board.put puzzle.board points piece 
    |> Board.to_string
    |> Printf.printf "%s\n"
  )
  ;[%expect{|
    #########A#########
    .........A.........
    .........A.........
    .........A.........
    .........A.........
    .........A.........
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................

    #########B#########
    .........BBBBB.....
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................

    #########B#########
    .........B.........
    .........B.........
    .........B.........
    .........BB........
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................

    #########B#########
    .........B.........
    .........B.........
    .........B.........
    ........BB.........
    ...................
    ...................
    ...................
    ...................
    ...................
    ...................
    ................... |}]
 
  
let rec solve_aux ~report_progress puzzle targets = 
  let open Searchspace in
  if PointSet.is_empty targets then (
    report_progress "Solved" puzzle;
    return puzzle.board
  ) else 
    most_constrained_point puzzle targets 
    |=> (fun {piece; points} ->
      let puzzle = {
        board=Board.put puzzle.board points piece;
        pieces=List.filter (fun p -> Polyomino.compare p piece <> 0) puzzle.pieces;
      } in
        report_progress "Placed a piece" puzzle;
        let targets = PointSet.union targets (PointSet.adjacent points)
          |> PointSet.filter (fun pt -> (Board.get puzzle.board pt) = Vacant) in
        solve_aux ~report_progress puzzle targets
    )

let solve ?(report_progress = fun _ _ -> ()) puzzle = 
  solve_aux ~report_progress puzzle (PointSet.singleton (lowest_point puzzle.board))
  
let%expect_test "Placements of a polyomino on a target" =
  let puzzle = classic in
  let pentos = puzzle.pieces in
  let piece = List.hd pentos in
  let board = puzzle.board in

  let target = Point.{x=1;y=2} in

  Printf.printf "===============\nPlacing @ (%d, %d):\n%s" target.x target.y (Polyomino.to_string piece);
  place_one piece board target
  |> Searchspace.to_seq
  |> Seq.iter (fun placement -> 
    Board.put board placement.points placement.piece 
    |> Board.to_string
    |> Printf.printf "\n%s"
  )
  ; [%expect{|
    ===============
    Placing @ (1, 2):
    #####

    ........
    ........
    .AAAAA..
    ...##...
    ...##...
    ........
    ........
    ........

    ........
    ........
    AAAAA...
    ...##...
    ...##...
    ........
    ........
    ........

    ........
    ........
    .A......
    .A.##...
    .A.##...
    .A......
    .A......
    ........

    ........
    .A......
    .A......
    .A.##...
    .A.##...
    .A......
    ........
    ........

    .A......
    .A......
    .A......
    .A.##...
    .A.##...
    ........
    ........
    ........ |}]

let%expect_test "Placements of all tetro-minos @(0,0)" =
    let pentos = Polyomino.of_order 4 in
    let board = Board.classic in
    let target = Point.{x=0;y=0} in

    pentos |> List.iter (fun piece -> 
      Printf.printf "===============\nPlacing @ (%d, %d):\n%s" target.x target.y (Polyomino.to_string piece);
      place_one piece board target
      |> Searchspace.to_seq
      |> Seq.iter (fun placement -> 
        Board.put board placement.points placement.piece 
        |> Board.to_string
        |> Printf.printf "\n%s"
      )
    )
    ; [%expect{|
      ===============
      Placing @ (0, 0):
      ####

      AAAA....
      ........
      ........
      ...##...
      ...##...
      ........
      ........
      ........

      A.......
      A.......
      A.......
      A..##...
      ...##...
      ........
      ........
      ........
      ===============
      Placing @ (0, 0):
      ###
      #..

      BBB.....
      B.......
      ........
      ...##...
      ...##...
      ........
      ........
      ........

      BBB.....
      ..B.....
      ........
      ...##...
      ...##...
      ........
      ........
      ........

      BB......
      B.......
      B.......
      ...##...
      ...##...
      ........
      ........
      ........

      BB......
      .B......
      .B......
      ...##...
      ...##...
      ........
      ........
      ........

      B.......
      BBB.....
      ........
      ...##...
      ...##...
      ........
      ........
      ........

      B.......
      B.......
      BB......
      ...##...
      ...##...
      ........
      ........
      ........
      ===============
      Placing @ (0, 0):
      ###
      .#.

      CCC.....
      .C......
      ........
      ...##...
      ...##...
      ........
      ........
      ........

      C.......
      CC......
      C.......
      ...##...
      ...##...
      ........
      ........
      ........
      ===============
      Placing @ (0, 0):
      ##
      ##

      DD......
      DD......
      ........
      ...##...
      ...##...
      ........
      ........
      ........
      ===============
      Placing @ (0, 0):
      ##.
      .##

      EE......
      .EE.....
      ........
      ...##...
      ...##...
      ........
      ........
      ........

      E.......
      EE......
      .E......
      ...##...
      ...##...
      ........
      ........
      ........ |}]

  let%expect_test "solve classic pentos" =
    let puzzle = classic in
    solve puzzle |> Searchspace.to_seq
    |> Seq.take 4
    |> Seq.map Board.to_string
    |> Seq.iter print_endline
    ;[%expect{|
      AAAAAFFF
      BBBBHHIF
      BLHHHIIF
      LLL##KII
      CLG##KKK
      CCGGGJJK
      CEGEDDJJ
      CEEEDDDJ

      AAAAAFFF
      BBBBHHIF
      BLHHHIIF
      LLL##KII
      CLG##KKK
      CCGGGJJK
      CEGEJJDD
      CEEEJDDD

      AAAAAJJD
      BBBBJJDD
      EEEBJGDD
      EHE##GGG
      HHL##GKK
      HLLLIIKF
      HCLIIKKF
      CCCCIFFF

      AAAAADDD
      BBCCCCDD
      BGGGCEEE
      BHG##ELE
      BHG##LLL
      HHJKKILF
      HJJKIIIF
      JJKKIFFF |}]

let save_fmt out {pieces;board} = (
  pieces |> Polyomino.save_fmt out;
  Format.fprintf out "%s\n" (Board.to_string board)
)

let save out_channel puzzle = 
  save_fmt (Format.formatter_of_out_channel out_channel) puzzle 
      
let%expect_test "save puzzle" =
  let puzzle = classic in
  puzzle |> save_fmt (Format.std_formatter);
  [%expect{|
    A:
    #####

    #
    #
    #
    #
    #


    B:
    ####
    #...

    ####
    ...#

    ##
    #.
    #.
    #.

    ##
    .#
    .#
    .#

    #...
    ####

    #.
    #.
    #.
    ##

    .#
    .#
    .#
    ##

    ...#
    ####


    C:
    ####
    .#..

    ####
    ..#.

    #.
    ##
    #.
    #.

    #.
    #.
    ##
    #.

    .#..
    ####

    .#
    ##
    .#
    .#

    .#
    .#
    ##
    .#

    ..#.
    ####


    D:
    ###
    ##.

    ###
    .##

    ##.
    ###

    ##
    ##
    #.

    ##
    ##
    .#

    #.
    ##
    ##

    .##
    ###

    .#
    ##
    ##


    E:
    ###
    #.#

    ##
    #.
    ##

    ##
    .#
    ##

    #.#
    ###


    F:
    ###
    #..
    #..

    ###
    ..#
    ..#

    #..
    #..
    ###

    ..#
    ..#
    ###


    G:
    ###
    .#.
    .#.

    #..
    ###
    #..

    .#.
    .#.
    ###

    ..#
    ###
    ..#


    H:
    ###.
    ..##

    ##..
    .###

    #.
    ##
    .#
    .#

    #.
    #.
    ##
    .#

    .###
    ##..

    .#
    ##
    #.
    #.

    .#
    .#
    ##
    #.

    ..##
    ###.


    I:
    ##.
    .##
    .#.

    #..
    ###
    .#.

    .##
    ##.
    .#.

    .#.
    ###
    #..

    .#.
    ###
    ..#

    .#.
    ##.
    .##

    .#.
    .##
    ##.

    ..#
    ###
    .#.


    J:
    ##.
    .##
    ..#

    #..
    ##.
    .##

    .##
    ##.
    #..

    ..#
    .##
    ##.


    K:
    ##.
    .#.
    .##

    #..
    ###
    ..#

    .##
    .#.
    ##.

    ..#
    ###
    #..


    L:
    .#.
    ###
    .#.


    ---
    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ........
    ........ |}]

let load_lines input =
  let pieces = Polyomino.load_lines input in 
  let square_of_char = function 
  | '.' -> Board.Vacant
  | '#' -> Board.Blocked
  | ch -> Board.Occupied (List.find (fun piece -> Polyomino.name piece = ch) pieces) 
  in
  let lines = Lines.load (Lines.load_list "" Lines.load_line) input in
  let image = List.fold_right (fun l r -> l ^ "\n" ^r) lines "" in
  let board = Board.load_string square_of_char image in {
    board;pieces
  }

let load input = load_lines (Lines.of_channel input)

let%expect_test "load from save" =
  let puzzle = classic in
  let tmp_file = Filename.temp_file "test" ".pento-puzzle" in
  Out_channel.with_open_text tmp_file (fun out ->
    save out puzzle;
  );
  let puzzle = In_channel.with_open_text tmp_file load in
  save_fmt Format.std_formatter puzzle;
  [%expect{|
    A:
    #####

    #
    #
    #
    #
    #


    B:
    ####
    #...

    ####
    ...#

    ##
    #.
    #.
    #.

    ##
    .#
    .#
    .#

    #...
    ####

    #.
    #.
    #.
    ##

    .#
    .#
    .#
    ##

    ...#
    ####


    C:
    ####
    .#..

    ####
    ..#.

    #.
    ##
    #.
    #.

    #.
    #.
    ##
    #.

    .#..
    ####

    .#
    ##
    .#
    .#

    .#
    .#
    ##
    .#

    ..#.
    ####


    D:
    ###
    ##.

    ###
    .##

    ##.
    ###

    ##
    ##
    #.

    ##
    ##
    .#

    #.
    ##
    ##

    .##
    ###

    .#
    ##
    ##


    E:
    ###
    #.#

    ##
    #.
    ##

    ##
    .#
    ##

    #.#
    ###


    F:
    ###
    #..
    #..

    ###
    ..#
    ..#

    #..
    #..
    ###

    ..#
    ..#
    ###


    G:
    ###
    .#.
    .#.

    #..
    ###
    #..

    .#.
    .#.
    ###

    ..#
    ###
    ..#


    H:
    ###.
    ..##

    ##..
    .###

    #.
    ##
    .#
    .#

    #.
    #.
    ##
    .#

    .###
    ##..

    .#
    ##
    #.
    #.

    .#
    .#
    ##
    #.

    ..##
    ###.


    I:
    ##.
    .##
    .#.

    #..
    ###
    .#.

    .##
    ##.
    .#.

    .#.
    ###
    #..

    .#.
    ###
    ..#

    .#.
    ##.
    .##

    .#.
    .##
    ##.

    ..#
    ###
    .#.


    J:
    ##.
    .##
    ..#

    #..
    ##.
    .##

    .##
    ##.
    #..

    ..#
    .##
    ##.


    K:
    ##.
    .#.
    .##

    #..
    ###
    ..#

    .##
    .#.
    ##.

    ..#
    ###
    #..


    L:
    .#.
    ###
    .#.


    ---
    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ........
    ........ |}]

let to_string p =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  save_fmt fmt p;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let%expect_test "print classic pentomino" =
  let puz = classic_no_symmetric_solutions in
  Printf.printf "%s\n" (to_string puz);
  [%expect{|
    A:
    #####

    #
    #
    #
    #
    #


    B:
    ####
    #...


    C:
    ####
    .#..

    ####
    ..#.

    #.
    ##
    #.
    #.

    #.
    #.
    ##
    #.

    .#..
    ####

    .#
    ##
    .#
    .#

    .#
    .#
    ##
    .#

    ..#.
    ####


    D:
    ###
    ##.

    ###
    .##

    ##.
    ###

    ##
    ##
    #.

    ##
    ##
    .#

    #.
    ##
    ##

    .##
    ###

    .#
    ##
    ##


    E:
    ###
    #.#

    ##
    #.
    ##

    ##
    .#
    ##

    #.#
    ###


    F:
    ###
    #..
    #..

    ###
    ..#
    ..#

    #..
    #..
    ###

    ..#
    ..#
    ###


    G:
    ###
    .#.
    .#.

    #..
    ###
    #..

    .#.
    .#.
    ###

    ..#
    ###
    ..#


    H:
    ###.
    ..##

    ##..
    .###

    #.
    ##
    .#
    .#

    #.
    #.
    ##
    .#

    .###
    ##..

    .#
    ##
    #.
    #.

    .#
    .#
    ##
    #.

    ..##
    ###.


    I:
    ##.
    .##
    .#.

    #..
    ###
    .#.

    .##
    ##.
    .#.

    .#.
    ###
    #..

    .#.
    ###
    ..#

    .#.
    ##.
    .##

    .#.
    .##
    ##.

    ..#
    ###
    .#.


    J:
    ##.
    .##
    ..#

    #..
    ##.
    .##

    .##
    ##.
    #..

    ..#
    .##
    ##.


    K:
    ##.
    .#.
    .##

    #..
    ###
    ..#

    .##
    .#.
    ##.

    ..#
    ###
    #..


    L:
    .#.
    ###
    .#.


    ---
    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ........
    ........
    |}]

(* let hash puzzle =
  let puzzle_string = to_string puzzle in
  Digest.string puzzle_string |> Digest.to_hex    

let calculate_true_values = Cache.memoize
  ~cache_dir:"/tmp/no-sandbox-cache" 
  ~function_name:"caluclate_true_values"
  ~hash:hash
  (fun puzzle -> solve puzzle |> Stochastic_estimator.calculate_true_values) *)

let%expect_test "estimate classic pentomino" =
  let puz = classic_no_symmetric_solutions in
  let searchspace = solve puz in
  Printf.printf "%s\n" (Board.to_string (puz.board));
  (* commented out for faster test runs
  let true_values = calculate_true_values puz in
  Printf.printf "True values:\n";
  Printf.printf "   Nodes: %d\n" true_values.nodes;
  Printf.printf "   Solutions: %d\n" true_values.solutions;
  Printf.printf "   Fails: %d\n\n" true_values.fails; *)
  let est = Stochastic_estimator.create ~selector:Stochastic_estimator.undersampled_selector searchspace in
  let samples_per_iter = 2000 in
  let prev_nodes = ref 0.0 in
  let total_samples = ref 0 in
  let percent_change = ref 100.0 in
  let small_percent_change = 5.0 in
  while !percent_change > small_percent_change do
    Stochastic_estimator.sample samples_per_iter est;
    total_samples := !total_samples + samples_per_iter;
    let estimates = Stochastic_estimator.estimates est in
    percent_change :=
      if !prev_nodes = 0.0 then 100.0
      else abs_float (estimates.nodes -. !prev_nodes) /. !prev_nodes *. 100.0;
    Printf.printf "Estimates with %d samples:\n" !total_samples;
    Printf.printf "   Nodes: %d\n" (Int.of_float (estimates.nodes+.0.5));
    Printf.printf "   Solutions: %.2f\n" estimates.solutions;
    Printf.printf "   Fails: %d\n" (Int.of_float (estimates.fails+.0.5));
    Printf.printf "   Change in nodes: %.2f%%\n" !percent_change;
    Printf.printf "\n";
    prev_nodes := estimates.nodes;
  done;
  [%expect{|
    ........
    ........
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    Estimates with 2000 samples:
       Nodes: 55683
       Solutions: 0.00
       Fails: 43497
       Change in nodes: 100.00%

    Estimates with 4000 samples:
       Nodes: 63656
       Solutions: 0.00
       Fails: 49651
       Change in nodes: 14.32%

    Estimates with 6000 samples:
       Nodes: 86576
       Solutions: 0.00
       Fails: 66472
       Change in nodes: 36.01%

    Estimates with 8000 samples:
       Nodes: 76213
       Solutions: 0.00
       Fails: 59747
       Change in nodes: 11.97%

    Estimates with 10000 samples:
       Nodes: 80750
       Solutions: 0.00
       Fails: 63415
       Change in nodes: 5.95%

    Estimates with 12000 samples:
       Nodes: 85733
       Solutions: 0.00
       Fails: 67383
       Change in nodes: 6.17%

    Estimates with 14000 samples:
       Nodes: 89976
       Solutions: 0.00
       Fails: 70752
       Change in nodes: 4.95%
    |}]