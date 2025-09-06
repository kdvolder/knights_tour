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

let hash puzzle =
  let puzzle_string = to_string puzzle in
  Digest.string puzzle_string |> Digest.to_hex    

let calculate_true_values = Cache.memoize
  ~cache_dir:"/tmp/no-sandbox-cache" 
  ~function_name:"caluclate_true_values"
  ~hash:hash
  (fun puzzle -> solve puzzle |> Stochastic_estimator.calculate_true_values)

let%expect_test "estimate classic pentomino" =
  let puz = classic_no_symmetric_solutions in
  let searchspace = solve puz in
  Printf.printf "%s\n" (Board.to_string (puz.board));
  let true_values = calculate_true_values puz in
  Printf.printf "True values:\n";
  Printf.printf "   Nodes: %d\n" true_values.nodes;
  Printf.printf "   Solutions: %d\n" true_values.solutions;
  Printf.printf "   Fails: %d\n\n" true_values.fails;
  let est = Stochastic_estimator.create searchspace in
  let samples_per_iter = 2000 in
  let prev_nodes = ref 0.0 in
  let total_samples = ref 0 in
  let percent_change = ref 100.0 in
  let small_percent_change = 1.0 in
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

    True values:
       Nodes: 306487
       Solutions: 65
       Fails: 234622

    Estimates with 2000 samples:
       Nodes: 59464
       Solutions: 0.00
       Fails: 46977
       Change in nodes: 100.00%

    Estimates with 4000 samples:
       Nodes: 60068
       Solutions: 0.00
       Fails: 47677
       Change in nodes: 1.02%

    Estimates with 6000 samples:
       Nodes: 67742
       Solutions: 0.00
       Fails: 53749
       Change in nodes: 12.78%

    Estimates with 8000 samples:
       Nodes: 72903
       Solutions: 0.00
       Fails: 57868
       Change in nodes: 7.62%

    Estimates with 10000 samples:
       Nodes: 79828
       Solutions: 0.00
       Fails: 63308
       Change in nodes: 9.50%

    Estimates with 12000 samples:
       Nodes: 85637
       Solutions: 24.20
       Fails: 67898
       Change in nodes: 7.28%

    Estimates with 14000 samples:
       Nodes: 91280
       Solutions: 20.17
       Fails: 72387
       Change in nodes: 6.59%

    Estimates with 16000 samples:
       Nodes: 95441
       Solutions: 20.17
       Fails: 75632
       Change in nodes: 4.56%

    Estimates with 18000 samples:
       Nodes: 99946
       Solutions: 17.29
       Fails: 79121
       Change in nodes: 4.72%

    Estimates with 20000 samples:
       Nodes: 104045
       Solutions: 17.29
       Fails: 82380
       Change in nodes: 4.10%

    Estimates with 22000 samples:
       Nodes: 108135
       Solutions: 17.29
       Fails: 85618
       Change in nodes: 3.93%

    Estimates with 24000 samples:
       Nodes: 111908
       Solutions: 17.29
       Fails: 88585
       Change in nodes: 3.49%

    Estimates with 26000 samples:
       Nodes: 115898
       Solutions: 17.29
       Fails: 91744
       Change in nodes: 3.57%

    Estimates with 28000 samples:
       Nodes: 119470
       Solutions: 17.29
       Fails: 94500
       Change in nodes: 3.08%

    Estimates with 30000 samples:
       Nodes: 122925
       Solutions: 15.10
       Fails: 97181
       Change in nodes: 2.89%

    Estimates with 32000 samples:
       Nodes: 127535
       Solutions: 15.10
       Fails: 100688
       Change in nodes: 3.75%

    Estimates with 34000 samples:
       Nodes: 131161
       Solutions: 15.10
       Fails: 103563
       Change in nodes: 2.84%

    Estimates with 36000 samples:
       Nodes: 135525
       Solutions: 15.10
       Fails: 106962
       Change in nodes: 3.33%

    Estimates with 38000 samples:
       Nodes: 139132
       Solutions: 14.00
       Fails: 109696
       Change in nodes: 2.66%

    Estimates with 40000 samples:
       Nodes: 142563
       Solutions: 18.00
       Fails: 112331
       Change in nodes: 2.47%

    Estimates with 42000 samples:
       Nodes: 145584
       Solutions: 18.00
       Fails: 114670
       Change in nodes: 2.12%

    Estimates with 44000 samples:
       Nodes: 149050
       Solutions: 18.00
       Fails: 117346
       Change in nodes: 2.38%

    Estimates with 46000 samples:
       Nodes: 152788
       Solutions: 18.00
       Fails: 120231
       Change in nodes: 2.51%

    Estimates with 48000 samples:
       Nodes: 155610
       Solutions: 25.85
       Fails: 122421
       Change in nodes: 1.85%

    Estimates with 50000 samples:
       Nodes: 158487
       Solutions: 25.85
       Fails: 124636
       Change in nodes: 1.85%

    Estimates with 52000 samples:
       Nodes: 160944
       Solutions: 25.85
       Fails: 126550
       Change in nodes: 1.55%

    Estimates with 54000 samples:
       Nodes: 163702
       Solutions: 33.85
       Fails: 128656
       Change in nodes: 1.71%

    Estimates with 56000 samples:
       Nodes: 166126
       Solutions: 31.85
       Fails: 130560
       Change in nodes: 1.48%

    Estimates with 58000 samples:
       Nodes: 168962
       Solutions: 26.35
       Fails: 132725
       Change in nodes: 1.71%

    Estimates with 60000 samples:
       Nodes: 172266
       Solutions: 25.42
       Fails: 135269
       Change in nodes: 1.96%

    Estimates with 62000 samples:
       Nodes: 174915
       Solutions: 23.58
       Fails: 137320
       Change in nodes: 1.54%

    Estimates with 64000 samples:
       Nodes: 177546
       Solutions: 23.58
       Fails: 139306
       Change in nodes: 1.50%

    Estimates with 66000 samples:
       Nodes: 180369
       Solutions: 23.58
       Fails: 141462
       Change in nodes: 1.59%

    Estimates with 68000 samples:
       Nodes: 183257
       Solutions: 23.13
       Fails: 143654
       Change in nodes: 1.60%

    Estimates with 70000 samples:
       Nodes: 186015
       Solutions: 23.13
       Fails: 145738
       Change in nodes: 1.51%

    Estimates with 72000 samples:
       Nodes: 188583
       Solutions: 30.63
       Fails: 147692
       Change in nodes: 1.38%

    Estimates with 74000 samples:
       Nodes: 190887
       Solutions: 27.63
       Fails: 149451
       Change in nodes: 1.22%

    Estimates with 76000 samples:
       Nodes: 193234
       Solutions: 30.63
       Fails: 151228
       Change in nodes: 1.23%

    Estimates with 78000 samples:
       Nodes: 195607
       Solutions: 29.13
       Fails: 153028
       Change in nodes: 1.23%

    Estimates with 80000 samples:
       Nodes: 198057
       Solutions: 29.13
       Fails: 154935
       Change in nodes: 1.25%

    Estimates with 82000 samples:
       Nodes: 200690
       Solutions: 29.13
       Fails: 156902
       Change in nodes: 1.33%

    Estimates with 84000 samples:
       Nodes: 203538
       Solutions: 29.13
       Fails: 159059
       Change in nodes: 1.42%

    Estimates with 86000 samples:
       Nodes: 205619
       Solutions: 28.83
       Fails: 160629
       Change in nodes: 1.02%

    Estimates with 88000 samples:
       Nodes: 207904
       Solutions: 31.83
       Fails: 162357
       Change in nodes: 1.11%

    Estimates with 90000 samples:
       Nodes: 210110
       Solutions: 28.67
       Fails: 164023
       Change in nodes: 1.06%

    Estimates with 92000 samples:
       Nodes: 212877
       Solutions: 27.67
       Fails: 166102
       Change in nodes: 1.32%

    Estimates with 94000 samples:
       Nodes: 215140
       Solutions: 30.75
       Fails: 167828
       Change in nodes: 1.06%

    Estimates with 96000 samples:
       Nodes: 217151
       Solutions: 29.42
       Fails: 169350
       Change in nodes: 0.93%
    |}]