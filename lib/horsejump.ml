type point = {x: int; y:int}

type move = {
  from: point;
  dest: point;
}

let ( ** ) (xs:'a list) (ys:'b list) =
  xs |> List.map (fun x ->
    ys |> List.map (fun y ->
      {x;y}
    )
  )
  |> List.flatten

let move_deltas = 
  let deltas = [-1; 1; -2; 2] in
    deltas ** deltas 
  |> List.filter (fun {x;y} -> Int.abs x <> Int.abs y)

let (+.) p d = { x=p.x + d.x; y=p.y + d.y}

let moveIsValid {from; dest} =
  let dx = Int.abs (from.x - dest.x) in
  let dy = Int.abs (from.y - dest.y) in
  dx + dy = 3 && dx < 3 && dy < 3

let ints_upto upper_limit = List.init upper_limit Fun.id

let draw_size = 32

let rec repeat n s = 
  if n>0 then s ^ repeat (n-1) s else ""  

let colCode x = String.make 1 (Char.chr ((Char.code 'A') + x))
let rowCode x = Int.to_string x

let list_to_string to_string sep = function
  | [] -> ""
  | x::xs -> List.fold_left (fun s el -> s ^ sep ^ (to_string el)) (to_string x) xs  

let move_to_string {from=_; dest} =  (colCode dest.x) ^ (rowCode dest.y)
let moves_to_string = list_to_string move_to_string ", "

let grid_to_string w h (cell_to_string : int -> int -> string) =
  let row_seperator = "  " ^(repeat w "+---") ^ "+\n" in
  let board_header = ints_upto w
    |> List.map (fun col -> " " ^ colCode col ^ "  ")
    |> List.fold_left (^) "   "
    |> (fun h -> h ^ "\n")
  in
    ints_upto h |> List.map (fun y ->
      ints_upto w |> List.map (fun x -> cell_to_string x y) 
      |> List.fold_left (fun row cell -> row ^ "|" ^ cell) ((rowCode y) ^ " ")
      |> (fun row -> row ^ "|\n")
    )
    |> List.fold_left (fun board row -> board ^ row_seperator ^ row) ""
    |> (fun board -> board ^ row_seperator)
    |> (fun board -> board_header ^ board)
  
(* ************************* *)
(* * board                 * *)
(* ************************* *)
module Board : sig 
  type t

  val size : t -> int
  val make : int -> t
  val to_string : t -> string
  val inRange : t -> point -> bool
  val get : t -> point -> int option
  val set : t -> point -> int option -> unit
  val draw : t -> unit
  val isValid : t -> bool
  val isValidTarget : t -> point -> bool
  val count_targets : t -> point -> int

end = struct

  type cell = int option

  type t = cell array array

  let size =Array.length

  let get board {x;y} = board.(x).(y)

  let inRange board {x;y} = 
    let sz = size board in x>=0 && y>=0 && x<sz && y<sz

  let isValidTarget board pt = 
    (inRange board pt) && Option.is_none (get board pt)

  let count_targets board pt = 
    move_deltas |> List.to_seq
    |> Seq.map ((+.) pt)
    |> Seq.filter (isValidTarget board)
    |> Seq.length

  let make size = Array.make_matrix size size None

  let set board {x;y} v =  
    board.(x).(y) <- v
  
  let range lo hi = Seq.ints lo |> Seq.take_while (fun x -> x < hi) 

  let isValid (board:t) =
    let board_index = Hashtbl.create (size board |> fun x -> x*x) in
    board |> Array.iteri (fun y row ->
      row |> Array.iteri (fun x cel -> 
        match cel with
        | Some move_number -> Hashtbl.add board_index move_number {x;y}
        | None -> ()
      )
    );
    let moves_are_valid = range 1 (size board * size board) |> Seq.map (fun move_number ->
      match Hashtbl.find_all board_index move_number with 
      | [from] -> ( 
          match Hashtbl.find_opt board_index (move_number+1) with
          | Some dest -> moveIsValid {from;dest}
          | None -> false
      ) 
      | _ -> false (* either the movenumber was not found, or found more than once *)
    ) in
    Seq.for_all (fun x -> x) moves_are_valid

  let cell_to_string = function 
    | None -> " . "
    | Some x -> Printf.sprintf "%2d " x

  let to_string board = 
    let size = size board in
    grid_to_string size size (fun x y ->
      cell_to_string board.(x).(y)
  )

  let draw (board:t) = 
    (* let text_size = 20 in *)
    let adjust = 5 in
    Graphics.set_font "12x24";
    Graphics.clear_graph ();
    board |> Array.iteri (fun r row ->
      row |> Array.iteri (fun k cell ->
        let x = k*draw_size in
        let y = ((size board) - r - 1)*draw_size in
        Graphics.draw_rect x y draw_size draw_size;
        Graphics.moveto (x+adjust) (y+adjust);
        Graphics.draw_string (cell_to_string cell)
      )
    )

end

(* ************************* *)
(* * GameState             * *)
(* ************************* *)
module GameState : sig 
  type t
  val make : int -> t
  val valid_moves : t -> move list
  val steps : t -> int
  val board : t -> Board.t
  val do_move : t -> move -> unit
  val undo_move : t -> move -> unit
  val isWinning : t -> bool
end = struct

  type t = {
    mutable horse: point;
    mutable step: int;
    board: Board.t
  }

  let steps {step;_} = step
  let isWinning {step;board;_} = 
    let sz = Board.size board in
    Int.equal step (sz * sz)

  (** make a new gamestate, which starts at the 'start state' of the game*)
  let make board_size = 
    let step = 1 in
    let horse = {x = 0;y = 0} in
    let board = Board.make board_size in
      Board.set board horse (Option.some step);
      {horse; step; board}
  let do_move state move = 
    state.step <- state.step + 1;
    state.horse <- move.dest;
    Board.set state.board move.dest (Some state.step)

  let undo_move state move = 
    state.step <- state.step - 1;
    state.horse <- move.from;
    Board.set state.board move.dest None
  
  let board {board;_} = board

  let isValidTarget board target =
    Board.inRange board target && Option.is_none (Board.get board target)
  
  let valid_moves {horse; board; step=_} =
    move_deltas 
    |> List.map (fun d -> horse +. d)
    |> List.filter (isValidTarget board) 
    |> List.map (fun target -> {from=horse;dest=target})

end

let solve ?(report_backtracking = fun _ -> ()) = (
  let rec solve state =
    if GameState.isWinning state then
      Some (GameState.board state)
    else
      match GameState.valid_moves state with
      | [] ->
          report_backtracking state;
          None
      | moves ->
          List.to_seq moves
          |> Seq.find_map (fun move -> try_move state move)       
    and try_move state move = 
      GameState.do_move state move;
      match solve state with
      | None -> GameState.undo_move state move; None
      | Some solution -> Some solution
  in solve
)

let print_targetcount board = 
  let open Board in
  let sz = size board in
  print_endline (grid_to_string sz sz (fun x y ->
    match get board {x;y} with
    | None -> Printf.sprintf "%2d " (count_targets board {x;y})
    | Some _  -> " # "
  ))

let print_solve size = solve (GameState.make size)
  |> Option.get |> Board.to_string |> print_endline

let%test "5x5 board can be solved" = solve (GameState.make 8) 
  |> Option.get |> fun x -> Board.isValid x 

let%expect_test "print_solve 8" = print_solve 8;[%expect{|
      A   B   C   D   E   F   G   H
    +---+---+---+---+---+---+---+---+
  0 | 1 |16 | 7 |34 |55 |32 |47 |36 |
    +---+---+---+---+---+---+---+---+
  1 | 8 |23 |14 |25 |46 |35 |56 |31 |
    +---+---+---+---+---+---+---+---+
  2 |15 | 2 |17 | 6 |33 |54 |37 |48 |
    +---+---+---+---+---+---+---+---+
  3 |22 | 9 |24 |13 |26 |45 |30 |57 |
    +---+---+---+---+---+---+---+---+
  4 | 3 |18 | 5 |44 |53 |40 |49 |38 |
    +---+---+---+---+---+---+---+---+
  5 |10 |21 |12 |27 |62 |29 |58 |41 |
    +---+---+---+---+---+---+---+---+
  6 |19 | 4 |63 |52 |43 |60 |39 |50 |
    +---+---+---+---+---+---+---+---+
  7 |64 |11 |20 |61 |28 |51 |42 |59 |
    +---+---+---+---+---+---+---+---+ |}]
let%expect_test "print_solve 5" = print_solve 5;[%expect{|
      A   B   C   D   E
    +---+---+---+---+---+
  0 | 1 |10 |19 |14 |23 |
    +---+---+---+---+---+
  1 |18 | 5 |22 | 9 |20 |
    +---+---+---+---+---+
  2 |11 | 2 |13 |24 |15 |
    +---+---+---+---+---+
  3 | 6 |17 | 4 |21 | 8 |
    +---+---+---+---+---+
  4 | 3 |12 | 7 |16 |25 |
    +---+---+---+---+---+ |}]

let%expect_test "print targetcount empty board" =
  print_targetcount (Board.make 5); [%expect{|
        A   B   C   D   E
      +---+---+---+---+---+
    0 | 2 | 3 | 4 | 3 | 2 |
      +---+---+---+---+---+
    1 | 3 | 4 | 6 | 4 | 3 |
      +---+---+---+---+---+
    2 | 4 | 6 | 8 | 6 | 4 |
      +---+---+---+---+---+
    3 | 3 | 4 | 6 | 4 | 3 |
      +---+---+---+---+---+
    4 | 2 | 3 | 4 | 3 | 2 |
      +---+---+---+---+---+ |}]