open Knights_tour

module PointMap = Map.Make(Point)

type square =
  | Occupied of Polyomino.t 
  | Vacant
  | Blocked

type t = {
  size: Point.t;
  squares: Polyomino.t option PointMap.t; (* map of squares on the board excluding blocked squares *)
}

let size {size;_} = size

let get {squares;_} pt =
  match PointMap.find_opt pt squares with 
  | None -> Blocked
  | Some (Some p) -> Occupied p
  | Some None -> Vacant

let vacant {squares;_} =
  let vacancies = ref PointSet.empty in
  squares |> PointMap.iter (fun pt square ->
    if Option.is_none square then
      vacancies := PointSet.add pt !vacancies
  );
  !vacancies

let load_string square_of_char img = 
  let squares = ref PointMap.empty in
  Lines.of_string img
  |> Seq.iteri (fun y line ->
    line |> String.iteri (fun x ch -> 
      match square_of_char ch with
      | Blocked -> ()
      | Vacant -> squares := PointMap.add Point.{x;y} None !squares
      | Occupied p -> squares := PointMap.add Point.{x;y} (Some p) !squares
    )
  );
  let points = ref PointSet.empty in
  !squares |> PointMap.iter (fun pt _ -> points:= PointSet.add pt !points); 
  {
      size = Point.{x = 1 + PointSet.max_x !points; y = 1 + PointSet.max_y !points};
      squares = !squares
  }
  
let of_string img =
  let vacancies = PointSet.of_string img |> PointSet.normalize_translation in
  {
    size = Point.{
      x = PointSet.max_x vacancies + 1;
      y = PointSet.max_y vacancies + 1;
    };
    squares = PointSet.fold (fun vacant board -> PointMap.add vacant None board) vacancies PointMap.empty
  }

let rectangle w h =
  let open Searchspace in
    int_range 0 (w - 1) |=> (fun x ->
      int_range 0 (h - 1) |-> (fun y -> (Point.{x;y}, None))
    )
    |> to_seq
    |> PointMap.of_seq
    |> fun squares -> {
      size = Point.{x=w;y=h};
      squares
    }

let classic = of_string "
  ########
  ########
  ########
  ###..###
  ###..###
  ########
  ########
  ########"

let%expect_test "Board of_string |> vacant" =
  let open Printf in
  let board = classic in
  printf "min x: %d y: %d\n" (PointSet.min_x (vacant board)) (PointSet.min_y (vacant board));
  printf "max x: %d y: %d\n" (PointSet.max_x (vacant board)) (PointSet.max_y (vacant board));
  printf "size = (%d, %d)\n" (size board).x (size board).y;
  printf "\n";
  printf "%s" (PointSet.to_string (vacant board));
  [%expect{|
    min x: 0 y: 0
    max x: 7 y: 7
    size = (8, 8)

    ########
    ########
    ########
    ###..###
    ###..###
    ########
    ########
    ######## |}]

let put board points poly =
  PointSet.fold (fun pt board ->
    match PointMap.find pt board with 
    | None -> PointMap.add pt (Some poly) board
    | _ -> failwith "Invalid placement. Polyomino should only be placed on vacant squares"
  ) points board.squares
  |> (fun squares -> {board with squares})  

let square_to_char = function
  | Vacant -> '.'
  | Occupied p -> Polyomino.name p
  | Blocked -> '#'

let to_string board =
  let size = board.size in
  let image = Buffer.create ((size.x+1)*(size.y+1)) in
  for y = 0 to size.y-1 do
    for x = 0 to size.x-1 do
      Buffer.add_char image (square_to_char (get board {x;y}))
    done;
    Buffer.add_char image '\n'
  done;
  Buffer.contents image

let%expect_test "Place a polyomino" =
  let polyos = Polyomino.of_order 5 in
  let board = classic in
  polyos |> List.iteri (fun i poly ->
    Printf.printf "%d:\n" (i+1);
    let board = put board (Polyomino.points poly) poly in
    Printf.printf "%s\n" (to_string board)
  )
  ; [%expect{|
    1:
    AAAAA...
    ........
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    2:
    BBBB....
    B.......
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    3:
    CCCC....
    .C......
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    4:
    DDD.....
    DD......
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    5:
    EEE.....
    E.E.....
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    6:
    FFF.....
    F.......
    F.......
    ...##...
    ...##...
    ........
    ........
    ........

    7:
    GGG.....
    .G......
    .G......
    ...##...
    ...##...
    ........
    ........
    ........

    8:
    HHH.....
    ..HH....
    ........
    ...##...
    ...##...
    ........
    ........
    ........

    9:
    II......
    .II.....
    .I......
    ...##...
    ...##...
    ........
    ........
    ........

    10:
    JJ......
    .JJ.....
    ..J.....
    ...##...
    ...##...
    ........
    ........
    ........

    11:
    KK......
    .K......
    .KK.....
    ...##...
    ...##...
    ........
    ........
    ........

    12:
    .L......
    LLL.....
    .L......
    ...##...
    ...##...
    ........
    ........
    ........ |}]

let draw_size = 64
let margin = 2

let color_table : Graphics.color array = 
  let color_levels = [230; 130; 30] in
  let ct = Array.make (List.length color_levels |> (fun x-> x*x*x)) Graphics.black in
  let idx = ref 0 in
  color_levels |> List.iter (fun r -> 
    color_levels |> List.iter (fun g -> 
      color_levels |> List.iter (fun b -> 
        ct.(!idx) <- Graphics.rgb r g b;
        idx := !idx + 1
      )
    )
  );
  ct

let color_code pento_name = 
  (Char.code pento_name - Char.code 'A') mod (Array.length color_table)

let color_of = function 
| Vacant -> Graphics.white
| Blocked -> Graphics.black
| Occupied p -> color_table.(color_code (Polyomino.name p))

let to_screen xy = (xy*draw_size+margin) 

let vert_line x y =
  Graphics.moveto (to_screen x) (to_screen y);
  Graphics.lineto (to_screen x) (to_screen (y+1))

let hori_line x y = 
  Graphics.moveto (to_screen x) (to_screen y);
  Graphics.lineto (to_screen (x+1)) (to_screen y)


let draw_border board = 
  Graphics.set_line_width 1;
  Graphics.set_color (Graphics.black);
  Graphics.draw_rect 
    ((to_screen 0) - 2) ((to_screen 0) - 2)
    (draw_size * board.size.x + 3) (draw_size * board.size.y + 3)
  
let draw ?(black_and_white=false) (board:t) = 
  Graphics.set_font "12x24";
  Graphics.clear_graph ();

  let sz = size board in
  (* filling squares *)
  if not black_and_white then begin
  for y = 0 to sz.y-1 do
    for x = 0 to sz.x-1 do
      Graphics.set_color (get board {x;y} |> color_of);
      Graphics.fill_rect (to_screen x) (to_screen y) draw_size draw_size
    done
    done
  end;
  (* drawing boundaries *)
  Graphics.set_line_width 2;
  Graphics.set_color (if black_and_white then Graphics.black else Graphics.white);
  for y = 0 to sz.y do
    for x = 0 to sz.x do
      if get board {x;y} <> get board {x=x-1;y} then
        vert_line x y;
      if get board {x;y} <> get board {x;y=y-1} then
        hori_line x y;
    done
  done;

  if not black_and_white then 
  draw_border board

let init_graphics board_sz = 
  let open Point in
  let draw_sz = draw_size in
  Graphics.open_graph (Printf.sprintf " %dx%d" (board_sz.x * draw_sz + 2 * margin) (board_sz.y * draw_sz + 2 * margin));
  Graphics.set_font "12x24";
  ()

open Collections.Persist

let persistable (char2poly:(char -> Polyomino.t)) : (module Persistable with type t=t) =
  let char_to_square = (function
  | '.' -> Vacant
  | '#' -> Blocked
  | ch -> Occupied (char2poly ch)
  ) in
  let module PersistableBoard = struct 
    type nonrec t = t
    let save out board = (
      let size = size board in
      Point.save out size;
      for x = 0 to size.x-1 do 
        for y = 0 to size.y-1 do
          let sq = get board {x;y} in
          output_char out (square_to_char sq)
        done
      done
    )
    let load inp = (
      let size = Point.load inp in
      let squares = ref PointMap.empty in
      for x = 0 to size.x-1 do 
        for y = 0 to size.y-1 do
          let ch = input_char inp in
          let sq = char_to_square ch in
          match sq with
          | Blocked -> ()
          | Vacant -> squares := PointMap.add {x;y} None !squares
          | Occupied p -> squares := PointMap.add {x;y} (Some p) !squares
        done
      done;
      {size; squares = !squares}
    )
  end in
  (module PersistableBoard : Collections.Persist.Persistable with type t = t)

let%expect_test "save and load a board" =
  let pieces = Polyomino.of_order 3 in
  let board = rectangle 5 4 in
  let char2poly name =
    List.find (fun candidate -> Polyomino.name candidate == name) pieces in
  let module Persistable = (val persistable char2poly) in

  List.iter (fun poly -> 
    let placement = Polyomino.points poly in
    let placed = put board placement poly in
    Printf.printf "%s\n" (to_string placed);
    let (fname, output) = Filename.open_temp_file "board" "dat" in
    Persistable.save output placed;
    close_out output;
    let input = open_in_bin fname in
    let loaded = Persistable.load input in
    close_in input;
    Printf.printf "%s\n" (to_string loaded);
    Printf.printf "=====================\n"
  ) pieces
  ;[%expect{|
    AAA..
    .....
    .....
    .....

    AAA..
    .....
    .....
    .....

    =====================
    BB...
    B....
    .....
    .....

    BB...
    B....
    .....
    .....

    ===================== |}] 
