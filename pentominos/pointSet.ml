(** A PointSet is a set of points in 2d space. Each point has int coordinates and represents
    a square on a chess-like game board (of unspecified/inifinite dimenions)*)

open Knights_tour

include Set.Make(Point)

let of_string image =
  let lines = 
        String.split_on_char '\n' image
        |> List.map String.trim 
  in 
  let points = ref empty in
  lines |> List.iteri (fun y line ->
    line |> String.iteri (fun x c -> match c with
      | '.' | ' ' -> () 
      | _ -> points := add {x;y} !points
    )
  );
  !points

let min_map selector points =
  fold (fun pt acc -> Int.min (selector pt) acc) points Int.max_int

let max_map selector points =
  fold (fun pt acc -> Int.max (selector pt) acc) points Int.min_int
let min_x = min_map (fun p -> p.x)
let max_x = max_map (fun p -> p.x)
let min_y = min_map (fun p -> p.y)
let max_y = max_map (fun p -> p.y)

let%expect_test "min max" =
  let points = of_string "
      .
      ...#
      ..##
      ...#########
      ..."
  in (
    Format.printf "min_x = %d\n" (min_x points);
    Format.printf "max_x = %d\n" (max_x points);
    Format.printf "min_y = %d\n" (min_y points);
    Format.printf "max_y = %d\n" (max_y points);
  )
  ;[%expect{|
    min_x = 2
    max_x = 11
    min_y = 2
    max_y = 4 |}] 
  
let%expect_test "pointset parsed from string image" =
   of_string (
    ".#     
     ##      
     .##"
  ) 
  |> iter (fun {x;y} -> (Format.printf "(%d, %d)\n" x y))
  ;[%expect{|
    (0, 1)
    (1, 0)
    (1, 1)
    (1, 2)
    (2, 2) |}]

let manhatan_distance p1 p2 = 
  let open Point in 
  Int.abs (p1.x - p2.x) + Int.abs (p1.y - p2.y)
 
let is_neighbour p1 p2 = (manhatan_distance p1 p2) = 1
let is_coherent points =
  points |> for_all (fun p -> 
    exists (is_neighbour p) points
  )

let to_string points =
  let image = Buffer.create 60 in
  for y = min_y points to max_y points do 
    for x = min_x points to max_x points do
      Buffer.add_char image (
        if mem {x;y} points then '#' else '.'
      )
    done;
    Buffer.add_char image '\n'
  done;
  Buffer.contents image

let%expect_test "to_string" =
  let points = of_string "
      .
      ...#
      ..##
      ...#########
      ..."
  in print_endline (to_string points)
  ;[%expect{|
    .#........
    ##........
    .######### |}] 
  
  