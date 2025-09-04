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
    (1, 0)
    (0, 1)
    (1, 1)
    (1, 2)
    (2, 2) |}]
 
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

let neighbours Point.{x;y} = Point.[
  { x = x - 1 ; y         };
  { x         ; y = y - 1 };
  { x = x + 1 ; y         };
  { x         ; y = y + 1 }
] |> of_list

let flatmap f points = fold (fun elt acc -> union (f elt) acc) points empty

let adjacent points = 
  let all_neighbours = points |> flatmap neighbours in
  diff all_neighbours points

let%expect_test "adjacent" =
  let input = of_string "
    #
    #
    ##
  " in
  input
  |> adjacent
  |> (fun adjacent -> 
    Printf.printf "org: (%d, %d)\n" (min_x input) (min_y input);
    Printf.printf "adj: (%d, %d)\n" (min_x adjacent) (min_y adjacent);
    Printf.printf "%s\n" (to_string adjacent)
  )
  ;[%expect{|
    org: (0, 1)
    adj: (-1, 0)
    .#..
    #.#.
    #.#.
    #..#
    .##. |}]

let translate (delta : Point.t) = map (fun {x; y} -> {
  x = x + delta.x;
  y = y + delta.y
})

let normalize_translation points =
  let min_x = points |> min_x in
  let min_y = points |> min_y in
  translate {x = -min_x; y = -min_y} points 

let rotate_point Point.{x;y} = Point.{x = -y; y = x}

let mirror_point Point.{x;y} = Point.{y = x; x = y}

let rotate points = map rotate_point points |> normalize_translation
let mirror = map mirror_point

module PointSetSet = Set.Make(struct 
  type nonrec t = t
  let compare = compare 
end)

let variants ini = 
  let open Searchspace in
  (
    int_range 0 1 |=> fun mirror_count ->
    int_range 0 3 |-> fun rot_count -> 
      ini |> normalize_translation
      |> Fun.repeat rot_count rotate 
      |> Fun.repeat mirror_count mirror
  )
  |> Searchspace.to_seq
  |> PointSetSet.of_seq
  |> PointSetSet.to_seq
  |> List.of_seq
  
let variants_with_transform shape =
  let open Searchspace in
  let transforms =
    int_range 0 1 |=> fun mirror_count ->
    int_range 0 3 |-> fun rot_count ->
      fun pts ->
        pts
        |> normalize_translation
        |> Fun.repeat rot_count rotate
        |> Fun.repeat mirror_count mirror
  in
  let seen = ref PointSetSet.empty in
  let results = ref [] in
  transforms |> to_seq |> Seq.iter (fun transform ->
    let transformed = transform shape |> normalize_translation in
    if not (PointSetSet.mem transformed !seen) then begin
      seen := PointSetSet.add transformed !seen;
      results := (transform, transformed) :: !results
    end
  );
  List.rev !results
  
let normalize points = 
  points
  |> variants
  |> List.hd
  
let%expect_test "variants assymetric" =
  of_string
    ".# 
     ##
     .#
     .#"
  |> variants
  |> List.map to_string
  |> List.iter (Format.printf "-------------\n%s")
  ;[%expect{|
    -------------
    ####
    .#..
    -------------
    ####
    ..#.
    -------------
    #.
    ##
    #.
    #.
    -------------
    #.
    #.
    ##
    #.
    -------------
    .#..
    ####
    -------------
    .#
    ##
    .#
    .#
    -------------
    .#
    .#
    ##
    .#
    -------------
    ..#.
    #### |}]

let%expect_test "variants symmetric" = 
  of_string
    ".#
     ###
     .#"
  |> variants
  |> List.map to_string
  |> List.iter (Format.printf "-------------\n%s")
  ;[%expect{|
    -------------
    .#.
    ###
    .#. |}]

  
let%expect_test "normalized rep of variants are equal to one another" =
  let open Printf in
  let vars = of_string
    ".# 
     ##
     .#
     .#"
    |> variants
    |> Array.of_list
  in
    vars |> Array.iteri (fun i1 v1 ->
      printf "%d: " i1;
      vars |> Array.iter (fun v2 ->
        printf "%b " ((compare (normalize v1) (normalize v2)) = 0)
      );
      printf "\n"
    ) ; [%expect{|
      0: true true true true true true true true
      1: true true true true true true true true
      2: true true true true true true true true
      3: true true true true true true true true
      4: true true true true true true true true
      5: true true true true true true true true
      6: true true true true true true true true
      7: true true true true true true true true |}]

let%expect_test "rotate and normalize translation" =
  let points = of_string (
    ".#     
     ##      
     .#
     .#"
  ) in
  for i = 0 to 3 do
    Fun.repeat i rotate points
    |> (fun points -> 
      Printf.printf "=======================\n%!";
      Printf.printf "min_x : %d   min_y : %d\n%!" (min_x points) (min_y points);
      Printf.printf "=======================\n%!";
      Printf.printf "%s\n%!" (to_string points)
    )
  done 
  ; [%expect{|
    =======================
    min_x : 0   min_y : 0
    =======================
    .#
    ##
    .#
    .#

    =======================
    min_x : 0   min_y : 0
    =======================
    ..#.
    ####

    =======================
    min_x : 0   min_y : 0
    =======================
    #.
    #.
    ##
    #.

    =======================
    min_x : 0   min_y : 0
    =======================
    ####
    .#.. |}]

let%expect_test "variants_with_transform" =
  let input_shapes = List.map of_string [
      "##
       ##"
   ;
      "##
       ##
       ##
       ##"
   ;
      "#.
       ##"
    ;
      "#
       ##"
    ;
      "#
       #
       ##"
  ] in
  let test_shape = of_string (
    ".#     
     ##      
     .#
     .#"
  ) in
  input_shapes |> List.iter (fun shape ->
    Printf.printf "Shape:\n%s\n" (to_string shape);
  let syms = variants_with_transform shape in
    Printf.printf "Symmetries: %d\n" (List.length syms);
    List.iteri (fun i (transform, b) -> (
      Printf.printf "Symmetry %d:\n" i;
      Printf.printf "%s\n" (to_string b);
      Printf.printf "Transformed shape:\n";
      Printf.printf "%s\n" (test_shape |> transform |> to_string);
      Printf.printf "---\n"
    )) syms;
    Printf.printf "========================\n"
  ); 
  [%expect{|
    Shape:
    ##
    ##

    Symmetries: 1
    Symmetry 0:
    ##
    ##

    Transformed shape:
    .#
    ##
    .#
    .#

    ---
    ========================
    Shape:
    ##
    ##
    ##
    ##

    Symmetries: 2
    Symmetry 0:
    ##
    ##
    ##
    ##

    Transformed shape:
    .#
    ##
    .#
    .#

    ---
    Symmetry 1:
    ####
    ####

    Transformed shape:
    ..#.
    ####

    ---
    ========================
    Shape:
    #.
    ##

    Symmetries: 4
    Symmetry 0:
    #.
    ##

    Transformed shape:
    .#
    ##
    .#
    .#

    ---
    Symmetry 1:
    ##
    #.

    Transformed shape:
    ..#.
    ####

    ---
    Symmetry 2:
    ##
    .#

    Transformed shape:
    #.
    #.
    ##
    #.

    ---
    Symmetry 3:
    .#
    ##

    Transformed shape:
    ####
    .#..

    ---
    ========================
    Shape:
    #.
    ##

    Symmetries: 4
    Symmetry 0:
    #.
    ##

    Transformed shape:
    .#
    ##
    .#
    .#

    ---
    Symmetry 1:
    ##
    #.

    Transformed shape:
    ..#.
    ####

    ---
    Symmetry 2:
    ##
    .#

    Transformed shape:
    #.
    #.
    ##
    #.

    ---
    Symmetry 3:
    .#
    ##

    Transformed shape:
    ####
    .#..

    ---
    ========================
    Shape:
    #.
    #.
    ##

    Symmetries: 8
    Symmetry 0:
    #.
    #.
    ##

    Transformed shape:
    .#
    ##
    .#
    .#

    ---
    Symmetry 1:
    ###
    #..

    Transformed shape:
    ..#.
    ####

    ---
    Symmetry 2:
    ##
    .#
    .#

    Transformed shape:
    #.
    #.
    ##
    #.

    ---
    Symmetry 3:
    ..#
    ###

    Transformed shape:
    ####
    .#..

    ---
    Symmetry 4:
    ###
    ..#

    Transformed shape:
    .#..
    ####

    ---
    Symmetry 5:
    ##
    #.
    #.

    Transformed shape:
    .#
    .#
    ##
    .#

    ---
    Symmetry 6:
    #..
    ###

    Transformed shape:
    ####
    ..#.

    ---
    Symmetry 7:
    .#
    .#
    ##

    Transformed shape:
    #.
    ##
    #.
    #.

    ---
    ========================
    |}]

    