open Knights_tour

type t = PointSet.t

let points = Fun.id

let compare = PointSet.compare

let rotate_point Point.{x;y} = Point.{x = -y; y = x}

let mirror_point Point.{x;y} = Point.{y = x; x = y}

let translate (delta : Point.t) = PointSet.map (fun {x; y} -> {
  x = x + delta.x;
  y = y + delta.y
})

(** Translates all points so that all points x and a y coordinates are greater or equal to 0; 
    and have the smallest possible values given these conditions (i.e there is at 
    least on point with [x = 0], and one point (possibly a different one) with [y = 0])) *)
let normalize_translation points =
  let min_x = points |> PointSet.min_x in
  let min_y = points |> PointSet.min_y in
  translate {x = -min_x; y = -min_y} points 
let rotate points = PointSet.map rotate_point points |> normalize_translation
let mirror = PointSet.map mirror_point

let%expect_test "rotate and normalize translation" =
  let open PointSet in
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

module PointSetSet = Set.Make(PointSet)

let variants ini = 
  let open Searchspace in
  (
    int_range 0 1 |=> fun mirror_count ->
    int_range 0 3 |-> fun rot_count -> 
      ini 
      |> Fun.repeat rot_count rotate 
      |> Fun.repeat mirror_count mirror
  )
  |> Searchspace.to_seq
  |> PointSetSet.of_seq
  |> PointSetSet.to_seq

let create points = variants points 
  |> Seq.uncons 
  |> Option.get
  |> fun (hd, _) -> hd

let of_string s = PointSet.of_string s |> create

let to_string = PointSet.to_string

let%expect_test "variants assymetric" =
  of_string
    ".# 
     ##
     .#
     .#"
  |> variants
  |> Seq.map PointSet.to_string
  |> Seq.iter (Format.printf "-------------\n%s")
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
  |> Seq.map PointSet.to_string
  |> Seq.iter (Format.printf "-------------\n%s")
  ;[%expect{|
    -------------
    .#.
    ###
    .#. |}]

let%expect_test "variants are equal to one another" =
  let open Printf in
  let vars = of_string
    ".# 
     ##
     .#
     .#"
    |> variants
    |> Seq.map create
    |> Array.of_seq
  in
    vars |> Array.iteri (fun i1 v1 ->
      printf "%d: " i1;
      vars |> Array.iter (fun v2 ->
        printf "%b " ((compare v1 v2) = 0)
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
