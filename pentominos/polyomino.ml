open Knights_tour

(** A Polyomino is represented as a list of variants. Each variant is
    a PointSet that has been normalize by PointSet.normalize_translation;
    but has a different shape obtaining by mirroring and translation.
    The first variant in the list is considered as the 'canonical' 
    representation and can be used to compare two polyominos to
    see if they are 'equal' *)
type t = PointSet.t list

let points : t -> PointSet.t = function 
  | canonical_rep::_ -> canonical_rep
  | [] -> failwith "Bug: this should not be possible. A polymino should have at least 1 variant."

let compare p1 p2 = PointSet.compare (points p1) (points p2)

let rotate_point Point.{x;y} = Point.{x = -y; y = x}

let mirror_point Point.{x;y} = Point.{y = x; x = y}

let rotate points = PointSet.map rotate_point points |> PointSet.normalize_translation
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


let calculate_variants ini = 
  let module PointSetSet = Set.Make(PointSet) in
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
  |> List.of_seq

let create points = points
  |> PointSet.normalize_translation 
  |> calculate_variants

let variants = Fun.id

let of_string s = PointSet.of_string s |> create

let to_string p = p |> points |> PointSet.to_string

let%expect_test "variants assymetric" =
  of_string
    ".# 
     ##
     .#
     .#"
  |> variants
  |> List.map PointSet.to_string
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
  |> List.map PointSet.to_string
  |> List.iter (Format.printf "-------------\n%s")
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
    |> Array.of_list
  in
    vars |> Array.iteri (fun i1 v1 ->
      printf "%d: " i1;
      vars |> Array.iter (fun v2 ->
        printf "%b " ((compare (create v1) (create v2)) = 0)
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

let adjacent_point points = PointSet.adjacent points 
  |> PointSet.to_seq |> Searchspace.of_seq
      
let rec of_order n =
  let open Searchspace in (
    if n<1 then
      failwith "Illegal argument: order must >=1"
    else if n=1 then
      of_string "#" |> return 
    else (
      let with_duplicates = of_order (n - 1) |-> points |=> (fun smaller_piece ->
        adjacent_point smaller_piece |=> (fun adjacent_point ->
          PointSet.add adjacent_point smaller_piece
          |> create
          |> return
        )
      ) in 
      with_duplicates |> Searchspace.no_dup compare
    )
  )

let print_polyos n =
  let polys = of_order n |> Searchspace.to_seq in
  polys |> Seq.iteri (fun i piece -> 
    let open Printf in
    printf "=============\n";
    printf "%d:\n" (i+1);
    printf "-------------\n";
    printf "%s\n" (to_string piece)
  ) 

let%expect_test "mono-minos" =
  print_polyos 1
  ;[%expect{|
    =============
    1:
    -------------
    # |}]

let%expect_test "do-minos" =
  print_polyos 2
  ;[%expect{|
    =============
    1:
    -------------
    ## |}]

let%expect_test "3-minos" =
  print_polyos 3
  ;[%expect{|
    =============
    1:
    -------------
    ###

    =============
    2:
    -------------
    ##
    #. |}]

let%expect_test "tetro-minos" =
  print_polyos 4
  ;[%expect{|
    =============
    1:
    -------------
    ####

    =============
    2:
    -------------
    ###
    #..

    =============
    3:
    -------------
    ###
    .#.

    =============
    4:
    -------------
    ##
    ##

    =============
    5:
    -------------
    ##.
    .## |}]

let pp_poly out poly =
  let open Format in
  pp_open_vbox out 5;
  fprintf out "\n%s" (to_string poly);
  pp_close_box out ()

