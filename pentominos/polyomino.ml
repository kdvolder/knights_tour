open Knights_tour

type t = {
  name: char;
  variants: PointSet.t list
}

let name {name;_} = name

let points {variants;_} = match variants with 
  | canonical_rep::_ -> canonical_rep
  | [] -> failwith "Bug: this should not be possible. A polymino should have at least 1 variant."

let order p = PointSet.cardinal (points p)

let compare p1 p2 = PointSet.compare (points p1) (points p2)

let variants {variants;_}= variants

let to_string p = p |> points |> PointSet.to_string

let adjacent_point points = PointSet.adjacent points 
  |> PointSet.to_seq |> Searchspace.of_seq

let rec of_order_aux n = 
  let open Searchspace in (
    if n<1 then
      failwith "Illegal argument: order must >=1"
    else if n=1 then
      PointSet.of_string "#" |> PointSet.normalize |> return
    else (
      let with_duplicates = of_order_aux (n - 1) |=> (fun smaller_piece ->
        adjacent_point smaller_piece |=> (fun adjacent_point ->
          PointSet.add adjacent_point smaller_piece
          |> PointSet.normalize
          |> return
        )
      ) in 
      with_duplicates |> Searchspace.no_dup PointSet.compare
    )
  )

let of_order n =
  let nextName = ref 'A' in
  of_order_aux n 
  |> Searchspace.map (fun points ->
    let p = {name = !nextName; variants = PointSet.variants points} in
    nextName := (Char.code !nextName) + 1 |> Char.chr;
    p
  )
  |> Searchspace.to_seq
  |> List.of_seq

let print_polyos n =
  let polys = of_order n in
  polys |> List.iteri (fun i piece -> 
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

let%expect_test "pento-minos" =
  print_polyos 5
  ;[%expect{|
    =============
    1:
    -------------
    #####

    =============
    2:
    -------------
    ####
    #...

    =============
    3:
    -------------
    ####
    .#..

    =============
    4:
    -------------
    ###
    ##.

    =============
    5:
    -------------
    ###
    #.#

    =============
    6:
    -------------
    ###
    #..
    #..

    =============
    7:
    -------------
    ###
    .#.
    .#.

    =============
    8:
    -------------
    ###.
    ..##

    =============
    9:
    -------------
    ##.
    .##
    .#.

    =============
    10:
    -------------
    ##.
    .##
    ..#

    =============
    11:
    -------------
    ##.
    .#.
    .##

    =============
    12:
    -------------
    .#.
    ###
    .#. |}]

let pp_poly out poly =
  let open Format in
  pp_open_vbox out 5;
  fprintf out "\n%s" (to_string poly);
  pp_close_box out ()

