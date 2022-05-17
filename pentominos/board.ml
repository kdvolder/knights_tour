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
  
let of_string img =
  let vacancies = PointSet.of_string img |> PointSet.normalize_translation in
  {
    size = Point.{
      x = PointSet.max_x vacancies + 1;
      y = PointSet.max_y vacancies + 1;
    };
    squares = PointSet.fold (fun vacant board -> PointMap.add vacant None board) vacancies PointMap.empty
  }

let%expect_test "Board of_string |> vacant" =
  let open Printf in
  let board = of_string "
  ########
  ########
  ########
  ###..###
  ###..###
  ########
  ########
  ########
  " in
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
