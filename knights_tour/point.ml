type t = {x: int; y:int}

let compare {x=x1;y=y1} {x=x2;y=y2} =
  let xcomp = y1 - y2 in
  if xcomp<>0 then 
    xcomp
  else
    x1 - x2

let diff p1 p2 = {
  x = p1.x - p2.x; 
  y = p1.y - p2.y;
}

let load inp = (
  let x = input_binary_int inp in
  {x;y=input_binary_int inp}
)

let save out {x;y} =
  output_binary_int out x;
  output_binary_int out y

let%expect_test "save and load a point" =
  let (data_name, data_file) = Filename.open_temp_file "testdata" ".dat" in
  save data_file {x=4; y=5};
  close_out data_file;
  let input = open_in_bin data_name in
  let loaded = load input in
  Format.printf "{x=%d; y=%d}" loaded.x loaded.y;
  ; [%expect{| {x=4; y=5} |}]

module Set =
  Set.Make(struct type nonrec t = t let compare = compare end)
