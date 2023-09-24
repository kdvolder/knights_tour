type 'a t = int * 'a list

let empty = (0, [])

let size (sz,_) = sz

let is_empty l = size l = 0

let%expect_test "empty" =
  Printf.printf "size = %d\n" (size empty);
  Printf.printf "is_empty = %B\n" (is_empty empty);
  [%expect{|
    size = 0
    is_empty = true |}]

let singleton x = (1, [x])

let rec take n ~taken ~remaining =
  if n>0 then 
    take (n-1) ~taken:((List.hd remaining)::taken) ~remaining:(List.tl remaining)
  else if n=0 then
    (List.rev taken, remaining)
  else
    raise (Failure "Can't take a negative number of elements from a list")

let to_string str (c,xs) = string_of_int c ^ "[" ^ Util.with_separator str ", " xs ^ "]"

let println msg it = Printf.printf "%s = %s\n" msg (to_string string_of_int it)

let of_list xs = (List.length xs, xs)

let%expect_test "take n" =
    let it = [1;2;3;4;5] in
    let (taken, remaining) = take 2 ~taken:[] ~remaining:it in
    println "taken" (of_list taken);
    println "remaining" (of_list remaining);
    [%expect{|
      taken = 2[1, 2]
      remaining = 3[3, 4, 5] |}]

let split_n n (sz,xs) = 
  take n ~remaining:xs ~taken:[]
  |> fun (lt,rt) -> ((n, lt), (sz-n, rt))
  
let%expect_test "split n" =
  let it = of_list [1;2;3;4;5] in
  println "it" it;
  let (f,b) = split_n 2 it in
  println "f" f;
  println "b" b;
  [%expect{|
    it = 5[1, 2, 3, 4, 5]
    f = 2[1, 2]
    b = 3[3, 4, 5] |}]

let split xs = 
  let n = (size xs) / 2 in
  split_n n xs

let%expect_test "split" =
  let do_test it = (
    let it = of_list it in
    Printf.printf "--- %s\n" (to_string string_of_int it);
    let (f, b) = split it in
    println "f" f;
    println "b" b
  ) in
  do_test [1;2;3;4;5];
  do_test [1;2;3;4];
  do_test [];
  do_test [1];
  [%expect{|
    --- 5[1, 2, 3, 4, 5]
    f = 2[1, 2]
    b = 3[3, 4, 5]
    --- 4[1, 2, 3, 4]
    f = 2[1, 2]
    b = 2[3, 4]
    --- 0[]
    f = 0[]
    b = 0[]
    --- 1[1]
    f = 0[]
    b = 1[1] |}]

let append (sx,x) (sy,y) = (sx+sy, List.append x y)
let reverse (s,x) = (s, List.rev x)

let hd (_,ls) = List.hd ls
let tl (s, ls) = (s-1, List.tl ls)

let map f (sz, ls) = (sz, List.map f ls)

let cons x (sz, xs) = (sz+1, x::xs)

let (++) = append

let to_list (_,xs) = xs

let%expect_test "of_list" =
  println "empty" (of_list []);
  println "single" (of_list [42]);
  println "several" (of_list [11;12;13]);
  [%expect{|
    empty = 0[]
    single = 1[42]
    several = 3[11, 12, 13] |}]

let%expect_test "append" =
  println "empty" (append empty empty);
  println "single l" (append (singleton 42) empty);
  println "single r" (append empty (singleton 42));
  println "double" (append (singleton 42) (singleton 43));
  println "general" (append (of_list [1;2;3]) (of_list [4;5;6]));
  [%expect{|
    empty = 0[]
    single l = 1[42]
    single r = 1[42]
    double = 2[42, 43]
    general = 6[1, 2, 3, 4, 5, 6] |}]

let%expect_test "singleton" =
  let it = singleton 42 in
  Printf.printf "it = %s\n" (to_string string_of_int it);
  Printf.printf "size = %d\n" (size it);
  Printf.printf "is_empty = %B\n" (is_empty it);
  it |> to_list |> List.iter (Printf.printf "el: %d\n");
  [%expect{|
    it = 1[42]
    size = 1
    is_empty = false
    el: 42 |}]

let with_separator str sep (_,els) = Util.with_separator str sep els