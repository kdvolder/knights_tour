(**
  Disk-based memoization utility for expensive computations.
  Uses Marshal for serialization and a hash function for cache keys.
*)

let default_cache_dir = Filename.concat (Filename.get_temp_dir_name ()) "ocaml_cache"

let ensure_cache_dir_exists dir =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755

(**
  [memoize ~cache_dir ~hash f] returns a memoized version of [f].
  - [hash] is a function from input to string (used as filename).
  - [cache_dir] is the directory to store cache files (default: /tmp/ocaml_cache).
  - [f] is the expensive function to memoize.
*)
let memoize
    ?(cache_dir=default_cache_dir)
    ~(function_name:string)
    ~hash
    f =
  let fn_dir = Filename.concat cache_dir function_name in
  ensure_cache_dir_exists cache_dir;
  ensure_cache_dir_exists fn_dir;
  fun x ->
    let key = hash x in
    let file = Filename.concat fn_dir key in
    if Sys.file_exists file then (
      let ic = open_in_bin file in
      let result = Marshal.from_channel ic in
      close_in ic;
      result
    ) else (
      let result = f x in
      let oc = open_out_bin file in
      Marshal.to_channel oc result [];
      close_out oc;
      result
    )
  
  let%expect_test "memoize basic usage" =
    let slow_square x =
      Printf.printf "Computing square of %d\n" x;
      x * x
    in
    let hash_int x = string_of_int x in
    let memo_square = memoize ~function_name:"square" ~hash:hash_int slow_square in
    Printf.printf "Result: %d\n" (memo_square 5);
    Printf.printf "Result: %d\n" (memo_square 5);
    Printf.printf "Result: %d\n" (memo_square 6);
    Printf.printf "Result: %d\n" (memo_square 6);
    [%expect{|
      Computing square of 5
      Result: 25
      Result: 25
      Computing square of 6
      Result: 36
      Result: 36
      |}]
