type 'a column = {
  label : string;
  extract_and_format : int -> 'a -> string;
  width : int;  (* 0 = use label length *)
}

type 'a t = 'a column list

let add_column ?(width = 0) ~label ~extract_and_format cols =
  { label; extract_and_format; width = max width (String.length label) } :: cols

let print_header (cols : 'a t) =
  let reversed = List.rev cols in
  let widths = List.map (fun c -> c.width) reversed in
  let rec print_labels cs wds =
    match cs, wds with
    | [], [] -> ()
    | [c], [w] -> Printf.printf "%*s" w c.label
    | c :: rest, w :: ws ->
        Printf.printf "%*s" w c.label;
        Printf.printf " | ";
        print_labels rest ws
    | _ -> ()
  in
  print_labels reversed widths;
  Printf.printf "\n%!";
  let rec print_sep wds =
    match wds with
    | [] -> ()
    | [w] -> Printf.printf "%s" (String.make w '-')
    | w :: ws ->
        Printf.printf "%s" (String.make w '-');
        Printf.printf " | ";
        print_sep ws
  in
  print_sep widths;
  Printf.printf "\n%!"

let print_row (cols : 'a t) (row : 'a) =
  let reversed = List.rev cols in
  let widths = List.map (fun c -> c.width) reversed in
  let rec print_cells cs wds =
    match cs, wds with
    | [], [] -> ()
    | [c], [w] -> Printf.printf "%s" (c.extract_and_format w row);
    | c :: rest, w :: ws ->
        Printf.printf "%s" (c.extract_and_format w row);
        Printf.printf " | ";
        print_cells rest ws
    | _ -> ()
  in
  print_cells reversed widths;
  Printf.printf "\n%!"

(** Standard formatter functions. Each takes a width and returns a function
    that formats the value into a string of that width. *)
let ellipsis = "\xe2\x80\xa6" (* … — 3 bytes in UTF-8 *)

let grapheme_len s = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 s

let take_graphemes s n =
  let rec aux count acc = function
    | [] -> String.concat "" (List.rev acc)
    | seg :: rest ->
        if count >= n then String.concat "" (List.rev acc)
        else aux (count + 1) (seg :: acc) rest
  in
  if n <= 0 then ""
  else
    let segs =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun acc seg -> seg :: acc)
        [] s
    in
    aux 0 [] (List.rev segs)

let chop w s =
  if grapheme_len s <= w then String.make (w - grapheme_len s) ' ' ^ s
  else if w <= 1 then ellipsis
  else take_graphemes s (w - 1) ^ ellipsis

let format_int w v =
  let num_str = string_of_int v in
  if w >= String.length num_str then Printf.sprintf "%*d" w v
  else (
    (* Try E notation with decreasing precision, start at width *)
    let rec find_fit prec =
      if prec < 0 then None
      else
        let e_str = Printf.sprintf "%.*e" prec (float_of_int v) in
        if String.length e_str <= w then Some e_str
        else find_fit (prec - 1)
    in
    match find_fit w with
    | Some e_str -> Printf.sprintf "%*s" w e_str
    | None -> chop w (Printf.sprintf "%.0e" (float_of_int v))
  )
let format_percent w v = Printf.sprintf "%*s" w (Printf.sprintf "%.1f%%" v)
let format_string_left w s =
  if grapheme_len s <= w then s ^ String.make (w - grapheme_len s) ' '
  else if w <= 1 then ellipsis
  else take_graphemes s (w - 1) ^ ellipsis
let format_string_right w s =
  if grapheme_len s <= w then String.make (w - grapheme_len s) ' ' ^ s
  else if w <= 1 then ellipsis
  else take_graphemes s (w - 1) ^ ellipsis

let%expect_test "simple header with 3 columns" =
  let table =
    add_column ~label:"Hello" ~extract_and_format:format_string_right []
    |> add_column ~label:"my" ~extract_and_format:format_string_right
    |> add_column ~label:"Friend" ~extract_and_format:format_string_right
  in
  print_header table;
  [%expect {|
    Hello | my | Friend
    ----- | -- | ------
    |}]

let%expect_test "format_int adapts to width for big number" =
  List.iter (fun x ->
    Printf.printf "Formatting: %d\n\n" x;
    for i=21 downto 1 do
      Printf.printf "width %d: |%s|\n" i (format_int i x)
    done;
  ) [1234567890;-1234567890;1234567890123456789];
  [%expect{|
    Formatting: 1234567890

    width 21: |           1234567890|
    width 20: |          1234567890|
    width 19: |         1234567890|
    width 18: |        1234567890|
    width 17: |       1234567890|
    width 16: |      1234567890|
    width 15: |     1234567890|
    width 14: |    1234567890|
    width 13: |   1234567890|
    width 12: |  1234567890|
    width 11: | 1234567890|
    width 10: |1234567890|
    width 9: |1.235e+09|
    width 8: |1.23e+09|
    width 7: |1.2e+09|
    width 6: | 1e+09|
    width 5: |1e+09|
    width 4: |1e+…|
    width 3: |1e…|
    width 2: |1…|
    width 1: |…|
    Formatting: -1234567890

    width 21: |          -1234567890|
    width 20: |         -1234567890|
    width 19: |        -1234567890|
    width 18: |       -1234567890|
    width 17: |      -1234567890|
    width 16: |     -1234567890|
    width 15: |    -1234567890|
    width 14: |   -1234567890|
    width 13: |  -1234567890|
    width 12: | -1234567890|
    width 11: |-1234567890|
    width 10: |-1.235e+09|
    width 9: |-1.23e+09|
    width 8: |-1.2e+09|
    width 7: | -1e+09|
    width 6: |-1e+09|
    width 5: |-1e+…|
    width 4: |-1e…|
    width 3: |-1…|
    width 2: |-…|
    width 1: |…|
    Formatting: 1234567890123456789

    width 21: |  1234567890123456789|
    width 20: | 1234567890123456789|
    width 19: |1234567890123456789|
    width 18: |1.234567890123e+18|
    width 17: |1.23456789012e+18|
    width 16: |1.2345678901e+18|
    width 15: |1.234567890e+18|
    width 14: |1.23456789e+18|
    width 13: |1.2345679e+18|
    width 12: |1.234568e+18|
    width 11: |1.23457e+18|
    width 10: |1.2346e+18|
    width 9: |1.235e+18|
    width 8: |1.23e+18|
    width 7: |1.2e+18|
    width 6: | 1e+18|
    width 5: |1e+18|
    width 4: |1e+…|
    width 3: |1e…|
    width 2: |1…|
    width 1: |…|
    |}]

let%expect_test "format_string_left with shrinking width" =
   let text = "Froombotswana" in
   for i = String.length text +2 downto 1 do
    let formatted = (format_string_left i text) in
    Format.printf "Width %d=%d |%s|\n" i (grapheme_len formatted) formatted
   done;
   [%expect{|
     Width 15=15 |Froombotswana  |
     Width 14=14 |Froombotswana |
     Width 13=13 |Froombotswana|
     Width 12=12 |Froombotswa…|
     Width 11=11 |Froombotsw…|
     Width 10=10 |Froombots…|
     Width 9=9 |Froombot…|
     Width 8=8 |Froombo…|
     Width 7=7 |Froomb…|
     Width 6=6 |Froom…|
     Width 5=5 |Froo…|
     Width 4=4 |Fro…|
     Width 3=3 |Fr…|
     Width 2=2 |F…|
     Width 1=1 |…|
     |}]

let%expect_test "format_string_right with shrinking width" =
   let text = "Froombotswana" in
   for i = String.length text +2 downto 1 do
    let formatted = (format_string_right i text) in
    Format.printf "Width %d=%d |%s|\n" i (grapheme_len formatted) formatted
   done;
   [%expect{|
     Width 15=15 |  Froombotswana|
     Width 14=14 | Froombotswana|
     Width 13=13 |Froombotswana|
     Width 12=12 |Froombotswa…|
     Width 11=11 |Froombotsw…|
     Width 10=10 |Froombots…|
     Width 9=9 |Froombot…|
     Width 8=8 |Froombo…|
     Width 7=7 |Froomb…|
     Width 6=6 |Froom…|
     Width 5=5 |Froo…|
     Width 4=4 |Fro…|
     Width 3=3 |Fr…|
     Width 2=2 |F…|
     Width 1=1 |…|
     |}]
