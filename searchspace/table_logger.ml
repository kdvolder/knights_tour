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
let format_float w v =
  let rec find_fit prec =
    if prec < 0 then None
    else
      let f_str = Printf.sprintf "%.*g" prec v in
      if String.length f_str <= w then Some f_str
      else find_fit (prec - 1)
  in
  match find_fit 6 with (* start with 6 significant digits *)
  | Some f_str -> Printf.sprintf "%*s" w f_str
  | None -> chop w (Printf.sprintf "%.0g" v)

let format_percent w v =
  if w <= 1 then ellipsis
  else
    let rec find_fit prec =
      if prec < 0 then None
      else
        let f_str = Printf.sprintf "%.*g" prec (v *. 100.0) in
        if String.length f_str + 1 <= w then Some (f_str ^ "%")
        else find_fit (prec - 1)
    in
    match find_fit 6 with (* start with 6 significant digits *)
    | Some f_str -> Printf.sprintf "%*s" w f_str
    | None -> chop (w - 1) (Printf.sprintf "%g" (v *. 100.0)) ^ "%"
let format_string_left w s =
  if grapheme_len s <= w then s ^ String.make (w - grapheme_len s) ' '
  else if w <= 1 then ellipsis
  else take_graphemes s (w - 1) ^ ellipsis
let format_string_right w s =
  if grapheme_len s <= w then String.make (w - grapheme_len s) ' ' ^ s
  else if w <= 1 then ellipsis
  else take_graphemes s (w - 1) ^ ellipsis

let kalpa_seconds = 1.36328832e+17 (* 4.32 billion years *)

let format_time w seconds =
  let rec fmt (secs : float) : string =
    if secs < 0. then "-" ^ fmt (-.secs)
    else
      if secs >= kalpa_seconds then format_float (w - 7) (secs /. kalpa_seconds) ^ " kalpas"
      else
        let max_int = float_of_int (max_int / 2) in (* ~68 years *)
        if secs > max_int then format_float (w - 6) (secs /. 31536000.) ^ " years"
        else
        let total = int_of_float secs in
        if total < 60 then string_of_int total ^ " s"
        else if total < 3600 then
          let m = total / 60 in
          let s = total mod 60 in
          if s = 0 then string_of_int m ^ " min"
          else string_of_int m ^ " min " ^ string_of_int s ^ " s"
        else if total < 86400 then
          let h = total / 3600 in
          let rem = total mod 3600 in
          if rem = 0 then string_of_int h ^ " h"
          else
            let m = rem / 60 in
            let s = rem mod 60 in
            if m = 0 then string_of_int h ^ " h " ^ string_of_int s ^ " s"
            else if s = 0 then string_of_int h ^ " h " ^ string_of_int m ^ " min"
            else string_of_int h ^ " h " ^ string_of_int m ^ " min " ^ string_of_int s ^ " s"
        else if total < 31536000 then
          let d = total / 86400 in
          let rem = total mod 86400 in
          if rem = 0 then string_of_int d ^ " day" ^ (if d > 1 then "s" else "")
          else
            let h = rem / 3600 in
            let m = (rem mod 3600) / 60 in
            let s = rem mod 60 in
            if h = 0 && m = 0 then string_of_int d ^ " day" ^ (if d > 1 then "s" else "")
            else if s = 0 && m = 0 then string_of_int d ^ " day" ^ (if d > 1 then "s" else "") ^ " " ^ string_of_int h ^ " h"
            else if m = 0 then string_of_int d ^ " day" ^ (if d > 1 then "s" else "") ^ " " ^ string_of_int h ^ " h " ^ string_of_int s ^ " s"
            else if s = 0 then string_of_int d ^ " day" ^ (if d > 1 then "s" else "") ^ " " ^ string_of_int h ^ " h " ^ string_of_int m ^ " min"
            else string_of_int d ^ " day" ^ (if d > 1 then "s" else "") ^ " " ^ string_of_int h ^ " h " ^ string_of_int m ^ " min " ^ string_of_int s ^ " s"
        else
          let years = float_of_int (total / 31536000) in
          if years > 1e9 then Printf.sprintf "%.5g years" years
          else string_of_int (int_of_float years) ^ " year" ^ (if int_of_float years > 1 then "s" else "")
  in
  let raw = fmt seconds in
  if grapheme_len raw <= w then String.make (w - grapheme_len raw) ' ' ^ raw
  else if w <= 1 then ellipsis
  else take_graphemes raw (w - 1) ^ ellipsis

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

let%expect_test "format_float adapts to width" =
  List.iter (fun (label, v) ->
    Printf.printf "Formatting: %s (%.6g)\n" label v;
    for i = 12 downto 1 do
      Printf.printf "width %d: |%s|\n" i (format_float i v)
    done;
  ) [
    "normal", 45.6789;
    "large", 1.23456789e+30;
    "small", 1.23456789e-30;
    "negative", -1.23456789e+10;
  ];
  [%expect{|
    Formatting: normal (45.6789)
    width 12: |     45.6789|
    width 11: |    45.6789|
    width 10: |   45.6789|
    width 9: |  45.6789|
    width 8: | 45.6789|
    width 7: |45.6789|
    width 6: |45.679|
    width 5: |45.68|
    width 4: |45.7|
    width 3: | 46|
    width 2: |46|
    width 1: |…|
    Formatting: large (1.23457e+30)
    width 12: | 1.23457e+30|
    width 11: |1.23457e+30|
    width 10: |1.2346e+30|
    width 9: |1.235e+30|
    width 8: |1.23e+30|
    width 7: |1.2e+30|
    width 6: | 1e+30|
    width 5: |1e+30|
    width 4: |1e+…|
    width 3: |1e…|
    width 2: |1…|
    width 1: |…|
    Formatting: small (1.23457e-30)
    width 12: | 1.23457e-30|
    width 11: |1.23457e-30|
    width 10: |1.2346e-30|
    width 9: |1.235e-30|
    width 8: |1.23e-30|
    width 7: |1.2e-30|
    width 6: | 1e-30|
    width 5: |1e-30|
    width 4: |1e-…|
    width 3: |1e…|
    width 2: |1…|
    width 1: |…|
    Formatting: negative (-1.23457e+10)
    width 12: |-1.23457e+10|
    width 11: |-1.2346e+10|
    width 10: |-1.235e+10|
    width 9: |-1.23e+10|
    width 8: |-1.2e+10|
    width 7: | -1e+10|
    width 6: |-1e+10|
    width 5: |-1e+…|
    width 4: |-1e…|
    width 3: |-1…|
    width 2: |-…|
    width 1: |…|
    |}]

let%expect_test "format_percent adapts to width" =
  List.iter (fun (label, v) ->
    Printf.printf "Formatting: %s (%.6g)\n" label v;
    for i = 12 downto 1 do
      Printf.printf "width %d: |%s|\n" i (format_percent i v)
    done;
  ) [
    "normal", 0.456789;
    "large", 1234.56789;
    "tiny", 0.000123456789;
    "negative", -0.456789;
  ];
  [%expect{|
    Formatting: normal (0.456789)
    width 12: |    45.6789%|
    width 11: |   45.6789%|
    width 10: |  45.6789%|
    width 9: | 45.6789%|
    width 8: |45.6789%|
    width 7: |45.679%|
    width 6: |45.68%|
    width 5: |45.7%|
    width 4: | 46%|
    width 3: |46%|
    width 2: |…%|
    width 1: |…|
    Formatting: large (1234.57)
    width 12: |     123457%|
    width 11: |    123457%|
    width 10: |   123457%|
    width 9: |  123457%|
    width 8: | 123457%|
    width 7: |123457%|
    width 6: |1e+05%|
    width 5: |123…%|
    width 4: |12…%|
    width 3: |1…%|
    width 2: |…%|
    width 1: |…|
    Formatting: tiny (0.000123457)
    width 12: |  0.0123457%|
    width 11: | 0.0123457%|
    width 10: |0.0123457%|
    width 9: |0.012346%|
    width 8: |0.01235%|
    width 7: |0.0123%|
    width 6: |0.012%|
    width 5: |0.01%|
    width 4: |0.…%|
    width 3: |0…%|
    width 2: |…%|
    width 1: |…|
    Formatting: negative (-0.456789)
    width 12: |   -45.6789%|
    width 11: |  -45.6789%|
    width 10: | -45.6789%|
    width 9: |-45.6789%|
    width 8: |-45.679%|
    width 7: |-45.68%|
    width 6: |-45.7%|
    width 5: | -46%|
    width 4: |-46%|
    width 3: |-…%|
    width 2: |…%|
    width 1: |…|
    |}]

let%expect_test "format_time: seconds" =
  Printf.printf "10s: |%s|\n" (format_time 10 0.0);
  Printf.printf "10s: |%s|\n" (format_time 10 5.0);
  Printf.printf "10s: |%s|\n" (format_time 10 42.0);
  [%expect{|
    10s: |       0 s|
    10s: |       5 s|
    10s: |      42 s|
    |}]

let%expect_test "format_time: minutes and seconds" =
  Printf.printf "10s: |%s|\n" (format_time 10 59.0);
  Printf.printf "10s: |%s|\n" (format_time 10 60.0);
  Printf.printf "10s: |%s|\n" (format_time 10 142.0);
  [%expect{|
    10s: |      59 s|
    10s: |     1 min|
    10s: |2 min 22 s|
    |}]

let%expect_test "format_time: hours" =
  Printf.printf "10s: |%s|\n" (format_time 10 3600.0);
  Printf.printf "15s: |%s|\n" (format_time 15 7530.0);
  [%expect{|
    10s: |       1 h|
    15s: | 2 h 5 min 30 s|
    |}]

let%expect_test "format_time: days" =
  Printf.printf "15s: |%s|\n" (format_time 15 86400.0);
  Printf.printf "25s: |%s|\n" (format_time 25 150125.0);
  [%expect{|
    15s: |          1 day|
    25s: |    1 day 17 h 42 min 5 s|
    |}]

let%expect_test "format_time: years" =
  Printf.printf "10s: |%s|\n" (format_time 10 31536000.0);
  Printf.printf "25s: |%s|\n" (format_time 25 1e30);
  [%expect{|
    10s: |    1 year|
    25s: |       7.33521e+12 kalpas|
    |}]

let%expect_test "format_time: width truncation" =
  for i = 15 downto 1 do
    Printf.printf "width %d: |%s|\n" i (format_time i 150125.0)
  done;
  [%expect{|
    width 15: |1 day 17 h 42 …|
    width 14: |1 day 17 h 42…|
    width 13: |1 day 17 h 4…|
    width 12: |1 day 17 h …|
    width 11: |1 day 17 h…|
    width 10: |1 day 17 …|
    width 9: |1 day 17…|
    width 8: |1 day 1…|
    width 7: |1 day …|
    width 6: |1 day…|
    width 5: |1 da…|
    width 4: |1 d…|
    width 3: |1 …|
    width 2: |1…|
    width 1: |…|
    |}]

let%expect_test "format_time: negative" =
  Printf.printf "%s\n" (format_time 10 (-42.0));
  [%expect{|
    -42 s
  |}]

let%expect_test "format_time: kalpas" =
  Printf.printf "1 kalpa (4.32B years): |%s|\n" (format_time 25 kalpa_seconds);
  Printf.printf "10 kalpas: |%s|\n" (format_time 25 (kalpa_seconds *. 10.));
  Printf.printf "2.4631e+11 years: |%s|\n" (format_time 25 (2.4631e+11 *. 31536000.));
  Printf.printf "1.4e+33 years: |%s|\n" (format_time 25 (1.4e+33 *. 31536000.));
  Printf.printf "1e30 seconds: |%s|\n" (format_time 25 1e30);
  [%expect{|
    1 kalpa (4.32B years): |                 1 kalpas|
    10 kalpas: |                10 kalpas|
    2.4631e+11 years: |           56.9772 kalpas|
    1.4e+33 years: |       3.23852e+23 kalpas|
    1e30 seconds: |       7.33521e+12 kalpas|
    |}]
