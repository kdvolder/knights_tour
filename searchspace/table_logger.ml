type 'a column = {
  label : string;
  extract_and_format : int -> 'a -> string;
}

type 'a t = 'a column list

let add_column ~label ~extract_and_format cols =
  { label; extract_and_format } :: cols

let print_header (cols : 'a t) =
  let reversed = List.rev cols in
  let labels = List.map (fun c -> c.label) reversed in
  let widths = List.map String.length labels in
  let rec print_labels lbls wds =
    match lbls, wds with
    | [], [] -> ()
    | l :: ls, w :: ws ->
        Printf.printf "%s" (String.make w ' ');
        let padding = w - String.length l in
        Printf.printf "%s" (String.make padding ' ');
        Printf.printf "%s" l;
        print_labels ls ws
    | _ -> ()
  in
  print_labels labels widths;
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
  let rec print_cells cs =
    match cs with
    | [] -> ()
    | [c] -> Printf.printf "%s" (c.extract_and_format (String.length c.label) row);
    | c :: rest ->
        Printf.printf "%s" (c.extract_and_format (String.length c.label) row);
        Printf.printf " | ";
        print_cells rest
  in
  print_cells reversed;
  Printf.printf "\n%!"

(** Standard formatter functions. Each takes a width and returns a function
    that formats the value into a string of that width. *)
let format_int w v = Printf.sprintf "%*d" w v
let format_float w v = Printf.sprintf "%*s" w (Printf.sprintf "%.1f%%" v)
let format_string_left w s = Printf.sprintf "%-*s" w s
let format_string_right w s = Printf.sprintf "%*s" w s
