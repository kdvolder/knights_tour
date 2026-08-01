(** Generic table logger with automatic column alignment.

    Each column is defined by a label and an [extract_and_format] function
    that takes the row data and column width, returning a formatted string.
    Column widths are determined by the label length. *)

type 'a column = {
  label : string;
  extract_and_format : int -> 'a -> string;
}

type 'a t = 'a column list

(** [add_column ~label ~extract_and_format cols] appends a column.
    Column width equals the length of [label]. *)
val add_column :
  label:string ->
  extract_and_format:(int -> 'a -> string) ->
  'a t -> 'a t

(** [print_header cols] prints the header row and a separator line. *)
val print_header : 'a t -> unit

(** [print_row cols row] prints a single data row, auto-aligned. *)
val print_row : 'a t -> 'a -> unit
