open Pentominos

let donut = Board.of_string "
      122456789012345    
      2##############
      3##############
      4##############
      5##############
      6##############
      7####.....12345
      8####.....#####
      9####.....#####
      10#############
      11#############
      12#############
      13#############
      14#############
      15#############
      " 

let out_file = "polymino-puzzle.txt"

let save_file path puzzle =
  Out_channel.with_open_text path (fun out -> 
    Puzzle.save out puzzle 
  )

(** Generates a hexo puzzle, with randomized puzzle piece ordering and writes it out into a file called
    'puzzle.txt' in current directory*)
let () = 
  if Sys.file_exists out_file then (
    Format.printf "The file '%s' already exists. Refusing to overwrite it" out_file;
    exit 99
  ) else
    Puzzle.{
      pieces = Polyomino.of_order 6 |> Randomize.list;
      board = donut
    } |> save_file out_file
