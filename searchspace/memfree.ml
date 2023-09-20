let meminfo_file = "/proc/meminfo"

let parse line = Scanf.sscanf line "%s@: %d %s" (fun k v _ -> (k, v))

let meminfo () = 
  let info = Hashtbl.create 20 in
  In_channel.with_open_text meminfo_file (fun input ->
    (try (
      while true do
        try (
          let (k, v) = parse (input_line input) in 
          Hashtbl.add info k v
        ) with
        | Scanf.Scan_failure msg -> print_endline msg
      done
    ) with
    | End_of_file -> ()
    );
    info
  )

let mem_free_ratio () =
  let info = meminfo () in
  let available = Hashtbl.find info "MemAvailable" |> Float.of_int in
  let total = Hashtbl.find info "MemTotal" |> Float.of_int in
  available /. total
