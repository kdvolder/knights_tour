type progress = {
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  pruned_nodes : int;
  progress_percent : float;
  estimated_remaining_seconds : float;
}

open Stochastic_estimator

type time_components = {
  years : float;
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let rec format_time (seconds : float) : string =
  if seconds < 0. then "-" ^ format_time (-.seconds)
  else
    let years_float = seconds /. 31536000. in
    if years_float > 1e9 then
      string_of_float years_float ^ " years"
    else
      let total = int_of_float seconds in
      let comps = {
        years = float_of_int (total / 31536000);
        days = (total mod 31536000) / 86400;
        hours = (total mod 86400) / 3600;
        minutes = (total mod 3600) / 60;
        seconds = total mod 60;
      } in
      let parts = ref [] in
      if comps.years > 0. then parts := (if comps.years = 1. then "1 year" else string_of_int (int_of_float comps.years) ^ " years") :: !parts;
      if comps.days > 0 then parts := (if comps.days = 1 then "1 day" else string_of_int comps.days ^ " days") :: !parts;
      if comps.hours > 0 then parts := (string_of_int comps.hours ^ " h") :: !parts;
      if comps.minutes > 0 then parts := (string_of_int comps.minutes ^ " min") :: !parts;
      if comps.seconds > 0 then parts := (string_of_int comps.seconds ^ " s") :: !parts;
      if !parts = [] then parts := ["0 s"];
      let result = ref "" in
      let sep = ref "" in
      List.iter (fun part ->
        if !sep = "" then result := part
        else result := !result ^ !sep ^ part;
        sep := if String.ends_with ~suffix:"day" part || String.ends_with ~suffix:"year" part then ", " else " "
      ) (List.rev !parts);
      !result

let make_progress (start_time : float) (est : 'a t) : progress =
  let now = Unix.gettimeofday () in
  let elapsed = now -. start_time in
  let ests = Stochastic_estimator.estimates est in
  let progress_percent =
    if ests.nodes > 0. then
      (float_of_int ests.materialized_nodes) /. ests.nodes *. 100.0
    else 0.0
  in
  let estimated_remaining =
    if progress_percent > 0. && progress_percent < 100. then
      elapsed *. (100.0 /. progress_percent) -. elapsed
    else if progress_percent >= 100. then
      0.0
    else
      Float.infinity
  in
  {
    elapsed_seconds = elapsed;
    total_nodes_estimate = ests.nodes;
    fails_estimate = ests.fails;
    solutions_estimate = ests.solutions;
    materialized_nodes = ests.materialized_nodes;
    pruned_nodes = get_pruned_nodes est;
    progress_percent;
    estimated_remaining_seconds = estimated_remaining;
  }

let default_progress_printer (p : progress) : unit =
  let eta_str =
    if p.progress_percent >= 100. then "done"
    else if Float.is_infinite p.estimated_remaining_seconds then "inf"
    else format_time p.estimated_remaining_seconds
  in
  Printf.printf "[%5.1f%%] materialized: %d, elapsed: %s, ETA: %s\n" p.progress_percent p.materialized_nodes (format_time p.elapsed_seconds) eta_str;
  flush stdout

let run_with_progress ?(batch_size = 100) ?(on_progress = default_progress_printer) (est : 'a t) : unit =
  let start_time = Unix.gettimeofday () in
  let rec loop () =
    if not (is_completed est) then (
      ignore (sample batch_size est);
      let p = make_progress start_time est in
      on_progress p;
      loop ()
    )
  in
  loop ();
  (* Final report when complete *)
  let p = make_progress start_time est in
  on_progress p

(** Progress Data Structure Tests *)

let%expect_test "make_progress initial state" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
  [%expect{|
    materialized: 1
    progress%: 100.0
  |}]
end

let%expect_test "make_progress after sampling" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  ignore (sample 10 est);
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
  [%expect{|
    materialized: 4
    progress%: 100.0
  |}]
end

(** Time Formatting Tests *)

let%expect_test "format_time: seconds" = begin
  Printf.printf "%s\n" (format_time 0.0);
  Printf.printf "%s\n" (format_time 5.0);
  Printf.printf "%s\n" (format_time 42.0);
  [%expect{|
    0 s
    5 s
    42 s
  |}]
end

let%expect_test "format_time: minutes and seconds" = begin
  Printf.printf "%s\n" (format_time 59.0);
  Printf.printf "%s\n" (format_time 60.0);
  Printf.printf "%s\n" (format_time 142.0);
  [%expect{|
    59 s
    1 min
    2 min 22 s
  |}]
end

let%expect_test "format_time: hours" = begin
  Printf.printf "%s\n" (format_time 3600.0);
  Printf.printf "%s\n" (format_time 7530.0);
  [%expect{|
    1 h
    2 h 5 min 30 s
  |}]
end

let%expect_test "format_time: days" = begin
  Printf.printf "%s\n" (format_time 86400.0);
  Printf.printf "%s\n" (format_time 150125.0);
  [%expect{|
    1 day
    1 day, 17 h 42 min 5 s
  |}]
end

let%expect_test "format_time: larger units" = begin
  Printf.printf "%s\n" (format_time 31536000.0); (* ~1 year *)
  [%expect{|
    1 year
  |}]
end

let%expect_test "format_time: astronomical years" = begin
  Printf.printf "%s\n" (format_time 1e20); (* ~3 billion years *)
  Printf.printf "%s\n" (format_time 1e30); (* ~31 trillion years *)
  Printf.printf "%s\n" (format_time 1e50); (* ~3.2e41 years *)
  [%expect{|
    3.17097919838e+12 years
    3.17097919838e+22 years
    3.17097919838e+42 years
  |}]
end

(** Reporter Integration Tests *)

let%expect_test "run_with_progress invokes callback after each batch" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:3 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Reports: %s\n" (String.concat ", " (List.map string_of_int !reports));
  [%expect{|
    Reports: 4, 4
  |}]
end

let%expect_test "run_with_progress uses default stdout printer" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  run_with_progress ~batch_size:3 est;
  [%expect{|
    [100.0%] materialized: 4, elapsed: 0 s, ETA: done
    [100.0%] materialized: 4, elapsed: 0 s, ETA: done
  |}]
end

let%expect_test "run_with_progress stops when complete" = begin
  let simple_tree = Searchspace.(
    alt [ return "sol"; empty ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:5 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Final materialized: %d\n" (List.hd (List.rev !reports));
  [%expect{|
    Final materialized: 3
  |}]
end
