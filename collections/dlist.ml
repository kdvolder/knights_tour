module type S = Dlist_itf.S

let name = "dlist"

type 'a t = {front: 'a Clist.t; back: 'a Clist.t}

let to_string str {back;front} = Clist.(
  "[" ^ with_separator str "->" front ^ " | " ^ with_separator str "<-" (reverse back) ^ "]"   
)

let println msg it = Printf.printf "%s = %s\n" msg (to_string string_of_int it)

let%expect_test "to_string" =
  println "to_string" {
    front=Clist.of_list [1;2;3]; back=Clist.of_list [10;20;30]
  }
  ;[%expect{| to_string = [1->2->3 | 30<-20<-10] |}]

let empty = Clist.{front=empty;back=empty}
let%expect_test "empty" =
  println "empty" empty;
  [%expect{| empty = [ | ] |}]

let is_empty {front;back} = Clist.(is_empty front && is_empty back)

let singleton x = Clist.{front=singleton x; back=empty}
let%expect_test "singleton" =
  println "singleton" (singleton 42);
  [%expect{| singleton = [42 | ] |}]

let size {front;back} = Clist.(size front + size back)

let rec pop {front;back} = Clist.(
  if is_empty front then
    if is_empty back then
      None
    else
      let (front_of_back, back_of_back) = split back in
        assert(not (is_empty back_of_back));
        pop {front=reverse back_of_back;back=front_of_back}
  else (* front not empty *)
    let x = hd front in 
    let front = tl front in
    Some (x, {front; back})
)

let rec pop_end {front;back} = Clist.(
  if is_empty back then
    if is_empty front then
      None
    else
      let (front_of_front, back_of_front) = split front in
      assert( not (is_empty back_of_front));
      pop_end {front=front_of_front;back=reverse back_of_front}
  else (* back not empty *)
    let x = Clist.hd back in
    let xs = Clist.tl back in
    Some (x, {front;back=xs})
)

let map f {front;back} = Clist.{front=map f front;back=map f back}

let push x {front;back} = {front=Clist.cons x front;back}

let to_front {front;back} = Clist.(append front (reverse back))

let front_append xs {front;back} =
  Clist.{front=(to_front xs) ++ front; back}

let to_back {front;back} = Clist.(append back (reverse front))

let back_append {front;back} xs = 
  Clist.{front;back =(to_back xs) ++ back}

let append xs ys = 
  if size xs < size ys then
    front_append xs ys
  else
    back_append xs ys

let of_list xs = Clist.{front=of_list xs; back=empty}

let to_list {front;back} =
  Clist.to_list front @ (Clist.to_list (Clist.reverse back))

let%expect_test "to_list basic" =
  let d = append (of_list [1;2;3]) (of_list [10;20;30]) in
  let lst = to_list d in
  List.iter (fun x -> Printf.printf "%d " x) lst;
  [%expect {| 1 2 3 10 20 30 |}]



let%expect_test "append" =
   let it = append (of_list [1;2;3]) (of_list [4;5;6]) in
   println "start" it;
   let it = append it (map (fun x-> x+100) it) in
   println "append" it;
   [%expect{|
     start = [1->2->3 | 4<-5<-6]
     append = [1->2->3 | 4<-5<-6<-101<-102<-103<-104<-105<-106] |}]

let push_end x {front;back} = {front;back=Clist.cons x back}

let to_string str {back;front} = "[ " ^ 
    List.fold_left (fun x y -> x ^ " " ^ y) "| " (List.map str (Clist.to_list front)) ^
    List.fold_left (fun x y -> x ^ " " ^ y) "]" (List.rev (List.map str (Clist.to_list back)))

let%expect_test "empty list is empty" =
    Printf.printf "%B" (empty |> is_empty)
    ;[%expect{| true |}]

let pop_all popper it =
  let rec loop count it =
    assert(count<100);
    popper it |> (function
      | None -> 
        Printf.printf "Done!\n"
      | Some (x, rest) -> 
        Printf.printf "got: %d\n" x;
        println "rest" rest;
        loop (count+1) rest
    )
  in loop 0 it

let%expect_test "push and pop" =
    let it = empty |> push 1 |> push 2 |> push 3 in
    pop_all pop it
    ;[%expect{|
      got: 3
      rest = [2->1 | ]
      got: 2
      rest = [1 | ]
      got: 1
      rest = [ | ]
      Done! |}]

let%expect_test "push_end |> pop" = (
    let it = empty |> push_end 11 in
    println "it" it;
    pop it |> function
      | None -> Printf.printf "Nothing!\n"
      | Some (x, it) -> 
        Printf.printf "el = %d\n" x;
        println "rest" it
);[%expect{|
  it = [ | 11]
  el = 11
  rest = [ | ] |}]

let%expect_test "push |> pop" = (
    let it = empty |> push 1 in
    match pop it with
    | Some (x, rst) ->
      Printf.printf "x = %d\n" x;
      Printf.printf "rst is empty? %B\n" (is_empty rst)
    | None -> Printf.printf "Nothing!\n"
    )
    ;[%expect{|
      x = 1
      rst is empty? true |}]

let%expect_test "push pushend popall" =
    let it = empty 
        |> push 1 |> push 2 |> push 3
        |> push_end 11 |> push_end 12 |> push_end 13 |> push_end 14 |> push_end 15 in
    println "starting" it;
    pop_all pop it;
    [%expect{|
      starting = [3->2->1 | 11<-12<-13<-14<-15]
      got: 3
      rest = [2->1 | 11<-12<-13<-14<-15]
      got: 2
      rest = [1 | 11<-12<-13<-14<-15]
      got: 1
      rest = [ | 11<-12<-13<-14<-15]
      got: 11
      rest = [12->13 | 14<-15]
      got: 12
      rest = [13 | 14<-15]
      got: 13
      rest = [ | 14<-15]
      got: 14
      rest = [ | 15]
      got: 15
      rest = [ | ]
      Done! |}]

let%expect_test "push pushend pop_end_all" =
    let it = empty 
      |> push 1 |> push 2 |> push 3 |> push 4
      |> push_end 11 |> push_end 12 |> push_end 13 |> push_end 14 in
  println "starting" it;
  pop_all pop_end it;
  [%expect{|
    starting = [4->3->2->1 | 11<-12<-13<-14]
    got: 14
    rest = [4->3->2->1 | 11<-12<-13]
    got: 13
    rest = [4->3->2->1 | 11<-12]
    got: 12
    rest = [4->3->2->1 | 11]
    got: 11
    rest = [4->3->2->1 | ]
    got: 1
    rest = [4->3 | 2]
    got: 2
    rest = [4->3 | ]
    got: 3
    rest = [4 | ]
    got: 4
    rest = [ | ]
    Done! |}]
  

let%expect_test "singleton list" =
    let it = singleton 99 in
    Printf.printf "is_empty = %B\n" (it |> is_empty);
    Printf.printf "size = %d\n" (it |> size);
    Printf.printf "to_string = %s\n" (it |> to_string string_of_int)
    ;[%expect{|
      is_empty = false
      size = 1
      to_string = [ |  99] |}]

let get i ({front;back} as l) = 
   if i < Clist.size front then
      List.nth (Clist.to_list front) i
   else let from_end = (size l) - i - 1 in (
      if from_end < 0 then
        invalid_arg "Index out of range"
      else
        List.nth (Clist.to_list back) from_end
   )
  
let%expect_test "get" =
    let it = empty 
      |> push 1 |> push 2 |> push 3 |> push 4
      |> push_end 11 |> push_end 12 |> push_end 13 |> push_end 14 in
    for i = 0 to (size it) - 1 do
      Printf.printf "%d: %d\n" i (get i it)
    done
    ;[%expect{|
      0: 4
      1: 3
      2: 2
      3: 1
      4: 11
      5: 12
      6: 13
      7: 14 |}]