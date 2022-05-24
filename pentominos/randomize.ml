let rnd = Random.State.make_self_init ()

let list els =
  let arr = Array.of_list els in
  let sz = Array.length arr in
  for i = 0 to sz - 1 do
    let choices = sz - i in
    let pick = Random.State.int rnd choices in
    let tgt = sz - pick - 1 in
    let tmp = arr.(pick) in
    arr.(pick) <- arr.(tgt);
    arr.(tgt) <- tmp
  done;
  Array.to_list arr
