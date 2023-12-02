let digit ch =
  match ch with '0' .. '9' -> Some (int_of_char ch - 48) | _ -> None

let pair (first, _) digit = (first, digit)

let calibration_value str =
  let digits = str |> String.to_seq |> Seq.filter_map digit in
  let first, last =
    match digits () with
    | Nil -> assert false
    | Cons (first, digits) -> Seq.fold_left pair (first, first) digits
  in
  (10 * first) + last

(* 55621 *)
let part1 () =
  Lib.fold_file_lines "input" (fun sum line -> sum + calibration_value line) 0

let digitize str =
  let len = String.length str in
  let buf = Buffer.create 16 in
  let idx = ref 0 in
  while !idx < len do
    let idx_val = !idx in
    let left = len - idx_val in
    let add digit ch =
      let len_digit = String.length digit in
      if left >= len_digit && String.sub str idx_val len_digit = digit then (
        Buffer.add_char buf ch;
        idx := idx_val + len_digit - 1;
        true)
      else false
    in
    if add "zero" '0' then ()
    else if add "one" '1' then ()
    else if add "two" '2' then ()
    else if add "three" '3' then ()
    else if add "four" '4' then ()
    else if add "five" '5' then ()
    else if add "six" '6' then ()
    else if add "seven" '7' then ()
    else if add "eight" '8' then ()
    else if add "nine" '9' then ()
    else if add "0" '0' then incr idx
    else if add "1" '1' then incr idx
    else if add "2" '2' then incr idx
    else if add "3" '3' then incr idx
    else if add "4" '4' then incr idx
    else if add "5" '5' then incr idx
    else if add "6" '6' then incr idx
    else if add "7" '7' then incr idx
    else if add "8" '8' then incr idx
    else if add "9" '9' then incr idx
    else incr idx
  done;
  Buffer.contents buf

(* 53592 *)
let part2 () =
  Lib.fold_file_lines "input"
    (fun sum line -> sum + calibration_value (digitize line))
    0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
