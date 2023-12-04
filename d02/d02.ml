type cubeset = { red : int; green : int; blue : int }
type game = { id : int; cubesets : cubeset list }

let cubeset_add { red = r1; green = g1; blue = b1 }
    { red = r2; green = g2; blue = b2 } =
  { red = r1 + r2; green = g1 + g2; blue = b1 + b2 }

let cubseset_leq { red = r1; green = g1; blue = b1 }
    { red = r2; green = g2; blue = b2 } =
  r1 <= r2 && g1 <= g2 && b1 <= b2

let cubeset_parse_join initial str =
  match String.split_on_char ' ' str with
  | [ ""; num_str; clr ] ->
      let num = int_of_string num_str in
      let next =
        match clr with
        | "red" -> { red = num; green = 0; blue = 0 }
        | "green" -> { red = 0; green = num; blue = 0 }
        | "blue" -> { red = 0; green = 0; blue = num }
        | _ -> assert false
      in
      cubeset_add initial next
  | _ -> assert false

let cubeset_of_string str =
  str |> String.split_on_char ','
  |> List.fold_left cubeset_parse_join { red = 0; green = 0; blue = 0 }

let game_of_string str =
  match String.split_on_char ':' str with
  | [ game_id_str; cubesets_str ] ->
      Scanf.sscanf game_id_str "Game %d" (fun id ->
          {
            id;
            cubesets =
              cubesets_str |> String.split_on_char ';'
              |> List.map cubeset_of_string;
          })
  | _ -> assert false

let possible_id leq_cubeset game_str =
  let { id; cubesets } = game_of_string game_str in
  if List.for_all (fun cubeset -> cubseset_leq cubeset leq_cubeset) cubesets
  then id
  else 0

let real_cubeset = { red = 12; green = 13; blue = 14 }

(* 2593 *)
let part1 () =
  Lib.fold_file_lines "input"
    (fun sum line -> sum + possible_id real_cubeset line)
    0

let () = Printf.printf "Part 1: %d\n" (part1 ())
