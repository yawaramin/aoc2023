module Cubeset = struct
  type t = { red : int; green : int; blue : int }

  let add { red = r1; green = g1; blue = b1 }
      { red = r2; green = g2; blue = b2 } =
    { red = r1 + r2; green = g1 + g2; blue = b1 + b2 }

  let parse_join initial str =
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
        add initial next
    | _ -> assert false

  let of_string str =
    str |> String.split_on_char ','
    |> List.fold_left parse_join { red = 0; green = 0; blue = 0 }

  let leq { red = r1; green = g1; blue = b1 }
      { red = r2; green = g2; blue = b2 } =
    r1 <= r2 && g1 <= g2 && b1 <= b2

  let max { red = r1; green = g1; blue = b1 }
      { red = r2; green = g2; blue = b2 } =
    { red = max r1 r2; green = max g1 g2; blue = max b1 b2 }

  let power { red; green; blue } = red * green * blue
end

type game = { id : int; cubesets : Cubeset.t list }

let game_of_string str =
  match String.split_on_char ':' str with
  | [ game_id_str; cubesets_str ] ->
      Scanf.sscanf game_id_str "Game %d" (fun id ->
          {
            id;
            cubesets =
              cubesets_str |> String.split_on_char ';'
              |> List.map Cubeset.of_string;
          })
  | _ -> assert false

let possible_id leq_cubeset game_str =
  let { id; cubesets } = game_of_string game_str in
  if List.for_all (fun cubeset -> Cubeset.leq cubeset leq_cubeset) cubesets then
    id
  else 0

let real_cubeset = { Cubeset.red = 12; green = 13; blue = 14 }

(* 2593 *)
let part1 () =
  Lib.fold_file_lines "input"
    (fun sum line -> sum + possible_id real_cubeset line)
    0

let min_cubeset { cubesets; _ } =
  match cubesets with
  | cubeset :: cubesets -> List.fold_left Cubeset.max cubeset cubesets
  | [] -> assert false

let part2 () =
  Lib.fold_file_lines "input"
    (fun sum line -> sum + Cubeset.power (min_cubeset (game_of_string line)))
    0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
