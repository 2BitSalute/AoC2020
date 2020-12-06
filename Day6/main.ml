open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

let read_input filename : string list =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  lines

let rec parse_groups ~(groups: (string list) list) ~(group: string list) (lines: string list) : ((string list) list) =
  match lines with
  | [] -> group :: groups
  | "" :: lines ->
    parse_groups ~groups:(group :: groups) ~group:[] lines
  | passenger :: lines ->
    parse_groups ~groups ~group:(passenger :: group) lines

let count_group_answers_part1 (group: string list) : int =
  let answers =
    List.fold
      group
      ~init:(Set.empty (module Char))
      ~f:(fun answers passenger ->
        let passenger = String.to_list passenger in
        List.fold passenger ~init:answers ~f:(fun answers answer -> Set.add answers answer))
  in
  Set.length answers

let count_group_answers_part2 (group: string list) : int =
  let answers =
    List.fold
      group
      ~init:(Map.empty (module Char))
      ~f:(fun answers passenger ->
        let passenger = String.to_list passenger in
        List.fold
          passenger
          ~init:answers
          ~f:(fun answers answer ->
            let count =
              match Map.find answers answer with
              | Some count -> count + 1
              | None -> 1
            in
            Map.set answers ~key:answer ~data:count))
  in
  Map.fold
    answers
    ~init:0
    ~f:(fun ~key ~data sum ->
      ignore key;
      if data = (List.length group) then sum + 1 else sum)

(*
  dune exec ./main.exe -- ./small_input
  dune exec ./main.exe -- ./input
*)
let () =
  let input = Sys.argv.(1) in
  log "Filename: %s" input;

  read_input input
  |> parse_groups ~groups:[] ~group:[]
  |> List.map ~f:(fun group -> count_group_answers_part1 group)
  |> List.fold ~init:0 ~f:(fun total count -> total + count)
  |> log "Part 1: %d";

  read_input input
  |> parse_groups ~groups:[] ~group:[]
  |> List.map ~f:(fun group -> count_group_answers_part2 group)
  |> List.fold ~init:0 ~f:(fun total count -> total + count)
  |> log "Part 2: %d";