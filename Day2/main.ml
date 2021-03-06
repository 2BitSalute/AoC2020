open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

type policy = {
  letter: char;
  min: int;
  max: int;
} [@@deriving show]

type t = {
  policy: policy;
  password: string;
} [@@deriving show]

let make line : t =
  let line_channel = Scanf.Scanning.from_string line in
  let f min max letter password = {
    policy = {
      letter;
      min;
      max
    };
    password;
  }
  in
  Scanf.bscanf line_channel "%d-%d %c: %s" f

let read_input filename : t list =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  List.map lines ~f:make

let valid_part1 (t: t) : int =
  let chars = String.to_list t.password in
  let count = List.count chars ~f:(fun (c: char) -> Char.(=) c t.policy.letter) in
  if count >= t.policy.min && count <= t.policy.max then
    1
  else
    0

let valid_part2 (t: t) : int =
  let chars = String.to_array t.password in
  let length = Array.length chars in
  let first = t.policy.min - 1 in
  let last = t.policy.max - 1 in
  let c = t.policy.letter in
  if first >= length then
    0
  else if last >= length then
    if Char.(=) chars.(first) c then
      1
    else
      0
  else
    if (Char.(=) chars.(first) c && Char.(<>) chars.(last) c) ||
       (Char.(=) chars.(last) c && Char.(<>) chars.(first) c) then
      1
    else
      0

let count_valid_passwords l valid : int =
  List.fold l ~init:0 ~f:(fun acc t -> acc + (valid t))

(* To run: `dune exec ./main.exe -- ./input` *)
let () =
  let input = Sys.argv.(1) in
  log "Filename: %s\n%!" input;
  let l = read_input input in
  let count = count_valid_passwords l valid_part1 in
  log "Part 1: %d" count;
  let count = count_valid_passwords l valid_part2 in
  log "Part 2: %d" count
