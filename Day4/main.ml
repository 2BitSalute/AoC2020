open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

let read_input filename : string list =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  lines

type doc = {
  byr: string option;
  iyr: string option;
  eyr: string option;
  hgt: string option;
  hcl: string option;
  ecl: string option;
  pid: string option;
  cid: string option;
}

let default : doc = {
  byr = None;
  iyr = None;
  eyr = None;
  hgt = None;
  hcl = None;
  ecl = None;
  pid = None;
  cid = None;
}

let parse_group (group: string) (doc: doc) : doc =
  let tokens = String.split ~on:':' group in
  assert (List.length tokens = 2);
  let (tag, value) = (List.nth_exn tokens 0, List.nth_exn tokens 1) in
  let value = Some value in
  match tag with
  | "byr" -> { doc with byr = value }
  | "iyr" -> { doc with iyr = value }
  | "eyr" -> { doc with eyr = value }
  | "hgt" -> { doc with hgt = value }
  | "hcl" -> { doc with hcl = value }
  | "ecl" -> { doc with ecl = value }
  | "pid" -> { doc with pid = value }
  | "cid" -> { doc with cid = value }
  | other -> failwith (Printf.sprintf "Unrecognized tag: %s" other)
  
let parse (line: string) (doc: doc) : doc =
  let groups = String.split ~on:' ' line in

  List.fold
    groups
    ~init:doc
    ~f:(fun doc group -> parse_group group doc)

let rec docs_of_lines
    (acc: doc list)
    (lines: string list)
    (curr: doc)  : doc list =
  match lines with
  | [] -> curr :: acc
  | line :: lines ->
    let (curr, acc) =
      if String.(=) line "" then
        (default, curr :: acc)
      else
        (parse line curr, acc)
    in
    docs_of_lines acc lines curr

let is_valid (doc: doc) : unit option =
  let open Option.Monad_infix in
  doc.pid >>= fun _pid ->
  doc.ecl >>= fun _ecl ->
  doc.hcl >>= fun _hcl ->
  doc.hgt >>= fun _hgt ->
  doc.eyr >>= fun _eyr ->
  doc.iyr >>= fun _iyr ->
  doc.byr >>= fun _byr ->
  Some ()

let is_valid_part1 (doc: doc) : int =
  match is_valid doc with
  | Some () -> 1
  | None -> 0

let is_valid (doc: doc) : unit option =
  let safe (f: unit -> unit) : unit option =
    try Some (f ())
    with e ->
      log "%s" (Exn.to_string e);
      None
  in
  let validate ~pat s : unit option =
    safe (fun () -> ignore (Pcre.exec ~pat s))
  in
  let validate_height s : unit option =
    safe (fun () ->
      let matches = Pcre.exec ~pat:"^(\\d+)(cm|in)$" s in
      match (Int.of_string (Pcre.get_substring matches 1), Pcre.get_substring matches 2) with
      | (h, "cm") -> if h < 150 || h > 193 then failwith (Printf.sprintf "Invalid height: %dcm" h)
      | (h, "in") -> if h < 59 || h > 76 then failwith (Printf.sprintf "Invalid height: %din" h)
      | (_h, other) -> failwith (Printf.sprintf "Unknown value: %s" other)
    )
  in
  let validate_year s min max : unit option =
    safe (fun () ->
      ignore (Pcre.exec ~pat:"^\\d{4}$" s);
      let year = Int.of_string s in
      if year < min || year > max then
        failwith (Printf.sprintf "Invalid year: %d" year)
    )
  in
  let open Option.Monad_infix in
  doc.pid
  >>= fun pid -> validate ~pat:"^\\d{9}$" pid
  >>= fun () -> doc.ecl
  >>= fun ecl -> validate ~pat:"^(amb|blu|brn|gry|grn|hzl|oth)$" ecl
  >>= fun () -> doc.hcl
  >>= fun hcl -> validate ~pat:"^#[0-9a-f]{6}$" hcl
  >>= fun () -> doc.hgt
  >>= fun hgt -> validate_height hgt
  >>= fun () -> doc.eyr
  >>= fun eyr -> validate_year eyr 2020 2030
  >>= fun () -> doc.iyr
  >>= fun iyr -> validate_year iyr 2010 2020
  >>= fun () -> doc.byr
  >>= fun byr -> validate_year byr 1920 2002
  >>= fun () -> Some ()

let is_valid_part2 (doc: doc) : int =
  match is_valid doc with
  | Some () -> 1
  | None -> 0

(*
  dune exec ./main.exe -- ./small_input
  dune exec ./main.exe -- ./input
*)
let () =
  let input = Sys.argv.(1) in
  log "Filename: %s" input;

  let lines = read_input input in
  let docs = docs_of_lines [] lines default in

  let count =
    List.fold
      docs
      ~init:0
      ~f:(fun acc doc -> acc + is_valid_part1 doc)
  in
  log "Part 1: %d" count;

  let count =
    List.fold
      docs
      ~init:0
      ~f:(fun acc doc -> acc + is_valid_part2 doc)
  in
  log "Part 2: %d" count