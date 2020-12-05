open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

let read_input filename : string list =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  lines

let small_input = "FBFBBFFRLR"

(* Rows 0-127, columns 0-7 *)

type t = {
  row: int;
  col: int;
  id: int;
}

let rec bin_search (spec: char list) ~(low: int) ~(high: int) : int =
  match spec with
  | [] ->
    log "FINALLY %d - %d" low high;
    low
  | 'L' :: spec ->
    let high = low + (high - low) / 2 in
    log "%c: Keeping %d - %d" 'L' low high;
    bin_search spec ~low ~high
  | 'R' :: spec ->
    let low = low + (high - low + 1) / 2 in
    log "%c: Keeping %d - %d" 'R' low high;
    bin_search spec ~low ~high
  | direction :: _spec ->
    failwith (Printf.sprintf "Unrecognized direction: %c" direction) 

let map_fb_to_lr (direction: char) : char =
  match direction with
  | 'F' -> 'L'
  | 'B' -> 'R'
  | _ -> failwith (Printf.sprintf "Unrecognized direction: %c" direction)

let parse_seat (raw: string) : t =
  let row_spec = String.sub raw ~pos:0 ~len:7
    |> String.to_list
    |> List.map ~f:map_fb_to_lr
  in
  log "row spec: %s raw: %s" (String.sub raw ~pos:0 ~len:7) raw;
  let col_spec = String.sub raw ~pos:7 ~len:3 |> String.to_list in
  log "col spec: %s" (String.sub raw ~pos:7 ~len:3);
  let row = bin_search row_spec ~low:0 ~high:127 in
  log "row: %d" row;
  let col = bin_search col_spec ~low:0 ~high:7 in
  log "col: %d" col;
  {
    row;
    col;
    id = row * 8 + col;
  }

(*
  dune exec ./main.exe -- ./small_input
  dune exec ./main.exe -- ./input
*)
let () =
  let input = Sys.argv.(1) in
  log "Filename: %s" input;

  let seats = read_input input |> List.map ~f:parse_seat in

  let part1 =
    seats
    |> List.fold
      ~init:(-100)
      ~f:(fun max curr -> if curr.id > max then curr.id else max)
  in
  log "Part 1: %d" part1;

  let sorted =
    List.sort
      seats
      ~compare:(fun x y -> Int.compare x.id y.id)
  in
  
  let (init, sorted) = List.split_n sorted 1 in
  let init =
    let init = List.nth init 0 in
    Option.value_exn init
  in
  List.fold
    sorted
    ~init
    ~f:(fun prev curr ->
      if curr.id - prev.id > 1 then
        failwith (Printf.sprintf "Part 2: %d" (curr.id - 1))
      else
        curr)
  |> ignore