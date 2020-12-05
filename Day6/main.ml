open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

let read_input filename : string list =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  lines

(*
  dune exec ./main.exe -- ./small_input
  dune exec ./main.exe -- ./input
*)
let () =
  let input = Sys.argv.(1) in
  log "Filename: %s" input;

  read_input input |> ignore