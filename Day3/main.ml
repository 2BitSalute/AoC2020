open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

let read_input filename : (char array) array =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  List.map lines ~f:(fun line -> String.to_array line)
  |> List.to_array

let rec step
    ~count
    ~row
    ~col
    ~slope
    (a: (char array) array) : int =
  if row >= Array.length a then
    count
  else
    let (row_incr, col_incr) = slope in
    let width = Array.length a.(row) in
    let count = if Char.(=) a.(row).(col) '#' then 1 + count else count in
    let row = row + row_incr in
    let col = (col + col_incr) mod width in
    step ~count ~row ~col ~slope a

(* To run: `dune exec ./main.exe -- ./input` *)
let () =
  let input = Sys.argv.(1) in
  log "Filename: %s" input;

  let trees = read_input input in
  let count = step ~count:0 ~row:0 ~col:0 ~slope:(1, 3) trees in
  log "Part 1: %d" count;

  let slopes = [
    (1, 1);
    (1, 3);
    (1, 5);
    (1, 7);
    (2, 1);
  ]
  in
  let product =
    List.fold
      slopes
      ~init:1
      ~f:(fun product slope ->
        log "Slope (%d, %d)" (fst slope) (snd slope);
        product * step ~count:0 ~row:0 ~col:0 ~slope trees)
  in
  log "Part 2: %d" product