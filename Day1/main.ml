open Core_kernel

let log x =
  Printf.printf (x ^^ "\n%!")

let read_set filename : (int, Int.comparator_witness) Set.t =
  let file = In_channel.create filename in
  let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
  In_channel.close file;
  List.fold
    ~init:(Set.empty (module Int))
    ~f:(fun acc num -> Set.add acc num)
    numbers

let () =
  let input = Sys.argv.(1) in
  log "Filename: %s\n%!" input;

  let set = read_set input in

  (* Part 1 *)
  Set.iter
    set
    ~f:(fun first ->
      let second = 2020 - first in
      if Set.mem set second then
        log "First: %d Second: %d Product: %d" first second (first * second));

  (* Part 2 *)
  Set.iter
    set
    ~f:(fun first ->
      (* Let's assume the numbers are unique *)
      Set.iter
        set
        ~f:(fun second ->
          let third = 2020 - (first + second) in
          if Set.mem set third then
            log "First: %d Second: %d Third: %d Product: %d" first second third (first * second * third)
        )
    )
