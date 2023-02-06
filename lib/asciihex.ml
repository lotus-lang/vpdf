let to_char x =
  match x with
  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> string_of_int x
  | 10 -> "A"
  | 11 -> "B"
  | 12 -> "C"
  | 13 -> "D"
  | 14 -> "E"
  | 15 -> "F"
  | _ -> failwith "Not in range"

let hex_of_int x =
  let rec helper x buf =
    let rem = x land 15 in
    match x - rem with
    | 0 -> to_char rem ^ buf
    | _ -> helper ((x - rem) lsr 4) (to_char rem ^ buf)
  in
  helper x ""

let compress (s : char Seq.t) =
  let rec helper s lst =
    match Seq.uncons s with
    | Some (x, xs) -> helper xs (hex_of_int (Char.code x) :: lst)
    | None -> lst
  in
  List.rev (helper s [])
