
let y = List.map Char.chr [45;45;45;45;45;65;45;45;45;66] in
let x = Vpdf.Lzw.compress (String.to_seq (String.of_seq (List.to_seq y))) in
List.iter (Printf.printf "%d ") (List.rev x);
Printf.printf "\n"
