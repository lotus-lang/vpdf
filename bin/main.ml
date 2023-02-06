let x = Vpdf.Asciihex.compress (String.to_seq "baa") in
List.iter (Printf.printf "%s  ") x;
Printf.printf "\n"
