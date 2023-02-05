let x = Vpdf.Lzw.compress ["A";"B";"B";"A";"B";"B";"B";"A";"B";"B";"A"] in
  List.iter (Printf.printf "%d ") (List.rev x)