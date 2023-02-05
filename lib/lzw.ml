module Tbl = struct 
  include Map.Make(String)

  let default = 
    let e = empty in
    let l = List.init 255 (fun x -> x + 1) in
    let rec create tbl lst = match lst with
      | h :: t -> create (add (String.make 1 (Char.chr h)) h tbl) t
      | [] -> tbl
  in create e l
end

let compress seq =
    let m = Tbl.default in
    let rec helper tbl lst cur out = match lst with
      | [] -> (Tbl.find cur tbl) :: out
      | h :: t -> 
        let ncur = cur ^ h in match (Tbl.find_opt ncur tbl) with
          | None -> 
            let ntbl = Tbl.add ncur ((Tbl.cardinal tbl) + 1) tbl in
            let nout = (Tbl.find cur tbl) :: out in
            helper ntbl t h nout
          | Some _ -> helper tbl t ncur out
    in helper m seq "" []


