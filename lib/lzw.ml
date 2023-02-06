module Tbl = struct
  include Map.Make (String)

  let default =
    let e = empty in
    let l = List.init 255 (fun x -> x + 1) in
    let rec create tbl lst =
      match lst with
      | h :: t -> create (add (String.make 1 (Char.chr h)) h tbl) t
      | [] -> tbl
    in
    create e l
end

let compress s =
  let eod = 257 in
  let clear_table = 256 in
  let maxCard = 4095 in
  let m = Tbl.default in
  let rec helper tbl seq cur out =
    match Seq.uncons seq with
    | None -> eod :: (Tbl.find cur tbl :: out)
    | Some (h, t) -> (
        let ncur = cur ^ String.make 1 h in
        match Tbl.find_opt ncur tbl with
        | None ->
            (* Adds 3 since EOD marker is 257*)
            let ncode = Tbl.cardinal tbl + 3 in
            let ntbl =
              if ncode <= maxCard then Tbl.add ncur ncode tbl else tbl
            in
            let nout = Tbl.find cur tbl :: out in
            helper ntbl t (String.make 1 h) nout
        | Some _ -> helper tbl t ncur out)
  in
  (* By convention, the encoder begins by issuing a clear-table code. *)
  helper m s "" [clear_table]

