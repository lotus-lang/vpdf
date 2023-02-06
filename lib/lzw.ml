(** Map that associates sequences of input characters (repr as strings) to compression codes. *)
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

(**
The encoder executes the following sequence of steps to generate each output code:
1. Accumulate a sequence of one or more input characters matching a sequence already present in the table. For maximum compression, the encoder looks for the longest such sequence.
2. Emit the code corresponding to that sequence.
3. Create a new table entry for the first unused code. Its value is the sequence found in step 1 followed by the next input character.
*)
let compress s =
  let eod = 257 in
  let clear_table = 256 in
  (* Codes are never longer than 12 bits; therefore, entry 4095 is the last entry of the LZW table. *)
  let max_card = 4095 in
  let m = Tbl.default in
  let rec helper tbl seq cur out =
    match Seq.uncons seq with
    | None -> eod :: Tbl.find cur tbl :: out
    | Some (h, t) -> (
        let ncur = cur ^ String.make 1 h in
        match Tbl.find_opt ncur tbl with
        | None ->
            (* Adds 3 since EOD marker is 257*)
            let ncode = Tbl.cardinal tbl + 3 in
            let ntbl =
              if ncode <= max_card then Tbl.add ncur ncode tbl else tbl
            in
            let nout = Tbl.find cur tbl :: out in
            helper ntbl t (String.make 1 h) nout
        | Some _ -> helper tbl t ncur out)
  in
  (* By convention, the encoder begins by issuing a clear-table code. *)
  helper m s "" [ clear_table ]
