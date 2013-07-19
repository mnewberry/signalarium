(** Pass this program a list of uniprot IDs *)
open Util
module I = Interval

let set_check ref v =
  if !ref = None then ref := Some v
  else raise (Arg.Bad "Only one argument is supported")
let proteins = ref SS.empty and filename = ref None
let set_protein_ids v = proteins := SS.add v !proteins
let usage = "Usage: prism_pdb_ids -f pdb_chain_uniprot.lst <uniprot list>"
let argspec = ["-f", Arg.String (set_check filename), "pdb_chain_uniprot.lst"]

let argreq z = match !z with Some x -> x | _ -> Arg.usage argspec usage; exit 1
let () = Arg.parse argspec set_protein_ids usage

let import siftsfn =
  let rec collect map ch =
    match (try Some (input_line ch) with End_of_file -> None) with
        Some ln ->
          let lst = Pcre.split ~pat:"\t" ln in
          let pdbid = List.nth lst 0 in
          let chain = List.nth lst 1 in
          let uac = List.nth lst 2 in
          let seqsta = int_of_string (List.nth lst 8) in
          let seqend = int_of_string (List.nth lst 9) in
          collect (SM.modify_def (Some (uac, [])) pdbid
              (function Some (_, ivl) ->
                    if chain = "A" 
                    then Some (uac, I.union ivl (I.mk seqsta seqend)) 
                    else None
                | _ -> None)
              map) ch
      |  None -> map in
  let read map ch = ignore (input_line ch) ; collect map ch in 
  let pdbs = smcoll (call_with_in_fh siftsfn (read SM.empty)) in
  let uacs = SM.fold 
    (fun pdbid (uac, ivl) map ->
      SM.modify_def SM.empty uac (fun sm -> SM.add pdbid ivl sm) map)
    pdbs SM.empty in
  uacs (* a uac to (pdb to interval map) map *)

let nonint_subset pm =
  let ids = sort ~cmp:(cmp_w (fun id -> -(I.length (SM.find id pm)))) 
              (smkeys pm) in
  let neighbors = fold
    (fun id map ->
      SM.add id
        (ssofls (grep (fun idb ->
            idb <> id && I.overlap (SM.find id pm) (SM.find idb pm)) ids))
        map)
    SM.empty ids in
  let rec greedy_subset acc = function
      [] -> acc
    | (hd :: tl) -> 
        let ns = SM.find hd neighbors in
        greedy_subset (hd :: acc) (grep (fun id -> not (SS.mem id ns)) tl) in
  greedy_subset [] ids

let uacs = import (argreq filename)
let () = SS.iter
  (fun ac ->
    let pm = try Some (SM.find ac uacs) with Not_found -> None in
    if opt_some pm 
    then Printf.printf "%s %s\n" ac (joinsp (nonint_subset (opt_req pm))))
  !proteins
