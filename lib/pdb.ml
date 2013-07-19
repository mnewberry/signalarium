open Util

let sub = String.sub
let stripws = Pcre.replace

type dbref = { chain : string ; 
  db : string ; ac : string ;
  seq_begin : int ; seq_end : int ; 
  dbseq_begin : int ; dbseq_end : int }

let line_to_dbref l = { 
  chain = sub l 12 1 ; 
  seq_begin = int_of_string (stripws (sub l 14 4)) ;
  seq_end = int_of_string (stripws (sub l 20 4)) ;
  db = stripws (sub l 26 6) ;
  ac = stripws (sub l 33 8) ;
  dbseq_begin = int_of_string (stripws (sub l 55 5)) ;
  dbseq_end = int_of_string (stripws (sub l 62 5)) }

let section sec pdb = 
  grep (fun l -> sub l 0 (String.length sec) = sec) (splitnl pdb)

let dbrefs pdb = map line_to_dbref (section "DBREF" pdb)

let uniprot_refs pdb = grep (fun r -> r.db = "UNP") (dbrefs pdb)

let sequences pdb =
  let chain l = sub l 11 1 in
  let lines = section "SEQRES" pdb in
  let chains = nub (map chain lines) in
  let seq ls ch = 
    let ls = grep (fun l -> chain l = ch) ls in
    let ress = catmap (fun l -> splitws (sub l 19 51)) ls in
    let rs = map (fun res -> opt_def "?" (assoc res Protein.residue_map)) ress in
    String.concat "" rs
  in
  map (fun ch -> (ch, seq lines ch)) chains
