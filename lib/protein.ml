module Map = BatMap

let residue_map = [
  ("ALA","A");
  ("ARG","R");
  ("ASN","N");
  ("ASP","D");
  ("CYS","C");
  ("GLU","E");
  ("GLN","Q");
  ("GLY","G");
  ("HIS","H");
  ("ILE","I");
  ("LEU","L");
  ("LYS","K");
  ("MET","M");
  ("PHE","F");
  ("PRO","P");
  ("SER","S");
  ("THR","T");
  ("TRP","W");
  ("TYR","Y");
  ("VAL","V")]

let res_tr str =
  let cap str = String.capitalize (String.lowercase str) in 
  Util.fold (fun (abr, sym) str -> Pcre.replace ~pat:(cap abr) ~templ:sym str) 
            str residue_map

let lett_to_abr_tr str =
  let letters = Util.split str in
  let lookup = List.map (fun (a, b) -> (b, a)) residue_map in
  String.concat " " (List.map (Util.flip List.assoc lookup) letters)

(** returns a BatMap of uniprot_ac to sequence *)
let parse_fasta text =
  let lines = Pcre.split ~pat:"\n" text in
  let header line =
    Pcre.get_substring (Pcre.exec ~pat:"^>\\w+\\|\\w+\\|(\\w+) " line) 1 in
  let rec parse ac seq map = function
      line :: lines ->
        (match try Some (header line) with Not_found -> None with
            Some next_ac -> parse next_ac "" (Map.add ac seq map) lines
          | None -> parse ac (seq ^ line) map lines)
    | [] -> Map.add ac seq map
  in parse (header (List.hd lines)) "" Map.empty (List.tl lines)

(** takes an aligned sequence (eg "AST----MTS--") and calculates the 
    position of the [protein_index] AA in the alignment *)
let aligned_index protein_index seq =
  let rec index i p = 
    if String.get seq i = '-' then index (i + 1) p else
    if p = protein_index then i else index (i + 1) (p + 1)
  in index 0 0

let res res_name = res_tr
  (Pcre.get_substring (Pcre.exec ~pat:"([^0-9])[0-9]+$" res_name) 1)

let loc res_name = int_of_string 
  (Pcre.get_substring (Pcre.exec ~pat:"[^0-9]([0-9]+)$" res_name) 1) - 1
