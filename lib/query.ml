open Db_sql
open Util
open Http
open Http_client.Convenience
module I = Interval


(** {7 Queries} *)

let uniprot
    ?get_xml:(get_xml = retrieve_url "http://www.uniprot.org/uniprot/%s.xml")
    ?obj:(obj = None)
    db name =
  if not (Pcre.pmatch ~pat:"_" name)
  then invalid_arg (pr "%s is not a uniprot name" name) else
  let retrieve name =
    let xml = get_xml name in
    let z = opt_def_f (fun () -> Xml.of_str xml) obj in (*avoid parsing twice*)
    let acs = map (fun x->x#data) (Xml.coll (Xml.cnamed "accession") z#root) in
    iter (fun ac -> ll_put db "un" ac name) acs ;
    ll_put db "uacs" name (joinsp acs) ;
    xml
  in memcache db.cache_u name (fun () -> Xml.of_str (get db retrieve "u" name))

let uniprot_name db ac = 
  let retrieve ac =
    let xml = (retrieve_url "http://www.uniprot.org/uniprot/%s.xml") ac in
    let obj = Xml.of_str xml in
    let name = (Xml.find (Xml.cnamed "name") obj#root)#data in
    ignore (uniprot ~get_xml:(fun _ -> xml) ~obj:(Some obj) db name) ;
    name
  in get db retrieve "un" ac

let uniprot_acs db name = 
  let retrieve name = ignore (uniprot db name) ; ll_get db "uacs" name in 
  splitws (get db retrieve "uacs" name)

let uniprot_dataset db name = 
  let retrieve name =
    let z = uniprot db name in
    Xml.sval ((Xml.find (Xml.cnamed "entry") z#root)#attribute "dataset")
  in get db retrieve "us" name

let uniprot_is_swiss db name = (uniprot_dataset db name) = "Swiss-Prot"

let uniprot_fasta db name =
  get db (retrieve_url "http://www.uniprot.org/uniprot/%s.fasta") "uf" name

let uniprot_sequence db name =
  let up = uniprot db name in
  let l = Xml.coll (Xml.cand Xml.chasdata (Xml.cnamed "sequence")) up#root in
  purge_ws (List.hd l)#data

let pdb_desc db id =
  Xml.of_str (get db (retrieve_url "http://www.rcsb.org/pdb/rest/describeMol?structureId=%s") "pd" id)

let pdb_record db id =
  get db (retrieve_url "http://www.pdb.org/pdb/download/downloadFile.do?fileFormat=pdb&compression=NO&structureId=%s") "pr" id

let pdb_uniprot_indices db id =
  let pdb = pdb_record db id in
  let dbrefs = Pdb.uniprot_refs pdb in
  let kons ref al = 
    let un = uniprot_name db (ref.Pdb.ac) in
    let seq = uniprot_sequence db un in
    let ssq = opt_req (assoc (ref.Pdb.chain) (Pdb.sequences pdb)) in
    (un, (try Some (BatString.find seq ssq) with Not_found -> None),
         ref.Pdb.seq_begin, ref.Pdb.dbseq_begin) :: al
  in fold kons [] dbrefs

let pdb_chains db id = 
  let pdb = pdb_record db id in
  let chains = fold (fun dbr cs -> SS.add (dbr.Pdb.chain) cs) 
    SS.empty (Pdb.dbrefs pdb) in
  sskeys chains

let pdb_uniprot_acs db id = 
  let pdb = pdb_record db id in
  let chains = fold (fun dbr cs -> SS.add (dbr.Pdb.ac) cs) 
    SS.empty (Pdb.uniprot_refs pdb) in
  sskeys chains

let pdb_uniprot_interval db id =
  let pdb = pdb_record db id in
  let dbrefs = Pdb.uniprot_refs pdb in
  fold (fun dbr iv -> 
         I.union iv (I.mk dbr.Pdb.dbseq_begin dbr.Pdb.dbseq_end)) 
    I.empty dbrefs

let uniprot_pdb_ids db name =
  let retrieve name =
    let up = uniprot db name in
    Xml.coll (Xml.cand (Xml.cnamed "dbReference") (Xml.cattr "type" ["PDB"]))
      up#root
    |> catmap (fun el -> opt_def [] (Xml.attr "id" el)) |> joinsp
  in splitws (get db retrieve "up" name)

let uniprot_nonoverlapping_pdb db name =
  let pdbids = uniprot_pdb_ids db name in
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
          greedy_subset (hd::acc) (grep (fun id -> not (SS.mem id ns)) tl) in
    greedy_subset [] ids
  in 
  nonint_subset
    (fold (fun id pm ->
            if List.length (pdb_uniprot_acs db id) = 1
            then SM.add id (pdb_uniprot_interval db id) pm
            else pm)
      SM.empty pdbids)

let kp_wnt = "ko04310"

let kegg_pathway db ko =
  let kgml = get db 
   (retrieve_url "http://www.genome.jp/kegg-bin/download?entry=%s&format=kgml")
    "kp" ko in
  let pathway = 
    Xml.of_str (Pcre.replace ~pat:"<!DOCTYPE [^>]*>" ~templ:"" kgml) in
  pathway

let kegg_pathway_orthologies db kp =
  let retrieve kp = 
    let pathway = kegg_pathway db kp in
    Xml.coll (Xml.cand (Xml.cnamed "entry") (Xml.cattr "type" ["ortholog"])) 
      pathway#root 
    |> (catmap (fun el -> catmap splitws (opt_def [] (Xml.attr "name" el))))
    |> map (Pcre.replace ~pat:"ko:" ~templ:"") |> nub |> joinsp
  in splitws (get db retrieve "kpo" kp)

let kegg_orthology_name db ko =
  let retrieve ko =
    let pg = http_get
      ("http://www.genome.jp/dbget-bin/www_bget?"^ko) in
    Pcre.get_substring 
      (Pcre.exec ~pat:(">Name</[^>]*></th>\\s*<td [^>]*><div [^>]*>([\\w]+)[,<]")
        pg) 1
  in get db retrieve "kon" ko
  
let kegg_orthology_uniprot_acs db ko =
  let retrieve ko =
    let name = kegg_orthology_name db ko in
    assert_eq name (get db (constant name) "kon" ko) ; 
    assert_eq ko (get db (constant ko) "koi" name) ;
    let page = http_get 
      ("http://www.genome.jp/dbget-bin/get_linkdb?-t+uniprot+ko:"^ko) in
    map (fun m -> Pcre.get_substring m 1)
      (Array.to_list
        (try
          print_endline ko ;
          Pcre.exec_all ~pat:"\"/dbget-bin/www_bget\\?uniprot:([^\"]+)\"" page
        with Not_found -> [||]))
    |> List.filter ((<>) "A8WWM9") (* Kegg still lists a deleted uniprot ID *)
    |> joinsp
  in splitws (get db retrieve "kou" ko)

let kegg_orthology_uniprot_names db ko =
  nub (map (uniprot_name db) (kegg_orthology_uniprot_acs db ko))

let kegg_orthology_pdb_ids db ko = 
  catmap (uniprot_pdb_ids db) (kegg_orthology_uniprot_names db ko) |> nub

let kegg_pathway_pdb_ids db kp = 
  catmap (kegg_orthology_pdb_ids db) (kegg_pathway_orthologies db kp) |> nub

let kegg_pathway_uniprot_names db kp =
  let retrieve kp = 
    catmap (kegg_orthology_uniprot_names db) (kegg_pathway_orthologies db kp)
      |> joinsp
  in splitws (get db retrieve "kpu" kp)

let kegg_orthology_fasta db ko = String.concat ""
  (map (uniprot_fasta db) 
    (grep (uniprot_is_swiss db) (kegg_orthology_uniprot_names db ko)))

(* Uses Clustal omega *)
let kegg_orthology_msa db ko =
  let retrieve ko = 
    let fasta = kegg_orthology_fasta db ko in
    if Map.cardinal (Pt.parse_fasta fasta) <= 1 then fasta else
    request_embl
      (fun _ ->
        http_post "http://www.ebi.ac.uk/Tools/services/rest/clustalo/run/"
          [("email", "debugfl@mailinator.com");
           ("outfmt", "fa");
           ("sequence", fasta)])
      (fun resp ->
        http_get (pr
"http://www.ebi.ac.uk/Tools/services/rest/clustalo/result/%s/aln-fasta" resp))
  in get db retrieve "kom" ko

let uniprot_pdb_complexes db names =
  duplicates (catmap (uniprot_pdb_ids db) (nub names))

let uniprot_iprscan db name =
  let retrieve name =
    let fasta = uniprot_fasta db name in
    request_embl 
      (fun _ ->
        Printf.printf "connected\n%!" ;
        http_post "http://www.ebi.ac.uk/Tools/services/rest/iprscan/run/"
          [("email", "debugfl@mailinator.com");("sequence", fasta)])
      (fun resp -> 
Printf.printf "Got %s\n%!" resp ;
http_get (pr 
"http://www.ebi.ac.uk/Tools/services/rest/iprscan/result/%s/xml" resp))
  in Xml.of_str (get db retrieve "uis" name)

type interpro = { protein_accession : string ; method_id : string; 
  method_name : string ; method_database_name : string ; pos_from : int ; 
  pos_to : int ; start : int ; length : int ; match_status : string ; 
  match_score : float ; entry_ac : string ; entry_short_name : string ; 
  entry_name : string ; entry_type : string }

let interpro_of_list l = 
  let pos_from = int_of_string (List.nth l 4) in 
  let pos_to = int_of_string (List.nth l 5) in {
		protein_accession = List.nth l 0 ;
		method_id = List.nth l 1 ;
		method_name = List.nth l 2 ;
		method_database_name = List.nth l 3 ;
		pos_from = pos_from ;
		pos_to = pos_to ;
    start = pos_from - 1 ;
    length = pos_to - pos_from + 1 ;
		match_status = List.nth l 6 ;
		match_score = float_of_string (List.nth l 7) ;
		entry_ac = List.nth l 8 ;
		entry_short_name = List.nth l 9 ;
		entry_name = List.nth l 10 ;
		entry_type = List.nth l 11 }

let interpros_of_tsv = of_tsv interpro_of_list

let interpro_align seq dom = 
  let start = Pt.aligned_index (dom.pos_from - 1) seq in
  let newend = Pt.aligned_index (dom.pos_to - 1) seq in
  {dom with start = start ; length = newend - start + 1 }

let uniprot_interpro db name =
  let query ac = (pr "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE Query>
<Query virtualSchemaName=\"default\" formatter=\"TSV\" header=\"0\" 
    uniqueRows=\"1\" count=\"\" datasetConfigVersion=\"0.6\" >
	<Dataset name=\"protein\" interface=\"default\" >
		<Filter name=\"protein_name\" value=\"%s\"/>
		<Filter name=\"method_database_name\" value=\"Pfam\"/>
		<Filter name=\"entry_type\" 
       value=\"Active_site,Binding_site,Conserved_site,Domain,PTM,Repeat\"/>
		<Attribute name=\"protein_accession\" />
		<Attribute name=\"method_id\" />
		<Attribute name=\"method_name\" />
		<Attribute name=\"method_database_name\" />
		<Attribute name=\"pos_from\" />
		<Attribute name=\"pos_to\" />
		<Attribute name=\"match_status\" />
		<Attribute name=\"match_score\" />
		<Attribute name=\"entry_ac\" />
		<Attribute name=\"entry_short_name\" />
		<Attribute name=\"entry_name\" />
		<Attribute name=\"entry_type\" />
	</Dataset>
</Query>" ac) in 
  let retrieve ac = retrieve_post 
    "http://www.ebi.ac.uk/interpro/biomart/martservice" [("query", query name)]
  in interpros_of_tsv (get db retrieve "uip" name)

type phosphosite = { name : string ; ac : string ; gene_symbol : string ;
  chr_loc_hum : string ;
  mod_type : string ; rsd : string ; site_grp_id : string ; species : string ;
  mw (* In kD *) : float ; in_domain : string ; modsite_seq : string ;
  pubmed_ltp : string ; pubmed_ms2 : string ; cst_ms2 : string ;
  cst_catn : string }

let phosphosite_of_list l = {
  name = List.nth l 0 ;
  ac = List.nth l 1 ;
  gene_symbol = List.nth l 2 ;
  chr_loc_hum = List.nth l 3 ;
  mod_type = List.nth l 3 ;
  rsd = List.nth l 5 ;
  site_grp_id = List.nth l 6 ;
  species = List.nth l 7 ;
  mw (* In kD *) = float_of_string (List.nth l 8) ;
  in_domain = List.nth l 9 ;
  modsite_seq = List.nth l 10 ;
  pubmed_ltp = List.nth l 11 ;
  pubmed_ms2 = (try List.nth l 12 with Failure _ -> "") ;
  cst_ms2 = (try List.nth l 13 with Failure _ -> "") ;
  cst_catn = (try List.nth l 14 with Failure _ -> "") }

let phosphosites_of_tsv = of_tsv phosphosite_of_list

let ll_uniprot_phosphosites db ac = 
  phosphosites_of_tsv (try ll_get db "ups" ac with Not_found -> "")

let uniprot_phosphosites db name = 
  let seq = uniprot_sequence db name in 
  let sane ps = 
    try (Pt.res ps.rsd).[0] == seq.[Pt.loc ps.rsd] 
    with Invalid_argument _ -> false 
  in grep sane (catmap (ll_uniprot_phosphosites db) (uniprot_acs db name))

(** {7 fancy queries } *)

(** This might have some issues *)
let kegg_orthology_with_pdb_complex db kos =
  let pdbs = ssofls (uniprot_pdb_complexes db
                    (catmap (kegg_orthology_uniprot_names db) kos)) in
  grep (fun ko -> 
          not (SS.is_empty 
                (SS.union pdbs 
                          (ssofls (kegg_orthology_pdb_ids db ko))))) 
       kos

(** {7 complex projections } *)

(** returns a uniprot name -> *)
let site_union db ko =
  let ks = Pt.parse_fasta (kegg_orthology_msa db ko) in
  let prot_kons name iv_map =
    let seq = Pt.Map.find name ks in 
    let of_domain ipr = let ipr = interpro_align seq ipr in 
      (ipr.entry_short_name, I.mk ipr.start (ipr.start + ipr.length - 1)) in
    let of_residue phr = let start = Pt.aligned_index (Pt.loc phr.rsd) seq in
      (pr "%s%i" (Pt.res phr.rsd) start, I.mk start start) in
    let site_kons (name, iv) iv_map =
      SM.modify_def [] name (I.union iv) iv_map in
    fold site_kons iv_map (app 
        (map of_domain (uniprot_interpro db name))
        (map of_residue (uniprot_phosphosites db name))) in
  fold prot_kons SM.empty 
    (grep (fun x -> Pt.Map.mem x ks) (kegg_orthology_uniprot_names db ko))

let agent_signature db ko =
  let kons k v l = 
    if List.length v == 1 then k :: l
    else app (map (pr ("%s_%d") k) (1 -- List.length v)) l in
  rev (SM.fold kons (site_union db ko) [])

let nethack_draw src str off = 
  let update c s n = match (c, s.[n]) with
      ( _ , '<') -> s.[n] <- '<'
    | ( _ , '>') -> s.[n] <- '>'
    | ( _ , '*') -> s.[n] <- '*'
    | ('.', ' ') -> s.[n] <- '.'
    | ('.', '.') -> s.[n] <- 'o'
    | ('.', 'o') -> s.[n] <- 'O'
    | ('.', 'O') -> s.[n] <- '0'
    | ( x ,  _ ) -> s.[n] <-  x
  in
  let str = String.copy str in
  for i = 0 to String.length src - 1 do
    update src.[i] str (i + off)
  done ; str

let draw_protein ?seq:(seq="") db name =
  let seq = if seq <> "" then seq else uniprot_sequence db name in
  let domain_img dom seq =
    if dom.length < 2 then String.sub dom.entry_short_name 0 dom.length else
    let str = splice "<>" 1 0 (prl (dom.length - 2) dom.entry_short_name) in
    let len = String.length str in if len < dom.length
    then splice str (len - 1) 0 (String.make (dom.length - len) '.')
    else str
  in
  let prot_img = fold 
    (fun dom str -> nethack_draw (domain_img dom seq) str dom.start)
    (String.make (String.length seq) ' ')
    (sort ~cmp:(cmp_w (fun x -> x.start)) 
      (map (interpro_align seq) (uniprot_interpro db name)))
  in fold 
    (fun rs sr -> nethack_draw (Pt.res rs.rsd)  sr 
      (Pt.aligned_index (Pt.loc rs.rsd) seq))
    prot_img (uniprot_phosphosites db name)

let draw_kegg db ko =
  let ks = Pt.parse_fasta (kegg_orthology_msa db ko) in
  let site_map = 
    let (_, seq) = Pt.Map.choose ks in 
    let cnv = String.make (String.length seq + 30) '-' in
    SM.fold (fun name ivs cnv ->
        let nm name n = if List.length ivs = 1 then (pr "%s" name) 
          else (pr "%s_%d" name n) in
        snd (fold (fun (s, e) (n, c) -> 
               (n + 1, nethack_draw (nm name n) c s))
               (1, cnv) (rev ivs)))
      (SM.map I.finite (site_union db ko)) cnv
  in
  let agent_row = pr "%s:%s" (kegg_orthology_name db ko) site_map in
  Pt.Map.foldi (fun name seq acc -> let f = pr "%s:%s" name in
      acc^"\n"^(f (draw_protein ~seq:seq db name))^"\n"^(f seq)) ks agent_row
