module PT = Pxp_types 
module PD = Pxp_document
module PTP = Pxp_tree_parser
module PEP = Pxp_ev_parser
module EM = Emysql

module Strs = Set.Make(String)

open Util
open Pxp_helper

let import ?(disp = (fun _ _ -> ())) ?(tags = []) uniprot_xml proteins dbd =
  let mysql a b = EM.exec dbd a b in
  (** Parse a protein element *)
  
  let str_or_null = function "" -> EM.Null | s -> EM.String s in
  
  let map = List.map and cat = String.concat and els = filter_element_type
    and length = List.length and filter = List.filter in
  
  let rec seek_one node path = 
    match path with
      [] -> Some node
    | (hd :: tl) -> (match els hd node#sub_nodes with
        [] -> None
      | [node] -> seek_one node tl
      | _ -> failwith ("seek_one encountered multiple \""^hd^"\" entries")) in

  let attr node attr = attr_str attr (node#attributes) in
  
  let iter = ref 0 in
  let entry evt pull =
    if !iter mod 59 = 0 then (disp None !iter ; incr iter) 
    else incr iter ;
    let entr = pxe_solidify evt pull in
    PD.strip_whitespace ~left:`Strip_seq ~right:`Strip_seq entr ;
    let ids =
      let ns = els "accession" entr#sub_nodes in
      map (fun m -> m#data) ns in
    if length (filter (fun id -> Strs.mem id proteins) ids) = 0 then () else
    let get l = try opt_req (seek_one entr l)
      with _ -> failwith (cat ";" l) in

    (disp (Some (List.hd ids)) !iter) ; 
    let gene_names = catmap
      (fun gene -> map (fun m -> m#data) (els "name" gene#sub_nodes))
      (els "gene" entr#sub_nodes) in
  
    let hd_opt = function (hd :: tl) -> Some hd | _ -> None in
    let tl_l = function (hd :: tl) -> tl | _ -> [] in
  
    ignore (mysql "INSERT INTO protein VALUES(NULL,?,?,?,?,?,?,?,?,?,?)" [
      EM.String (List.hd ids) ;
      EM.String (get ["name"])#data ;
      EM.String (get ["protein";"recommendedName";"fullName"])#data ;
      str_or_null (cat ";" (map (fun an -> an#data) 
                    (els "alternativeName" (get ["protein"])#sub_nodes))) ;
      EM.str_opt_to_mysql (hd_opt gene_names) ;
      str_or_null (cat ";" (tl_l gene_names)) ;
      EM.Number (opt_req (attr (get ["sequence"]) "length")) ;
      EM.String (List.hd (els "name" (get ["organism"])#sub_nodes))#data ;
      EM.String (cat ";" ("uniprot" :: tags)) ;
      EM.Null ]) in
  
  let rec parse pull = 
    match pull () with
      None -> ()
    | Some (PT.E_start_tag ("entry",_,_,_) as evt) ->
        entry evt pull ; parse pull
    | _ -> parse pull in
  
  (* Load XML document *)
  let xcfg = PT.default_config in
  let xentm = PEP.create_entity_manager xcfg (PT.from_file uniprot_xml) in
  let xpull = PEP.create_pull_parser xcfg (`Entry_document []) xentm in
  parse xpull
