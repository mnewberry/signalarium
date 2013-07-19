open Http_client.Convenience

let _of_pathway entry_type url =
  (* Get rid of doctype string, to avoid the complexity of the PXP resovler *)
  let kgml = Pcre.replace ~pat:"<!DOCTYPE [^>]*>" ~templ:"" (http_get url) in
  let config = Pxp_types.default_config in
  let spec = Pxp_tree_parser.default_spec in
  let source = Pxp_types.from_string kgml in
  let doc = Pxp_tree_parser.parse_wfdocument_entity config source spec in
  let names = List.map 
    (fun node -> match node # optional_string_attribute "type" with 
         Some ty -> 
           if String.lowercase ty = entry_type
             then node # optional_string_attribute "name" else None
       | _ -> None)
    (Pxp_helper.filter_element_type "entry" doc # root # sub_nodes) in
  List.map 
    (Pcre.replace ~pat:"ko:" ~templ:"")
    (Util.catmap (function 
          Some kos -> Pcre.split kos 
        | None -> [])
      names)

let ko_of_pathway = _of_pathway "ortholog"

let uniprot_of_ko ko =
  let page = 
    http_get ("http://www.genome.jp/dbget-bin/get_linkdb?-t+uniprot+ko:"^ko) in
  List.map (fun m -> Pcre.get_substring m 1)
    (Array.to_list
      (try 
        (Pcre.exec_all ~pat:"\"/dbget-bin/www_bget\\?uniprot:([^\"]+)\"" page)
      with Not_found -> print_endline page ; print_endline ko ; failwith "don'tknow"))

let name_of_ko ko =
  Pcre.get_substring
    (Pcre.exec ~pat:(ko^"</a></b><br>\\(([\\w]+)\\)")
      (http_get
        ("http://www.kegg.jp/kegg-bin/view_ortholog_table?orthology="^ko))) 1
