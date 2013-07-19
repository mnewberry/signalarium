open Util
module PD = Pxp_document
module PT = Pxp_types

(** collect subnodes matching a predicate into a list *)
let coll p node = 
  let tl = PD.find_all ~deeply:true p node in
  if p node then node :: tl else tl

(* ok, PXP has a feature where you can extend nodes with different
functionality, but you can't use normal subclassing for some reason, so you
have to create nodes with an extension object.  The trouble is that then all
nodes have a type parameter, and the unextended nodes have a free parameter,
which leads to all kinds of confusion and at the very least long type
descriptions.  So it's desirable to provide an extension for nodes, just to get
rid of the free parameter and give PXP documents and nodes a simple type. *)
class dext = object (self)  (* dummy extension *)
  val mutable node = (None : dext PD.node option)
  method clone = {< >}
  method node = match node with None -> assert false | Some n -> n 
  method set_node n = node <- Some n
end

let default_spec = let c = new dext in 
  PD.make_spec_from_mapping (new PD.data_impl c) (new PD.element_impl c) (Hashtbl.create 100) ()

type doc = dext PD.document

let parse_str str = Pxp_tree_parser.parse_wfdocument_entity
  PT.default_config (PT.from_string str) default_spec

let strip_ws doc =
  PD.strip_whitespace ~left:`Strip_seq ~right:`Strip_seq doc#root ;
  let remove_wsdata node = match node#node_type with
      PD.T_data ->
        if Pcre.pmatch ~pat:"^\\s*$" node#data then node#remove () else ()
    | _ -> () 
  in PD.iter_tree ~pre:remove_wsdata doc#root

let find p node = PD.find ~deeply:true p node

let vals = function
    PT.Value str -> [str]
  | PT.Valuelist l -> l
  | _ -> []

let to_alist node = map
  (fun n -> match n#node_type with PD.T_element s -> (s, n#data) | _ -> ("", n#data))
  node#sub_nodes

let sval = function PT.Value str -> str | _ -> failwith "Single value expected"

let attr att node = opt_fmap vals (assoc att (node#attributes))

let cand a b = (fun nd -> if a nd then b nd else false)
let cnamed name nd = match nd#node_type with 
  PD.T_element s -> s = name | _ -> false
let cattr a v nd = let nv = attr a nd in Some v = nv
let chasdata nd = nd#data <> ""

let of_str str = 
  let strip_ws doc = 
    Pxp_document.strip_whitespace ~left:`Strip_seq ~right:`Strip_seq doc#root ;
    let remove_wsdata node = match node#node_type with 
        Pxp_document.T_data -> 
          if Pcre.pmatch ~pat:"^\\s*$" node#data then node#remove () else ()
      | _ -> () in
    Pxp_document.iter_tree ~pre:remove_wsdata doc#root
  in
  let doc = Pxp_tree_parser.parse_wfdocument_entity 
    Pxp_types.default_config
    (Pxp_types.from_string str)
    Pxp_tree_parser.default_spec in
  Pxp_document.strip_whitespace ~left:`Strip_seq ~right:`Strip_seq doc#root ;
  strip_ws doc ; doc
