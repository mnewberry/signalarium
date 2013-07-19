module PT = Pxp_types
module PD = Pxp_document
module PTP = Pxp_tree_parser

(* PXP Event functions *)
let pxe_tag_wrapper consumer evt pull =
  match evt with 
    PT.E_start_tag (tag,_,_,_) -> consumer tag pull
  | _ -> invalid_arg "Non-start_tag event"

(** UNTESTED: ignore an element by consuming the event stream up to and
		including the matching end tag for [evt].  [evt] must be the most recent
		value returned from [pull].  [pe_ignore] handles nesting of same-named
    elements correctly.  *)
let pxe_ignore evt pull =
  let rec pxe_ignore_ tag pull = match pull () with
      None -> failwith "unbalanced tags!"
    | Some (PT.E_end_tag (t,_)) ->
        if t = tag then () else pxe_ignore_ tag pull
    | Some (PT.E_start_tag (t,_,_,_)) -> (* match depth if nested *)
        if t = tag
        then (pxe_ignore_ t pull ; pxe_ignore_ t pull)
        else pxe_ignore_ tag pull
    | _ -> pxe_ignore_ tag pull
  in pxe_tag_wrapper pxe_ignore_ evt pull

(** parse an element and its children into the [Pxp_document] representation.
		[evt] must be the last event returned from [pull].  [pxe_solidify]
		consumes the event stream up to and including the matching end tag for
		[evt] and returns node object representing the element. *)
let rec pxe_solidify evt pull =
  let pxe_solidify_ tag pull =
    let depth = ref (-1) in
    (* We need to know when to stop feeding events to PD.solidify *)
    let our_pull () =
      if !depth < 0 then (depth := 1; Some evt) else
      if !depth = 0 then None else match pull () with
        None -> failwith "unbalanced tags!"
      | Some PT.E_start_tag (t,_,_,_) as e ->
          if t = tag then incr depth else () ; e
      | Some PT.E_end_tag (t,_) as e ->
          if t = tag then decr depth else () ; e
      | e -> e
    in
    match PD.solidify PT.default_config PTP.default_spec our_pull with
      `Node node -> node | _ -> failwith "solidify should return a node"
  in pxe_tag_wrapper pxe_solidify_ evt pull

let string_of_value = function
    PT.Value str -> str 
  | _ -> failwith "Not a string attribute"

let attr_str k l = Util.opt_fmap string_of_value (Util.assoc k l)

let filter_element_type name = 
  List.filter (fun node -> match node#node_type with 
      PD.T_element n -> n = name 
    | _ -> false)
