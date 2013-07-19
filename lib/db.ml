(** In-memory database interface *)

open Util
open Http_client.Convenience

module Pt = Protein

let pr = Printf.sprintf
let iter = List.iter
let prl len str =    (* turn newlines into two characters '\n' *)
  if len < 3 then String.make len '.' else
  let str = Pcre.replace ~pat:"\\n" ~templ:"\\n" str in
  if String.length str <= len then str 
  else pr "%s..." (String.sub str 0 (len - 3))

let of_tsv of_list tsv = map (splittab |- of_list) (splitnl tsv)

(** {7 low-level database structural abstaction} *)

type db = {
  fn : string ;
  db : string SM.t ref ;
  cache_u : Xml.doc SM.t ref ;
  cache_pr : Xml.doc SM.t ref }

let dbopen fn =
  try { fn = fn ; db = ref (slurp_obj fn) ; 
        cache_u = ref SM.empty ; cache_pr = ref SM.empty }
  with Sys_error _ -> { fn = fn ; db = ref SM.empty ; 
        cache_u = ref SM.empty ; cache_pr = ref SM.empty }

let dbsave db = dump_obj db.fn !(db.db)

let keys db = BatList.of_enum (SM.keys !(db.db))
let ns_keys db ns = let l = String.length in
  map (fun str -> String.sub str (l ns + 1) (l str - 1 - l ns))
    (grep (Pcre.pmatch ~pat:(pr "^%s:" ns)) (keys db))
let full_key namespace key = (pr "%s:%s" namespace key)

let ll_put db ns k v = let key = full_key ns k in 
  Printf.printf "[DB put] %s -> %s\n%!" key (prl 50 v) ;
  db.db := SM.add key v !(db.db)

let ll_get db ns k = let key = full_key ns k in
  Printf.printf "[DB get] %s -> " key ;
  let v = SM.find key !(db.db) in
  Printf.printf "%s\n%!" (prl 50 v) ; v

let ll_del db ns k = let key = full_key ns k in db.db := SM.remove key !(db.db)
let ns_del db ns = iter (ll_del db ns) (ns_keys db ns)

(** {7 central abstraction} *)

let get db retrieve namespace key =
  try ll_get db namespace key
  with Not_found -> let v = retrieve key in ll_put db namespace key v ; v

let memcache cache k vf =
  try SM.find k !cache
  with Not_found -> let v = vf () in cache := SM.add k v !(cache) ; v
