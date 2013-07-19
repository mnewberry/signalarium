(** Sqlite3 database interface *)

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
let ign = ignore 

let of_tsv of_list tsv = map (splittab |- of_list) (splitnl tsv)

(** {7 low-level database structural abstaction} *)

type db = {
  fn : string ;
  db : Sqlite3.db ;
  cache_u : Xml.doc SM.t ref ;
  cache_pr : Xml.doc SM.t ref }

let dbopen fn =
  let db = Sqlite3.db_open fn in
  ign (Sqlite3.exec db "create table if not exists tab (key TEXT, val TEXT)") ;
  { fn = fn ; db = db; cache_u = ref SM.empty ; cache_pr = ref SM.empty }

let dbsave db = ()

let get_text = function 
    Sqlite3.Data.TEXT text -> text 
  | _ -> failwith "Not text"


let keys db = 
  let stmt = Sqlite3.prepare db.db "select key from tab" in
  let rec fetch ls = match Sqlite3.step stmt with 
      Sqlite3.Rc.ROW -> fetch ((get_text ((Sqlite3.row_data stmt).(0))) :: ls)
    | Sqlite3.Rc.DONE -> Util.rev ls
    | _ -> failwith "shouldn't happen"
  in fetch []

let ns_keys db ns = let l = String.length in
  map (fun str -> String.sub str (l ns + 1) (l str - 1 - l ns))
    (grep (Pcre.pmatch ~pat:(pr "^%s:" ns)) (keys db))

let full_key namespace key = (pr "%s:%s" namespace key)

let ll_put db ns k v = let key = full_key ns k in 
  Printf.printf "[DB put] %s -> %s\n%!" key (prl 50 v) ;
  let stmt = Sqlite3.prepare db.db "insert or replace into tab values (?, ?)" in
  ign (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key)) ;
  ign (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT v)) ;
  ign (Sqlite3.step stmt)

let ll_get db ns k = let key = full_key ns k in
  Printf.printf "[DB get] %s -> " key ;
  let stmt = Sqlite3.prepare db.db "select val from tab where key = ?" in
  ign (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key)) ;
  (match Sqlite3.step stmt with Sqlite3.Rc.ROW -> () | _ -> raise Not_found);
  let v = match (Sqlite3.row_data stmt).(0) with
      Sqlite3.Data.TEXT text -> text
    | _ -> raise Not_found in
  ign (Sqlite3.step stmt) ;
  Printf.printf "%s\n%!" (prl 50 v) ; v

let ll_del db ns k = let key = full_key ns k in 
  let stmt = Sqlite3.prepare db.db "delete from tab where key = ?" in
  ign (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key)) ;
  ign (Sqlite3.step)

let ns_del db ns = iter (ll_del db ns) (ns_keys db ns)

(** {7 central abstraction} *)

let get db retrieve namespace key =
  try ll_get db namespace key
  with Not_found -> let v = retrieve key in ll_put db namespace key v ; v

let memcache cache k vf =
  try SM.find k !cache
  with Not_found -> let v = vf () in cache := SM.add k v !(cache) ; v
