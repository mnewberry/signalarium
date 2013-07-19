open Db_sql
open Query
open Util
open Pdb 

(* let () = print_endline (draw_kegg db "k03068") *)
(* let _ = map (fun id -> (id, pdb_uniprot_indices db id)) (kegg_pathway_pdb_ids db "ko04310") *)
let db = dbopen "db.sqlite3"
(* let res = map (fun id -> (id, pdb_uniprot_indices db id)) (kegg_pathway_pdb_ids db "ko04310") ;; *)
let () = print_endline (draw_kegg db "K02105") ;;
(* let () = dbsave db ;; *)

