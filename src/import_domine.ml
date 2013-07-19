module M = BatMap.Make(String)
module S = BatSet.Make(String)

let pr = Printf.sprintf
let join set = String.concat " " (BatList.of_enum (S.enum set))

let import dbfn domfn = 
  let db = Db_sql.dbopen dbfn in 
  let rec collect ch map = 
    match (try Some (input_line ch) with End_of_file -> None) with
        Some ln ->
          let lst = Pcre.split ~pat:"\\|" ln in
          let pfamA = List.nth lst 0 in
          let pfamB = List.nth lst 1 in
          Db_sql.ll_put db "d" (pr "%s|%s" pfamA pfamB) ln ;
          Db_sql.ll_put db "d" (pr "%s|%s" pfamB pfamA) ln ;
          let map = M.modify_def S.empty pfamA (fun v -> S.add pfamB v) map in
          let map = M.modify_def S.empty pfamB (fun v -> S.add pfamA v) map in
          collect ch map
      |  None -> map in
  let ch = open_in domfn in let map = collect ch M.empty in close_in ch ;
  M.iter (fun k v -> Db_sql.ll_put db "pfdi" k (join v)) map ; Db_sql.dbsave db

let () = Printf.printf "reading into %s %s\n%!" (Sys.argv.(1)) (Sys.argv.(2))
let () = import (Sys.argv.(1)) (Sys.argv.(2))
