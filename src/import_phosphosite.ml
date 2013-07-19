module M = BatMap.Make(String)

let import dbfn phosfn =
  let rec collect ch map =
    match (try Some (input_line ch) with End_of_file -> None) with
        Some ln ->
          let lst = Pcre.split ~pat:"\t" ln in
          let uac = List.nth lst 1 in
          collect ch (M.modify_def "" uac (fun v -> v ^ "\n" ^ ln) map)
      | None -> map in
  let ps = open_in phosfn in let map = collect ps M.empty in close_in ps ;
  let db = Db_sql.dbopen dbfn in
  M.iter (fun ac psrec -> (* lchop because a "\n" is prepended to everything *)
           Db_sql.ll_put db "ups" ac (BatString.lchop psrec)) map

let () = Printf.printf "reading into %s %s\n%!" (Sys.argv.(1)) (Sys.argv.(2))
let () = import (Sys.argv.(1)) (Sys.argv.(2))
