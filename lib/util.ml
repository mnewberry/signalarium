let assoc k l = try Some (List.assoc k l) with _ -> None
let assocmem = List.mem_assoc

(* option combinators *)
let opt_none = function Some _ -> false | None -> true
let opt_some x = not (opt_none x)
let opt_def d = function Some x -> x | None -> d
let opt_def_f f = function Some x -> x | None -> f ()
let opt_req = function
    Some x -> x
  | None -> failwith "Some required but None present"
let opt_bind f = function
    Some x -> f x
  | None -> None
let opt_fmap f = function
    Some x -> Some (f x)
  | None -> None
let opt_of_list = function
    [] -> None
  | [x] -> Some x
  | _ -> failwith "Could not convert pleural list to option"
let list_of_opt = function
    Some x -> [x]
  | None -> []
let list_of_list_opt l = List.filter opt_some l
let the = function [x] -> x | _ -> failwith "Multiple items when one expected"
let opt_norm = function
    Some (Some x) -> x
  | _ -> None
let exc_to_opt f = try Some (f ()) with _ -> None

let constant x = (fun _ -> x)
let identity x = x
let assert_eq a b = if a = b then () else failwith "assertion failure"

(* tail recursive list operations *)
let cons a b = a :: b 
let rec fold kons knil = function
    [] -> knil
  | h :: t -> fold kons (kons h knil) t 
let rev l = fold cons [] l
let app m n = fold cons n (rev m)
let map f l = let kons kar kdr = (f kar) :: kdr in fold kons [] (rev l)
let catmap f l = let kons kar kdr = app (f kar) kdr in fold kons [] (rev l)
let cat l = fold app [] (rev l)
let grep p l = fold (fun el l -> if p el then el :: l else l) [] (rev l)

let slurp filename = let chan = open_in filename in
  let rec slurp_ str = 
    match (try Some (input_line chan) with End_of_file -> None) with
      Some line -> slurp_ (str ^ line ^ "\n") | None -> str in
  let cont = slurp_ "" in close_in chan ; cont
let slurp_obj filename = let chan = open_in filename in 
  let x = input_value chan in close_in chan ; x
let dump_obj filename obj = let ch = open_out filename in
  output_value ch obj ; close_out ch

let cmp_w f a b = compare (f a) (f b)
let call_with_in_fh n f = let h = open_in n in let v = f h in close_in h ; v

let split = Pcre.split ~pat:""
let splitws = Pcre.split
let splitnl = Pcre.split ~pat:"\\n"
let splittab = Pcre.split ~pat:"\\t"
let joinsp = String.concat " "
let joinnl = String.concat "\n"
let purge_ws = Pcre.replace

module SM = BatMap.Make(String)
module SS = BatSet.Make(String)

let smkeys sm = BatList.of_enum (SM.keys sm)
let sskeys ss = BatList.of_enum (SS.enum ss)
let smcoll sm = SM.filter_map (constant identity) sm
let ssofls ls = SS.of_enum (BatList.enum ls)
let lsofss ss = BatList.of_enum (SS.enum ss)


(* BatMap operations *)
module Map = BatMap
module Set = BatSet
module StrSet = BatSet.Make(String)

let map_of_set f s = Set.fold (fun el m -> Map.add el (f el) m) s Map.empty
let invert_map mp = (* ('a, 'b Set) Map -> ('b -> 'a Set) Map *)
  let values = Map.foldi (fun k v s -> Set.union v s) mp Set.empty in
  let pre_image el = 
    Set.of_enum (Map.keys
    (Map.filteri (fun k v -> 
      Set.mem el v) mp)) in
  map_of_set pre_image values

(* *)
let printf = BatPrint.printf
let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)
let (--) a b = BatList.of_enum (BatPervasives.(--) a b)
let nub = BatList.unique
let sort = BatList.sort
let splice = BatString.splice

let flip f a b = f b a

let duplicates (l : string list) =
  fold (fun id (seen, twice) ->
         if StrSet.mem id seen
         then (seen, StrSet.add id twice)
         else (StrSet.add id seen, twice)) (StrSet.empty, StrSet.empty) l
  |> snd |> StrSet.enum |> BatList.of_enum
