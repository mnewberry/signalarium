open Util

type endpoint = Inf | Pos of int

type t = (endpoint * endpoint) list

let mk a b = [(Pos a, Pos b)]
let empty = []

let finite = map (function (Pos a, Pos b) -> (a, b) 
                         | _ -> failwith "Not finite")

let length ivs = fold (fun (s,f) sum -> sum + f - s + 1) 0 (finite ivs)

let inv a = 
  let rec help acc lastf = function
      [] -> rev ((Pos (lastf + 1), Inf) :: acc)
    | hd :: tl -> (match hd with
         (Pos s, Inf) -> rev ((Pos (lastf + 1), Pos (s - 1)) :: acc)
       | (Pos s, Pos f) -> help ((Pos (lastf + 1), Pos (s - 1)) :: acc) f tl
       | _ -> failwith "somebody goofed")
  in match a with
      [] -> [(Inf, Inf)]
    | hd :: tl -> (match hd with
         (Inf, Inf) -> []
       | (Inf, Pos f) -> help [] f tl
       | (Pos s, Inf) -> [(Inf, Pos (s - 1))]
       | (Pos s, Pos f) -> help [(Inf, Pos (s - 1))] f tl)

let union a b =
  let pred a = match a with Inf -> Inf | Pos a -> Pos (a - 1) in
  let cmpll a b = match (a, b) with (Pos a, Pos b) -> compare a b
    | (Inf, Inf) -> 0
    | (Inf, Pos b) -> -1
    | (Pos a, Inf) -> 1 in
  let cmprr a b = match (a,b) with (Pos a,Pos b) -> compare a b 
    | _ -> -(cmpll a b) in
  let cmprl a b = match (a,b) with (Pos a,Pos b) -> compare a b 
    | _ -> 1 in
  let minll a b = if cmpll a b <= 0 then a else b in
  let maxrr a b = if cmprr a b <= 0 then b else a in
  let unop (xs, xf) (ys, yf) = (minll xs ys, maxrr xf yf) in
  let disjoint (xs, xf) (ys, yf) =
    cmprl xf (pred ys) = -1 || cmprl yf (pred xs) = -1 in
  let get_min a b = match (a, b) with
      [], [] -> failwith "no minimum of an empty list"
    | (ahd :: atl), [] -> (ahd, atl, [])
    | [], (bhd :: btl) -> (bhd, btl, [])
    | (((ast, af) as ahd) :: atl), (((bst, bf) as bhd) :: btl) ->
        if cmpll ast bst <= 0 then (ahd, atl, b) else (bhd, btl, a) in
  let rec unl acc (cur, a, b) =
    let merge (min, a, b) =
      if disjoint cur min
      then unl (cur :: acc) (min, a, b)
      else unl acc ((unop cur min), a, b)
    in
    match (a, b) with [], [] -> rev (cur :: acc) | _ -> merge (get_min a b)
  in if a = [] && b = [] then [] else unl [] (get_min a b)

let inter a b = inv (union (inv a) (inv b))

let overlap a b = inter a b <> []
