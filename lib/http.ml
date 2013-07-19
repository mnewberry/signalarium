let retrieve_url pattern key =
  let pr = Printf.sprintf in
  let url = (pr pattern key) in
  try Http_client.Convenience.http_get url
  with Http_client.Http_error (404, _) -> invalid_arg (pr "%s not found" url)

let retrieve_post url params =
  let pr = Printf.sprintf in
  try Http_client.Convenience.http_post url params
  with Http_client.Http_error (n, str) -> failwith (pr "Http error: %s" str)

let request_embl request retrieve =
  let pr = Printf.sprintf in
  let rec retry req t =
    let sleep time = let _ = Unix.select [] [] [] time in () in
    try req ()
    with Http_client.Http_error (400, _) -> sleep t ; retry req (t *. 1.5)
  in
  let resp =
    try request ()
    with Http_client.Http_error (n, str) -> failwith (pr "Http error: %s" str)
  in
  retry (fun () -> retrieve resp) 0.25
