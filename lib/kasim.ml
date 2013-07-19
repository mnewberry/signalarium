let rec agent_list_of_mixture = function
    Ast.COMMA (a, m) -> a :: (agent_list_of_mixture m)
  | Ast.EMPTY_MIX -> []

let rec port_list_of_intf = function
    Ast.PORT_SEP (a, m) -> a :: (port_list_of_intf m)
  | Ast.EMPTY_INTF -> []
