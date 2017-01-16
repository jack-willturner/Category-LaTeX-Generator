
type morphism =
  | Morphism of string * string list * string list         (* Name, input and output ports *)

type wire =
  | Wire of string * string * string * string * string     (* wire_id, from_exp, from_port, to_exp, to_port *)

(* AST for string-diagrams *)
type diagram =
  | Identity
  | Tensor of diagram * diagram
  | Composition of diagram * diagram
  | Morphism_ID of string
  | Wire_ID of string

type program =
  | Program of morphism list * wire list  * diagram

(* Pretty printing for the AST *)
let rec string_of_diagram = function
  | Identity                         -> "Identity"
  | Tensor (f,g)                     -> string_of_diagram f ^ string_of_diagram g
  | Composition (f,g)                -> string_of_diagram f ^ " ; " ^ string_of_diagram g
  | Morphism_ID s                    -> s
  | Wire_ID s                        -> s

let string_of_top = function
  | Program(m_list, w_list, diag) -> string_of_diagram diag
