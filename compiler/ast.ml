
type box =
  | Box of string * int * int         (* Name, input and output ports *)

type wire =
  | Wire of string * string     (* wire_id, from_exp, from_port, to_exp, to_port *)

(* AST for string-diagrams *)
type diagram =
  | Identity
  | Morphism of string
  | Tensor of diagram * diagram
  | Composition of diagram * diagram * string list * string list

type program =
  | Program of box list * wire list  * diagram

(* Pretty printing for the AST *)
let rec string_of_diagram = function
  | Identity                         -> "Identity"
  | Morphism m                       -> m
  | Tensor (f,g)                     -> string_of_diagram f ^ string_of_diagram g
  | Composition (f,g,ins,outs)       -> "Composition(" ^ (string_of_diagram f) ^ "," ^ (string_of_diagram g) ^"  )"

let string_of_top = function
  | Program(b_list, w_list, diag) -> string_of_diagram diag
