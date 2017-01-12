(* AST for string-diagrams *)
type diagram =
  | Identity
  | Morphism of string * string list * string list  (* Name, input and output ports *)
  | Object of int
  | Tensor of diagram * diagram
  | Composition of diagram * diagram
  | Morphism_ID of string
  | Wire_ID of string

(* Pretty printing for the AST *)
let rec string_of_ast = function
  | Identity                         -> "Identity"
  | Morphism (name, inputs, outputs) -> inputs ^ " -> " ^ name ^ " -> " ^ outputs
  | Object o                         -> string_of_int o
  | Tensor (f,g)                   -> string_of_ast f ^ string_of_ast g
  | Composition (f,g)              -> string_of_ast f ^ " ; " ^ string_of_ast g
  | Let(name, e1, e2)                -> "Let (" ^ name ^ " = " ^ (string_of_ast e1) ^ " in " ^ (string_of_ast e2)
