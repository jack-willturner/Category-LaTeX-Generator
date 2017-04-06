let unwrap_str = function
  | None    -> ""
  | Some(s) -> List.hd s (* Maybe monadic bind *)

type box =
  | Box of string * int * int * (string option * string option) option  (* Name, input and output ports *)

type wire =
  | Wire of string * string     (* wire_id, from_exp, from_port, to_exp, to_port *)

(* AST for string-diagrams *)
type diagram =
  | Identity of string * string list option * string list option
  | Morphism of string * string list option * string list option
  | Tensor of diagram * diagram
  | Composition of diagram * diagram
  | Subdiagram of string * diagram * string list option * string list option

type module_def =
  | Module of string * box list * wire list option * diagram

type definition =
  | Definition of box list * wire list option

type pnt = Point of (float * float)

type program =
  | Program of module_def list * definition list * diagram

(* Pretty printing for the AST *)
let rec string_of_diagram = function
  | Identity (str,ins,outs)          -> "[" ^ (unwrap_str  ins) ^ "]id[" ^ (unwrap_str outs) ^ "]"
  | Morphism (m, ins, outs)          ->  m
  | Tensor (f,g)                     -> "Tensor(" ^ string_of_diagram f ^ "," ^ string_of_diagram g ^ ")"
  | Composition (f,g)                -> "Composition(" ^ (string_of_diagram f) ^ "," ^ (string_of_diagram g) ^")"
  | Subdiagram (name, diagram', ins, outs) -> "Subdiagram(" ^ name ^ ")"

let string_of_top = function
  | Program(module_list, diagram_list, diagram') -> string_of_diagram diagram'

type priority =
    | Free
    | OccupiedHorizontal
    | OccupiedVertical
    | Blocked

type node = {
    name : string;
    xLoc : int;
    yLoc : int;
    status : priority;
    successors : string option list;
    parent : string;
    cost : int;
}

(* Ports on boxes can either be a number describing the number of ports, or a string name given explicitly to the port *)
type port =
  | Number of int
  | String of string
  | Fork of port
  | Join of port

type module_sub_box =
  | SubBox of string * int * int * string list * string list
