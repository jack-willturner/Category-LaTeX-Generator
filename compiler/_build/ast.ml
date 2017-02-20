type box =
  | Box of string * int * int * (string option * string option) option  (* Name, input and output ports *)

type wire =
  | Wire of string * string     (* wire_id, from_exp, from_port, to_exp, to_port *)

(* AST for string-diagrams *)
type diagram =
  | Identity
  | Morphism of string * string list option * string list option
  | Tensor of diagram * diagram
  | Composition of diagram * diagram
  | Subdiagram of diagram * int * int  (* inputs and outputs *)

type module_def =
  | Module of string * box list * wire list * diagram

type definition =
  | Definition of box list * wire list

type pnt = Point of (float * float)

type program =
  | Program of module_def list * definition list * diagram

(* Pretty printing for the AST *)
let rec string_of_diagram = function
  | Identity                         -> "Identity"
  | Morphism (m, ins, outs)          -> m
  | Tensor (f,g)                     -> "Tensor(" ^ string_of_diagram f ^ ", " ^ string_of_diagram g ^ ")"
  | Composition (f,g)                -> "Composition(" ^ (string_of_diagram f) ^ "," ^ (string_of_diagram g) ^"  )"
  | Subdiagram (diagram', ins, outs) -> string_of_diagram diagram'

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
      cost :  int;
  }
