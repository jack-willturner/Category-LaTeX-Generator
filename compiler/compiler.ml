open Ast
open Hashtbl
open Buffer

(* TODO today - label up the identity wires *)
(* TODO today - write rightmost, bottommost functions *)
(* TODO today - figure out escape characters *)

(* First, set up a hashtable of morphisms *)
let morphisms = Hashtbl.create 64
let wires     = Hashtbl.create 64
let wire_drawings = Buffer.create 64

let hiddenNodeCount = ref 0
let morphismCount   = ref 0

let hiddenNode() = hiddenNodeCount := hiddenNodeCount + 1; (!hiddenNodeCount - 1)
let morphism()   = morphismCount := morphismCount + 1; (!morphismCount - 1)

let prefix = "\\tikzstyle{morphism}=[minimum size = 7mm, above=0mm, rectangle, draw=black, fill=white, thick]\n
\\tikzstyle{empty}   =[circle, draw=white, fill=white, thick] \n
\\begin{tikzpicture}\n"

let suffix  = "\\end {tikzpicture}"


let rightmost = function
  | Identity            -> failwith "not implemented"
  | Tensor(d1, d2)      -> rightmost d2              (* TODO check that this is correct *)
  | Composition(d1,d2)  -> rightmost d2
  | Morphism_ID m       -> m
  | Wire_ID w           -> w

let compile_diagram = function
  | Identity                ->
        let empty1 = "empty" ^ (empty |> string_of_int) in
        let empty2 = "empty" ^ (empty |> string_of_int) in
        "\\node[empty]\\t\\t\\t(" ^ empty1 ^ ")\\t\\t\\t {};\\n" ^
        "\\node[empty]\\t\\t\\t(" ^ empty2 ^ ") [right of=" ^ empty1 ^ "] {};\n" ^
        "\\draw [black] (" ^ empty1 ^ ".east) -- " ^ empty2 ^ ".west);\n"
  | Tensor(d1, d2)          -> failwith "not implemented: tensor"
  | Composition(d1,d2)      ->
        let str1 = compile_diagram d1 in
        let rightmost_node = rightmost d1 in
        (match d2 with
          | Morphism_ID m     ->
              "\\node[morphism]     (" ^ m ^ ") [right of=" ^ rightmost_node ^ "]   {$"^m^"$};\n"
          | Composition(Morphism_ID m, d2) ->
              "\\node[morphism]     (" ^ m ^ ") [right of=" ^ rightmost_node ^ "]   {$"^m^"$};\n" ^
              compile_diagram d2
          | _ -> failwith "not implemented"
        )
  | Morphism_ID m           -> "\\node[morphism]     (" ^ m ^ ") []   {$"^m^"$};\n"
  | Wire_ID w               -> failwith "not implemented: wire"


let draw_wire (name,from_,from_port,to_,to_port) =
  Buffer.add_string wire_drawings "\\draw [black] (" ^ from_ ^ ".east) -- (" ^ to_ ".west);\n"

let update_morphism_table = function
  | []        -> ()
  | (Morphism(name, ins, outs)::xs) ->
          Hashtbl.add morphisms name (name, ins, outs); update_morphism_table xs

let update_wire_table = function
  | []        -> ()
  | (Wire(name, from_, from_port, to_, to_port)::xs) ->
          Hashtbl.add wires name (name, from_,from_port, to_, to_port); update_wire_table xs

let compile_program = function
  | Program(m_list, w_list, diagram) ->
          update_morphism_table m_list;
          update_wire_table w_list;
          let body = compile_diagram diag in
          let wires = Hashtbl.iter draw_wire wires in
          let whole = prefix ^ body ^ wires ^ suffix in
          printf "%s\n" whole
