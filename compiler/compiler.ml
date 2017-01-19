open Ast
open Hashtbl
open Printf

(* First, set up a hashtable of morphisms *)
let morphisms = Hashtbl.create 64
let inputs = Hashtbl.create 64
let outputs = Hashtbl.create 64

let hiddenNodeCount = ref 0
let inputNodeCount = ref 0
let outputNodeCount = ref 0
let morphismCount   = ref 0

let new_input_node()   = inputNodeCount := !inputNodeCount + 1; (!inputNodeCount - 1)
let new_output_node()   = outputNodeCount := !outputNodeCount + 1; (!outputNodeCount - 1)
let hiddenNode() = hiddenNodeCount := !hiddenNodeCount + 1; (!hiddenNodeCount - 1)
let morphism()   = morphismCount := !morphismCount + 1; (!morphismCount - 1)

let prefix = "\\tikzstyle{morphism}=[minimum size = 3cm, above=0mm, rectangle, draw=black, fill=white, thick]\n
\\tikzstyle{empty}   =[circle, draw=white, fill=white, thick] \n
\\begin{tikzpicture}\n"

let suffix  = "\\end {tikzpicture}"

let rec rightmost = function
  | Identity            -> failwith "not implemented"
  | Tensor(d1, d2)      -> rightmost d2              (* TODO check that this can be used for top *)
  | Composition(d1,d2)  -> rightmost d2
  | Morphism(m)         -> m

let rec width = function
  | Identity            -> 1
  | Tensor(d1,d2)       -> max (width d1) (width d2)
  | Composition(d1,d2)  -> width d1 + width d2
  | Morphism            -> 1

let rec height = function
  | Identity            -> 1
  | Tensor(d1,d2)       -> height d1 + height d2
  | Composition(d1,d2)  -> max (height d1) (height d2)
  | Morphism            -> 1

let rec compile_diagram = function
  | Identity                -> Identity
  | Tensor(d1, d2)          -> failwith "not implemented: tensor"
  | Composition(f,g,ins,outs) ->
        let outputs_from_f = Hashtbl.find outputs f in  (* [o1,o2,o3] *)
        assert List.length outputs_from_f = List.length outs;
        Hashtbl.replace outputs f outs;
        let inputs_to_g = Hashtbl.find inputs g in
        assert List.length inputs_to_g = List.length ins;
        Hashtbl.replace inputs g ins
  | Morphism m              -> failwith "not implemented: morphism"

let rec update_morphism_table = function
  | []        -> ()
  | (Box(name, ins, outs)::xs) ->                     (* going to have a problem with *)
          let inputs' = [] in                         (* multiple occurrences of a morphism *)
          let inputs'' = (for i = 1 to ins do
            inputs :: (new_input ())
          done) in
          let outputs' = [] in
          let outputs'' = (for i = 1 to outs do
            outputs :: (new_output ())
          done) in
          Hashtbl.add inputs name inputs'';
          Hashtbl.add outputs name outputs'';
          update_morphism_table xs

let rec draw_wires = function
  | [] -> ""
  | (Wire(from_, to_)::xs) ->
          "\\draw [black] (" ^ from_ ^ ".east) -- (" ^ to_ ^ ".west);\n" ^ (draw_wires xs)

let rec draw_structurally tree x y =
  let max_width = width tree in
  let max_height = height tree in
  (* Create a grid of max_width * max_height *)
  (* Scan left to right adding morphisms to boxes *)
  match tree with
    | Identity                  ->
        let empty_left = hiddenNode() in
        let empty_right = hiddenNode() in
        "\\node (" ^ empty_left  ^ ")\tat (" ^ x ^ "," ^ (y+1) ^ ")\t\t{}\n"
        "\\node (" ^ empty_right ^ ")\tat (" ^ (x+2) ^ "," ^ (y+1) ^ ")\t\t{}\n"
        "\\draw [black] (" ^ empty_left ^ ".east) -- (" ^ empty_right ^ ".west);\n"
    | Morphism m                ->
        
    | Tensor(a,b)               -> failwith "not implemented: draw tensor"
    | Composition(f,g,ins,outs) ->



let compile_program = function
  | Program(b_list, w_list, diagram) ->
          update_morphism_table b_list;
          let wires = draw_wires w_list in
          let body = compile_diagram diagram in
          let whole = prefix ^ body ^ wires ^ suffix in
          whole
