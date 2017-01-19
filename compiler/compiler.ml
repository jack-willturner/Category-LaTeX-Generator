open Ast
open Hashtbl
open Buffer
open Printf

(* First, set up a hashtable of morphisms *)
let morphisms = Hashtbl.create 64      (* f : 1 -> 3 :: f | 1,3 *)
let inputs    = Hashtbl.create 64      (* f : 1 -> 3 :: f | i1  *)
let outputs   = Hashtbl.create 64      (* f : 1 -> 3 :: f | o1, o2, o3 *)
let temporary = Buffer.create  64

let hiddenNodeCount = ref 0
let inputNodeCount  = ref 0
let outputNodeCount = ref 0
let morphismCount   = ref 0

let new_input_node()   = inputNodeCount   := !inputNodeCount  + 1; (!inputNodeCount   - 1)
let new_output_node()  = outputNodeCount  := !outputNodeCount + 1; (!outputNodeCount  - 1)
let hiddenNode()       = hiddenNodeCount  := !hiddenNodeCount + 1; (!hiddenNodeCount  - 1)
let morphism()         = morphismCount    := !morphismCount   + 1; (!morphismCount    - 1)

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
  | Identity            -> 3
  | Tensor(d1,d2)       -> max (width d1) (width d2)
  | Composition(d1,d2)  -> width d1 + width d2
  | Morphism            -> 3

let rec height = function
  | Identity            -> 1
  | Tensor(d1,d2)       -> height d1 + height d2
  | Composition(d1,d2)  -> max (height d1) (height d2)
  | Morphism            -> 1

let rec compile_diagram = function
  | Identity                -> Identity
  | Tensor(d1, d2)          -> Tensor(d1,d2)
  | Composition(f,g,ins,outs) ->
        let outputs_from_f = Hashtbl.find outputs f in  (* [o1,o2,o3] *)
        assert List.length outputs_from_f = List.length outs;
        Hashtbl.replace outputs f outs;
        let inputs_to_g = Hashtbl.find inputs g in
        assert List.length inputs_to_g = List.length ins;
        Hashtbl.replace inputs g ins;
        Composition(f,g,ins,outs)
  | Morphism m              -> Morphism m


(* Potential solution for morphism recurrence problem:
      - label all the morphisms by number
      - put the morphisms back in the tree with their new label *)

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

let rec draw_structurally x y tree =
  let max_width = width tree in
  let max_height = height tree in
  (* Create a grid of max_width * max_height *)
  (* Scan left to right adding morphisms to boxes *)
  match tree with
    | Identity                  ->
        let empty_left = hiddenNode() in
        let empty_right = hiddenNode() in
        "\\node (" ^ empty_left  ^ ")\tat (" ^ x ^ "," ^ (y) ^ ")\t\t{}\n"
        "\\node (" ^ empty_right ^ ")\tat (" ^ (x+2) ^ "," ^ (y) ^ ")\t\t{}\n"
        "\\draw [black] (" ^ empty_left ^ ".east) -- (" ^ empty_right ^ ".west);\n"
    | Morphism m                ->
        let ins = Hashtbl.find inputs m in
        let outs = Hashtbl.find inputs m in
        let m_y = List.length outs in
        let height_of_grid = max (List.length outs * 2 - 1) (List.length ins * 2 - 1) in
        (* Draw nodes *)
        for i = 0 to (List.length ins) do
          "\\node[empty] (" ^ nth ins i ^ ")\tat (" ^ x ^ "," ^ (y+i) ^ ")\t\t{};\n" |> Buffer.add_string temporary
        done;
        for i = 0 to (List.length outs) do
          "\\node[empty] (" ^ nth outs i ^ ")\tat (" ^ (x+4) ^ "," ^ (y+i) ^ ")\t\t{};\n" |> Buffer.add_string temporary
        done;
        "\\node[morphism] (" ^ m ^ ")\tat (" ^ (x+2) ^ "," ^ m_y ^ ")\t\t{$" ^ m ^"$};\n" |> Buffer.add_string temporary;
        (* Connect wires *)
        for i = 0 to (List.length ins) do
          "\\draw [black] (" ^ nth ins i ^ ".east) -- (" ^ m ^ ".west);\n" |> Buffer.add_string temporary
        done
        for i = 0 to (List.length outs) do
          "\\draw [black] (" ^ m ^ ".east) -- (" ^ nth outs i ^ ".west);\n" |> Buffer.add_string temporary
        done
        let morphism_drawing = Buffer.contents temporary in
        Buffer.clear temporary;
        morphism_drawing
    | Tensor(a,b)               -> failwith "not implemented: draw tensor structurally"
    | Composition(f,g,ins,outs) -> failwith "not implemented: draw composition structurally"



let compile_program = function
  | Program(b_list, w_list, diagram) ->
          update_morphism_table b_list;
          let wires = draw_wires w_list in
          let body = compile_diagram diagram |> draw_structurally 0 0  in
          let whole = prefix ^ body ^ wires ^ suffix in
          whole
