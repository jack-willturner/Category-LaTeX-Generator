open Ast
open Hashtbl
open Buffer
open Printf

(* First, set up a hashtable of morphisms *)
let morphisms : (Ast.diagram, (string list, string list)) Hashtbl.t = Hashtbl.create 64      (* f : 1 -> 3 :: f | 1,3 *)
let inputs    = Hashtbl.create 64      (* f : 1 -> 3 :: f | i1  *)
let outputs   = Hashtbl.create 64      (* f : 1 -> 3 :: f | o1, o2, o3 *)
let temporary = Buffer.create  64

let hiddenNodeCount = ref 0
let inputNodeCount  = ref 0
let outputNodeCount = ref 0
let morphismCount   = ref 0

let new_input_node()   = inputNodeCount   := !inputNodeCount  + 1; ("i" ^ string_of_int(!inputNodeCount - 1))
let new_output_node()  = outputNodeCount  := !outputNodeCount + 1; ("o" ^ string_of_int(!outputNodeCount  - 1))
let hiddenNode()       = hiddenNodeCount  := !hiddenNodeCount + 1; ("empty" ^string_of_int(!hiddenNodeCount  - 1))
let morphism()         = morphismCount    := !morphismCount   + 1; ("morph" ^ string_of_int(!morphismCount    - 1))

let prefix = "\\tikzstyle{morphism}=[minimum size = 3cm, above=0mm, rectangle, draw=black, fill=white, thick]\n
\\tikzstyle{empty}   =[circle, draw=white, fill=white, thick] \n
\\begin{tikzpicture}\n"

let suffix  = "\\end {tikzpicture}"

let rec compile_diagram = function
  | Identity                -> Identity
  | Tensor(d1, d2)          -> Tensor(d1,d2)
  | Composition(f,g,ins,outs) ->
        let outputs_from_f = Hashtbl.find outputs f in
        Hashtbl.replace outputs f outs;
        let inputs_to_g = Hashtbl.find inputs g in
        Hashtbl.replace inputs g ins;
        Composition(f,g,ins,outs)
  | Morphism m              -> Morphism m

(* Potential solution for morphism recurrence problem:
      - label all the morphisms by number
      - put the morphisms back in the tree with their new label *)

let rec gen_inputs = function
  | 0       -> []
  | n       -> let n1 = new_input_node () in
                n1 :: (gen_inputs (n-1))

let rec gen_outputs = function
  | 0      -> []
  | n      -> let n1 = new_output_node () in
                n1 :: (gen_outputs (n-1))

let rec update_morphism_table = function
  | []        -> ()
  | (Box(name, ins, outs)::xs) ->
          let inputs' = gen_inputs ins in
          let outputs' = gen_outputs outs in
          Hashtbl.add inputs (Morphism name) inputs';
          Hashtbl.add outputs (Morphism name) outputs';
          update_morphism_table xs

let rec draw_wires = function
  | [] -> ""
  | (Wire(from_, to_)::xs) ->
          "\\draw [black] (" ^ from_ ^ ".east) -- (" ^ to_ ^ ".west);\n" ^ (draw_wires xs)

let rec draw_structurally x y tree =
  (* Create a grid of max_width * max_height *)
  (* Scan left to right adding morphisms to boxes *)
  match tree with
    | Identity                  ->
        let empty_left = hiddenNode() in
        let empty_right = hiddenNode() in
        "\\node (" ^ empty_left  ^ ")\tat (" ^ (x |> string_of_int) ^ "," ^ (y |> string_of_int) ^ ")\t\t{}\n" ^
        "\\node (" ^ empty_right ^ ")\tat (" ^ ((x+2)|>string_of_int) ^ "," ^ (y|>string_of_int) ^ ")\t\t{}\n" ^
        "\\draw [black] (" ^ empty_left ^ ".east) -- (" ^ empty_right ^ ".west);\n"
    | Morphism m                ->
        let ins = Hashtbl.find inputs (Morphism m) in
        let outs = Hashtbl.find inputs (Morphism m) in
        let m_y = List.length outs |> string_of_int in
        let height_of_grid = max (List.length outs * 2 - 1) (List.length ins * 2 - 1) in
        (* Draw nodes *)
        for i = 0 to (List.length ins) do
          "\\node[empty] (" ^ List.nth ins i ^ ")\tat (" ^ (x|>string_of_int) ^ "," ^ (y+i|>string_of_int) ^ ")\t\t{};\n" |> Buffer.add_string temporary
        done;
        for i = 0 to (List.length outs) do
          "\\node[empty] (" ^ List.nth outs i ^ ")\tat (" ^ (x+4|>string_of_int) ^ "," ^ (y+i|>string_of_int) ^ ")\t\t{};\n" |> Buffer.add_string temporary
        done;
        "\\node[morphism] (" ^ m ^ ")\tat (" ^ (x+2|>string_of_int) ^ "," ^ m_y ^ ")\t\t{$" ^ m ^"$};\n" |> Buffer.add_string temporary;
        (* Connect wires *)
        for i = 0 to (List.length ins) do
          "\\draw [black] (" ^ List.nth ins i ^ ".east) -- (" ^ m ^ ".west);\n" |> Buffer.add_string temporary
        done;
        for i = 0 to (List.length outs) do
          "\\draw [black] (" ^ m ^ ".east) -- (" ^ List.nth outs i ^ ".west);\n" |> Buffer.add_string temporary
        done;
        let morphism_drawing = Buffer.contents temporary in
        Buffer.clear temporary;
        morphism_drawing
    | Tensor(a,b)               ->
        (*  |_|--------|_a_|--------|_|   *)
        (*                                *)
        (*  |_|--------|_b_|--------|_|   *)
        let a' = draw_structurally x y a in
        let b' = draw_structurally x (y+2) b in
        a' ^ b'
    | Composition(f,g,ins,outs) ->
        let f' = draw_structurally x y f in
        let g' = draw_structurally (x+2) y g in
        f' ^ g'
    | Subdiagram(diagram,ins,outs) ->
        draw_structurally diagram

let compile_program = function
  | Program(b_list, w_list, diagram) ->
          update_morphism_table b_list;
          let wires = draw_wires w_list in
          let body = compile_diagram diagram |> draw_structurally 0 0  in
          let whole = prefix ^ body ^ wires ^ suffix in
          whole
