open Ast
open Hashtbl
open Buffer
open Printf


(* Algorithm outline *)

(* PREPROCESSOR: Generate styles for boxes, number all occurrences of boxes  *)
(* STEP 1: Create Hashtbl of boxes with a list of their inputs and outputs   *)
(* STEP 2: Replace all of the inputs and outputs according to labelled wires *)
(* STEP 3: Create Hashtbl of wires to link to each other                     *)
(* STEP 4: Draw boxes structurally                                           *)
(* STEP 5: Connect wires                                                     *)

(* First, set up a hashtable of morphisms *)
let morphisms = Hashtbl.create 64         (* f : 1 -> 3 :: f | 1,3 *)
let morphismLocations = Hashtbl.create 64 (* f | x, y *)
let temporary = Buffer.create  64

let hiddenNodeCount = ref 0
let inputNodeCount  = ref 0
let outputNodeCount = ref 0
let morphismCount   = ref 0

let new_input_node()   = inputNodeCount   := !inputNodeCount  + 1; ("i" ^ string_of_int(!inputNodeCount - 1))
let new_output_node()  = outputNodeCount  := !outputNodeCount + 1; ("o" ^ string_of_int(!outputNodeCount  - 1))
let hiddenNode()       = hiddenNodeCount  := !hiddenNodeCount + 1; ("empty" ^ string_of_int(!hiddenNodeCount  - 1))
let morphism()         = morphismCount    := !morphismCount   + 1; ("morph" ^ string_of_int(!morphismCount    - 1))

let prefix = "\n\n\\tikzstyle{morphism}=[minimum size = 1.25cm,right=10mm, rectangle, draw=black, fill=white, thick]
\\tikzstyle{empty}   =[circle, draw=white, fill=white, thick]\n
\\begin{tikzpicture}\n"

let suffix  = "\\end {tikzpicture}\n\n"

let rec update_morphism_table = function
  | []        -> ()
  | (Box(name, ins, outs, _)::xs) ->
          Hashtbl.add morphisms name (ins,outs);
          update_morphism_table xs

let rec draw_wires = function
  | [] -> ""
  | (Wire(from_, to_)::xs) ->
          "\t\\draw [black] (" ^ from_ ^ ".east) -- (" ^ to_ ^ ".west);\n" ^ (draw_wires xs)

let rec draw_structurally x y tree =
  (* Create a grid of max_width * max_height *)
  (* Scan left to right adding morphisms to boxes *)
  match tree with
    | Identity                  ->
        let empty_left = hiddenNode() in
        let empty_right = hiddenNode() in
        "\t\\node (" ^ empty_left  ^ ")\tat (" ^ (x |> string_of_int) ^ "," ^ (y |> string_of_int) ^ ")\t\t{}\n" ^
        "\t\\node (" ^ empty_right ^ ")\tat (" ^ ((x+2)|>string_of_int) ^ "," ^ (y|>string_of_int) ^ ")\t\t{}\n" ^
        "\t\\draw [black] (" ^ empty_left ^ ".east) -- (" ^ empty_right ^ ".west);\n"
    | Morphism(m, None, None)       ->
        let (num_inputs,num_outputs)  = Hashtbl.find morphisms m in
        let m_y = num_outputs |> string_of_int in
        let height_of_grid = max (num_outputs * 2 - 1) (num_inputs * 2 - 1) in
        let inputNode = new_input_node()  in
        let outputNode = new_output_node()  in
        (* Draw input node *)
        "\t\\node[empty] (" ^ inputNode  ^ ")\tat (" ^ (x  |>string_of_int) ^ "," ^ (y|>string_of_int) ^ ")\t\t{};\n" |> Buffer.add_string temporary;
        "\t\\node[empty] (" ^ outputNode ^ ")\tat (" ^ (x+4|>string_of_int) ^ "," ^ (y|>string_of_int) ^ ")\t\t{};\n" |> Buffer.add_string temporary;
        "\t\\node[morphism] (" ^ m ^ ")\tat (" ^ (x+1|>string_of_int) ^ "," ^ (y|>string_of_int) ^ ")\t\t{$" ^ m ^"$};\n" |> Buffer.add_string temporary;
        "\n" |> Buffer.add_string temporary;

        (* no labelled wires - so we can just attach them all to the hidden node *)

        (* Inputs *)
        let height  = 1.25 in (* Where 0.55 and -0.55 are the bounds of a box *)
        let spacing = height /. float (num_inputs + 1) in
        let base = float(y) -. (height/.float(2)) in
        for i = 1 to (num_inputs ) do
          let from_x = x |> string_of_int in
          let from_y = base +. float i *. spacing |> string_of_float in
          let to_x = x + 2 |> string_of_int in
          let to_y = from_y in
          "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
        done;

        let spacing' = height /. (float(num_outputs + 1)) in
        for i = 1 to (num_outputs) do
          let from_x = float(x + 2) +.height |> string_of_float in
          let from_y = base +. float i *. spacing' |> string_of_float in
          let to_x = float(x + 2) +. height +. height |> string_of_float in
          let to_y = from_y in
          "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
        done;

        Hashtbl.add morphismLocations m ((x+1), y);
        let morphism_drawing = Buffer.contents temporary in
        Buffer.clear temporary;
        morphism_drawing
    | Morphism(m, Some(ins), None)  ->
        (* ins is in the style [1,x,2] -- signalling 4 inputs *)
        failwith "not implemented1"
    | Morphism(m, None, Some(outs)) -> failwith "not implemented2"
    | Morphism (m,Some(inputs), Some(outputs)) -> failwith "not implemented3"
    | Tensor(a,b)               ->
        (*  |_|--------|_a_|--------|_|   *)
        (*                                *)
        (*  |_|--------|_b_|--------|_|   *)
        let a' = draw_structurally x y a in
        let b' = draw_structurally x (y-2) b in
        a' ^ b'
    | Composition(f,g) ->
        let f' = draw_structurally x y f in
        let g' = draw_structurally (x+2) y g in
        f' ^ g'
    | Subdiagram(diagram,ins,outs) ->
        draw_structurally x y diagram

let unwrap_def_list = function
  | [] -> []
  | (Definition(b_list, w_list)::xs) -> b_list (* we can discard the tail *)

let compile_program = function
  | Program(module_list, def_list, diag) ->
          update_morphism_table (unwrap_def_list def_list);
          let body = (draw_structurally 0 0 diag)  in
          let whole = prefix ^ body ^ suffix in
          whole
