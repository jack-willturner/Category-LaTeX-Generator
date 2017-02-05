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
let morphisms         = Hashtbl.create 64         (* f : 1 -> 3 :: f | 1,3 *)
let morphismLocations = Hashtbl.create 64         (* f | x, y *)
let nodes             = Hashtbl.create 64
let links             = Hashtbl.create 64
let temporary = Buffer.create  64

let hiddenNodeCount = ref 0
let morphismCount   = ref 100 (* extremely hacky - but reasonable to assume we will never have more than 99 nodes in a diagram given project scale*)
let wireCount       = ref 100

let hiddenNode()       = hiddenNodeCount  := !hiddenNodeCount + 1; ("empty" ^ string_of_int(!hiddenNodeCount  - 1))
let morphism()         = morphismCount    := !morphismCount   + 1; (string_of_int(!morphismCount    - 1))
let wire()             = wireCount        := !morphismCount   + 1; (string_of_int(!wireCount -1))

type port =
  | Number of int
  | String of string

let prefix = "\n\n\\tikzstyle{morphism}=[minimum size = 1.25cm,right=10mm, rectangle, draw=black, fill=white, thick]
\\tikzstyle{empty}   =[circle, draw=white, fill=white, thick]\n
\\begin{tikzpicture}\n"

let suffix  = "\\end {tikzpicture}\n\n"

let rec update_morphism_table = function
  | []        -> ()
  | (Box(name, ins, outs, _)::xs) ->
          Hashtbl.add morphisms name (Point(ins,outs));
          update_morphism_table xs

let rec type_inputs = function
  | []        -> []
  | x::xs     -> try let n = int_of_string x in (Number n):: (type_inputs xs)
                 with
                  | Failure int_of_string -> (String x) :: (type_inputs xs)



(* Function that takes 4 points, x y, x' y' and draws a line and then a rectangle between them. Then returns a list
   of morphisms in that space *)
let intersects (Point(ax,ay)) (Point(ax',ay')) (Point(bx, by)) (Point(bx',by')) =
  ax <= bx' && ax' >= bx &&
  ay <= by' && ay' >= by

(* Start by connecting two morphisms *)
let rec connect port_x port_y node_name =
  let (node_x, node_y) = Hashtbl.find nodes node_name in
  "\\draw [black] (" ^ port_x ^ "," ^ port_y ^ ") -- (" ^ node_x ^ "," ^ node_y ^ ");\n"

let rec ones = function
  | 0 -> []
  | 1 -> [Number 1]
  | n -> Number 1 :: (ones (n-1))

let rec flatten_port_list = function
  | [] -> []
  | x::xs ->
    ( match x with
        | Number x -> (ones x) @ (flatten_port_list xs)
        | String s -> (String s) :: (flatten_port_list xs)
      )

let rec generate_some_inputs input_list x y =
  match (type_inputs input_list) with
    | [] -> ""
    | xs ->
        let flat_list = flatten_port_list xs in
        let num_inputs = List.length flat_list in
        let height  = 1.25 in (* Where 0.55 and -0.55 are the bounds of a box *)
        let spacing = height /. float (num_inputs + 1) in
        let base = float(y) -. (height/.float(2)) in
        for i = 0 to (List.length flat_list - 1) do
          let curr_input = List.nth flat_list i in
          let from_x = float (x) +.1.5 |> string_of_float in
          let from_y = base +. float (i+1) *. spacing |> string_of_float in
          let to_x = x + 2 |> string_of_int in
          let to_y = from_y in
          ( match curr_input with
            | Number n ->
                "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
            | String s ->
              if Hashtbl.mem nodes s then
                let (to_x', to_y') = Hashtbl.find nodes s in
                "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x' ^ "," ^  to_y' ^ ");\n" |> Buffer.add_string temporary
              else
                Hashtbl.add nodes s (to_x, to_y);
                "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
          )
        done;
        let some_inputs = Buffer.contents temporary in
        Buffer.clear temporary;
        some_inputs

let rec generate_some_outputs input_list x y =
  match (type_inputs input_list) with
    | [] -> ""
    | xs ->
        let flat_list = flatten_port_list xs in
        let num_outputs = List.length flat_list in
        let height  = 1.25 in (* Where 0.55 and -0.55 are the bounds of a box *)
        let spacing = height /. float (num_outputs + 1) in
        let base = float(y) -. (height/.float(2)) in
        for i = 0 to (List.length flat_list - 1) do
          let curr_input = List.nth flat_list i in
          let from_x = float(x + 2) +.height |> string_of_float in
          let from_y = base +. float (i+1) *. spacing |> string_of_float in
          let to_x = float(x + 1) +. height +. height |> string_of_float in
          let to_y = from_y in
          ( match curr_input with
            | Number n ->
                "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
            | String s ->
              if Hashtbl.mem nodes s then
                let (to_x', to_y') = Hashtbl.find nodes s in
                "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x' ^ "," ^  to_y' ^ ");\n" |> Buffer.add_string temporary
              else
                Hashtbl.add nodes s (to_x, to_y);
                "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
          )
        done;
        let some_outputs = Buffer.contents temporary in
        Buffer.clear temporary;
        some_outputs

let generate_none_inputs num_inputs x y  =
  let height  = 1.25 in (* Where 0.55 and -0.55 are the bounds of a box *)
  let spacing = height /. float (num_inputs + 1) in
  let base = float(y) -. (height/.float(2)) in
  for i = 1 to (num_inputs ) do
    let from_x = x+1 |> string_of_int in
    let from_y = base +. float i *. spacing |> string_of_float in
    let to_x = x + 2 |> string_of_int in
    let to_y = from_y in
    "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
  done;
  let wire_drawing = Buffer.contents temporary in
  Buffer.clear temporary;
  wire_drawing

let generate_none_outputs num_outputs x y =
  let height  = 1.25 in (* Where 0.55 and -0.55 are the bounds of a box *)
  let base = float(y) -. (height/.float(2)) in
  let spacing' = height /. (float(num_outputs + 1)) in
  for i = 1 to (num_outputs) do
    let from_x = float(x + 2) +.height |> string_of_float in
    let from_y = base +. float i *. spacing' |> string_of_float in
    let to_x = float(x + 1) +. height +. height |> string_of_float in
    let to_y = from_y in
    "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
  done;
  let wire_drawing = Buffer.contents temporary in
  Buffer.clear temporary;
  wire_drawing

let draw_morphism m x y =
  ("\t\\node[morphism] (" ^ m ^ ")\tat (" ^ (x+1|>string_of_int) ^ "," ^ (y|>string_of_int) ^ ")\t\t{$" ^ m ^"$};\n\n")

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
        let (Point(num_inputs,num_outputs))  = Hashtbl.find morphisms m in
        let morph   = draw_morphism m x y in
        let inputs  = generate_none_inputs num_inputs x y in
        let outputs = generate_none_outputs num_outputs x y in
        Hashtbl.add morphismLocations m (Point((x+1), y));
        inputs ^ morph ^ outputs
    | Morphism(m, Some(ins), None)  ->
        (* ins is in the style [1,x,2] -- signalling 4 inputs *)
        let Point(num_inputs, num_outputs) = Hashtbl.find morphisms m in
        let morph   = draw_morphism m x y in
        let inputs  = generate_some_inputs ins x y  in
        let outputs = generate_none_outputs num_outputs x y in
        Hashtbl.add morphismLocations m (Point((x+1), y));
        inputs ^ morph ^ outputs
    | Morphism(m, None, Some(outs)) ->
        let Point(num_inputs, num_outputs) = Hashtbl.find morphisms m in
        let morph = draw_morphism m x y in
        let inputs = generate_none_inputs num_inputs x y in
        let outputs = generate_some_outputs outs x y in
        Hashtbl.add morphismLocations m (Point((x+1), y));
        inputs ^ morph ^ outputs
    | Morphism (m,Some(inputs), Some(outputs)) -> failwith "not implemented3"
    | Tensor(a,b)               ->
        let a' = draw_structurally x y a in
        let b' = draw_structurally x (y-2) b in
        a' ^ b'
    | Composition(f,g) ->
        let f' = draw_structurally x y f in
        let g' = draw_structurally (x+3) y g in
        f' ^ g'
    | Subdiagram(diagram,ins,outs) ->
        draw_structurally x y diagram

let tup = function
  | Wire(inp, outp) -> (inp,outp)

let unwrap_def_list = function
  | [] -> []
  | (Definition(b_list, w_list)::xs) ->
      List.map (fun f wire -> let (inp, outp) = tup wire in Hashtbl.add links outp inp) w_list;
      b_list (* we can discard the tail *)

let add_styles (Box (b,_, _, style)) = match style with
  | None -> ""
  | Some(shape, colour) ->
    "\tikzstyle{" ^ b ^ "} = [minimum size = 1.25cm, right = 10mm, thick " |> Buffer.add_string temporary;
    let shp = (match shape with
      | None -> ",rectangle"
      | Some("CIRCLE") -> ",circle"
      | _              -> failwith "Shape not recognised")
    in shp |> Buffer.add_string temporary;
    let col = (match colour with
      | None -> ",draw=black, fill=white]"
      | Some("BLACK") -> ",draw=black, fill=black]"
      | Some("RED")   -> ",draw=red, fill=blue]"
      | Some("BLUE")  -> ",draw=blue, fill=blue]"
      | _             -> failwith "Colour not recognised")
    in col |> Buffer.add_string temporary;
    let styles = Buffer.contents temporary in
    Buffer.clear temporary;
    styles


let rec rename_morphs = function
  | Identity                -> Identity
  | Morphism(name,ins,outs) -> let new_name = ((morphism()) ^ name) in
                               let new_ins  = (match ins with
                                | None      -> None
                                | Some(xs)  -> Some(List.map (fun x -> (wire() ^ x)) xs) ) in
                               let new_outs = (match outs with
                                | None      -> None
                                | Some(xs)  -> Some(List.map (fun x -> (wire() ^ x)) xs) )in
                               Morphism(new_name, new_ins, new_outs)
  | Tensor(d1,d2)           -> Tensor(rename_morphs d1, rename_morphs d2)
  | Composition(d1,d2)      -> Composition(rename_morphs d1, rename_morphs d2)
  | Subdiagram(diagram, ins,outs)     -> Subdiagram(rename_morphs diagram, ins, outs)

let rec extract_boxes ms bs = match (ms, bs) with
  | [], [] -> []
  | [], (Definition(b_list', w_list')::xs) -> b_list' @ (extract_boxes [] xs)
  | (Module(name, b_list, w_list, diag)::xs), [] -> b_list @ (extract_boxes xs [])
  | (Module(name, b_list, w_list, diag)::xs), (Definition(b_list', w_list')::xs') -> b_list @ b_list' @ (extract_boxes xs xs')


let compile_program = function
  | Program(module_list, def_list, diag) ->
          let styling = extract_boxes module_list def_list |> List.map add_styles |> List.fold_left (^) "" in
          update_morphism_table (unwrap_def_list def_list);
          let body = (draw_structurally 0 0 diag)  in
          let whole = prefix  ^ body ^ suffix in
          whole
