open Ast
open Hashtbl
open Buffer
open Printf
open Bitmap

(* First, set up a hashtable of morphisms *)
let morphisms         = Hashtbl.create 64         (* f : 1 -> 3 :: f | 1,3 - symbol table *)
let links             = Hashtbl.create 64         (* symbol table for links *)
let morphismLocations = Hashtbl.create 64         (* f | Point(x, y) *)
let nodes             = Hashtbl.create 64         (* essentially link locations *)
let temporary         = Buffer.create  64         (* temporary Buffer *)

let height = 1.25

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
          Hashtbl.add morphisms name (ins, outs);
          update_morphism_table xs

let rec type_inputs = function
  | []        -> []
  | x::xs     -> try let n = int_of_string x in (Number n):: (type_inputs xs)
                 with
                  | Failure int_of_string -> (String x) :: (type_inputs xs)

(* Function that takes 4 points, x y, x' y' and draws a line and then a rectangle between them. Then returns a list
   of morphisms in that space *)
let intersect (Point(ax,ay)) (Point(ax',ay')) (Point(bx, by)) (Point(bx',by')) =
  ax <= bx' && ax' >= bx &&
  ay <= by' && ay' >= by

let rec intersects src dest list_of_morphs = match list_of_morphs with
  | []                      -> []
  | ((name, Point(x,y))::xs) -> if intersect src dest (Point(x,y)) (Point(x+.1.25, y+.1.25))
                                then (name, Point(x,y)) ::(intersects src dest xs)
                                else intersects src dest xs

let get_coords (Point(x,y)) = (x,y)

let draw src dest =
  (* iterate through hshtbl mLocs - return list of intersections *)
  let l_intersects = intersects src dest (Hashtbl.fold (fun k v acc -> (k, v) :: acc) morphismLocations []) in
  let (port_x, port_y) = get_coords src in
  let (node_x, node_y) = get_coords dest in
  (match l_intersects with
    | []                      ->  "\\draw [black] (" ^ (port_x -. 0.5 |> string_of_float) ^ "," ^ (port_y |> string_of_float) ^ ") -- (" ^ (node_x |> string_of_float) ^ "," ^ (node_y |> string_of_float) ^ ");\n"
    | (name, Point(x,y))::xs  -> (* get max and minimum y of the list *)
                          let (from_x, from_y) = get_coords src in
                          let (to_x, to_y) = get_coords dest in
                          let max_y = y in
                          "\\draw[black, rounded corners = 8pt] (" ^ (from_x |> string_of_float) ^ "," ^ (from_y |>string_of_float) ^ ") -- (" ^
                              ((from_x+.1.0) |> string_of_float) ^ "," ^ (max_y |> string_of_float) ^ ") -- (" ^ ((to_x-.1.0) |> string_of_float) ^ "," ^ (max_y |> string_of_float) ^ ") -- (" ^ (to_x |> string_of_float) ^ "," ^ (to_y |> string_of_float) ^ ")\n")

let rec connect = function
    | []              -> ""
    | (outp, inp)::xs -> draw outp inp ^ connect xs

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
        let base = y -. (height/.float(2)) in
        for i = 0 to (List.length flat_list - 1) do
          let curr_input = List.nth flat_list i in
          let from_x = x +. 1.5 |> string_of_float in
          let from_y = base +. float (i+1) *. spacing |> string_of_float in
          let to_x = x +. 2.0 |> string_of_float in
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
        let base = y -. (height/.float(2)) in
        for i = 0 to (List.length flat_list - 1) do
          let curr_input = List.nth flat_list i in
          let from_x = x +. 2.0 +.height              |> string_of_float in
          let from_y = base +. float (i+1) *. spacing |> string_of_float in
          let to_x = x +. 1.0 +. height +. height     |> string_of_float in
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
  let base = y -. (height/.2.0) in
  for i = 1 to (num_inputs ) do
    let from_x = x+.1.0 |> string_of_float in
    let from_y = base +. float i *. spacing |> string_of_float in
    let to_x = x +. 2.0 |> string_of_float in
    let to_y = from_y in
    "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
  done;
  let wire_drawing = Buffer.contents temporary in
  Buffer.clear temporary;
  wire_drawing

let generate_none_outputs num_outputs x y =
  let height  = 1.25 in (* Where 0.55 and -0.55 are the bounds of a box *)
  let base = y -. (height/.float(2)) in
  let spacing' = height /. (float(num_outputs + 1)) in
  for i = 1 to (num_outputs) do
    let from_x = x +. 2.0 +.height |> string_of_float in
    let from_y = base +. float i *. spacing' |> string_of_float in
    let to_x = x +. 1.0 +. height +. height |> string_of_float in
    let to_y = from_y in
    "\t\\draw[black] \t(" ^ from_x ^","^ from_y ^ ") -- (" ^ to_x ^ "," ^  to_y ^ ");\n" |> Buffer.add_string temporary
  done;
  let wire_drawing = Buffer.contents temporary in
  Buffer.clear temporary;
  wire_drawing

let rec width = function
  | Identity  -> 2.0
  | Morphism(m, _, _) -> 3.25
  | Tensor(a,b) -> max (width a) (width b)
  | Composition(a,b) -> width a +. width b

let rec width' = function
  | Identity  -> 1
  | Morphism(m, _, _) -> 1
  | Tensor(a,b) -> max (width' a) (width' b)
  | Composition(a,b) -> width' a + width' b

let rec height = function
  | Identity -> 1
  | Morphism(m,_,_) -> 1
  | Tensor(a,b) -> height a + height b
  | Composition(a,b) -> max (height a) (height b)

let draw_morphism m x y =
  ("\t\\node[morphism] (" ^ m ^ ")\tat (" ^ (x+.1.0|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{$" ^ m ^"$};\n\n")

let rec draw_structurally x y tree =
  (* Create a grid of max_width * max_height *)
  (* Scan left to right adding morphisms to boxes *)
  match tree with
    | Identity                  ->
        let empty_left = hiddenNode() in
        let empty_right = hiddenNode() in
        "\t\\node (" ^ empty_left  ^ ")\tat (" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ")\t\t{}\n" ^
        "\t\\node (" ^ empty_right ^ ")\tat (" ^ ((x+.2.0)|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{}\n" ^
        "\t\\draw [black] (" ^ empty_left ^ ".east) -- (" ^ empty_right ^ ".west);\n"
    | Morphism(m, None, None)       ->
        let (num_inputs,num_outputs) = Hashtbl.find morphisms m in
        let morph   = draw_morphism m x y in
        let inputs  = generate_none_inputs num_inputs x y in
        let outputs = generate_none_outputs num_outputs x y in
        Hashtbl.add morphismLocations m (Point((x+.1.0), y));
        inputs ^ morph ^ outputs
    | Morphism(m, Some(ins), None)  ->
        (* ins is in the style [1,x,2] -- signalling 4 inputs *)
        let (num_inputs, num_outputs) = Hashtbl.find morphisms m in
        let morph   = draw_morphism m x y in
        let inputs  = generate_some_inputs ins x y  in
        let outputs = generate_none_outputs num_outputs x y in
        Hashtbl.add morphismLocations m (Point((x+.1.0), y));
        inputs ^ morph ^ outputs
    | Morphism(m, None, Some(outs)) ->
        let (num_inputs, num_outputs) = Hashtbl.find morphisms m in
        let morph = draw_morphism m x y in
        let inputs = generate_none_inputs num_inputs x y in
        let outputs = generate_some_outputs outs x y in
        Hashtbl.add morphismLocations m (Point((x+.1.0), y));
        inputs ^ morph ^ outputs
    | Morphism (m,Some(inputs), Some(outputs)) -> failwith "not implemented3"
    | Tensor(a,b)               ->
        let a' = draw_structurally x y a in
        let b' = draw_structurally x (y-.2.0) b in
        a' ^ b'
    | Composition(f,g) ->
        let f' = draw_structurally x y f in
        let g' = draw_structurally (x+.(width f)) y g in
        f' ^ g'
    | Subdiagram(diagram,ins,outs) ->
        draw_structurally x y diagram

let tup = function
  | Wire(inp, outp) -> (inp,outp)

let unwrap_def_list = function
  | [] -> []
  | (Definition(b_list, w_list)::xs) ->
      List.map (fun wire -> let (inp, outp) = tup wire in Hashtbl.add links outp inp) w_list;
      b_list (* we can discard the tail because the list is only being used so we either have 0 or 1 definitions *)

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

let rec getNodeLocations = function
  | []                         -> []
  | ((x, y)::xs) -> let (from_nx,from_ny) = (Hashtbl.find nodes x) in
                    let (from_nx', from_ny') = ((from_nx |>float_of_string),(from_ny |> float_of_string)) in
                    let (to_nx, to_ny)    = (Hashtbl.find nodes y) in
                    let (to_nx', to_ny') = ((to_nx |>float_of_string),(to_ny |> float_of_string)) in
    ((from_nx', from_ny'),(to_nx',to_ny') ) :: (getNodeLocations xs)

let path_suffix corners = match corners with
  | []        -> ""
  | (x,y)::xs -> " -- (" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ")"

let path_prefix (x,y) =
  "\t\\draw[black, rounded corners = 8pt] (" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ")"

let draw = function
  | [] -> "\n"
  | xs -> let (sx,sy) = List.hd xs in
          let (gx,gy) = List.rev xs |> List.hd in
          path_prefix (sx,sy) ^ path_suffix xs ^ path_suffix [(gx,gy)]

let print_links_list = function
  | [] -> ()
  | ((x,y),(x',y'))::xs -> printf "Link x to y:\t\t (%f,%f) -- (%f,%f)\n" x y x' y'


let compile_program = function
  | Program(module_list, def_list, diag) ->
          let styling = extract_boxes module_list def_list |> List.map add_styles |> List.fold_left (^) "" in
          update_morphism_table (unwrap_def_list def_list);
          let body = (draw_structurally 0.0 0.0 diag)  in
          let links_list = (Hashtbl.fold (fun k v acc -> (k, v) :: acc) links [])  in (* (string * string) list *)
          let box_list   = (Hashtbl.fold (fun k v acc -> (get_coords v) :: acc) morphismLocations [])  in
          let links_list' = getNodeLocations links_list in
          print_links_list links_list';
          (*let links' = connect links_list' in *)
          let links_list'' = [((3.5,0.208333333333),(7.5,0.208333333333))] in
          let corners = Bitmap.find_routes links_list'' (width' diag) (height diag) box_list in
          let string_drawing_of_wires = List.map draw corners |> List.fold_left (^ ) ""  in
          let whole = prefix  ^ body ^ string_drawing_of_wires  ^ suffix in
          whole
