open Ast
open Hashtbl
open Buffer
open Printf
open Bitmap

(* Symbol tables for different types of diagram entity *)
(* Descriptions of all of the tables in the comments on the right are of the format: <key> | <value> *)
let morphisms         = Hashtbl.create 64         (* f | 1,3              : specifies that entry f has 1 input and 3 outputs *)
let links             = Hashtbl.create 64         (* x | y                : specifies that port x is linked to port y *)
let morphismLocations = Hashtbl.create 64         (* f | Point(x, y)      : gives location of box f *)
let nodes             = Hashtbl.create 64         (* i | (x, y)           : node i has location (x,y) *)
let morphism_names    = Hashtbl.create 64         (* f | mod_m_box_1      : box f has been renamed to internal name mod_m_box_1 *)
let wire_names        = Hashtbl.create 64         (* x | mod_wire_1       : wire x has been renamed to internal name mod_wire_1 *)
let modules           = Hashtbl.create 64         (* m | Subdiag(name,ins,outs, diag)     *)
let temporary         = Buffer.create  64         (* temporary Buffer *)

(* Default sizings for the boxes *)
let box_size     = ref 1.25 (* Size of box in tikz units *)
let pixel_b_size = ref 0.62 (* Size of box in pixels     *)
let box_spacing  = ref 10.0  (* Vertical and horizontal spacing between boxes *)

(* Counters used for renaming the variables to internal representations used for scoping *)
let hiddenNodeCount = ref 0
let inputNodeCount  = ref 0
let outputNodeCount = ref 0
let wireCount       = ref 0
let moduleMorphismCount = ref 0

(* Accessor methods for getting the counters above *)
let hiddenNode()       = hiddenNodeCount  := !hiddenNodeCount + 1; ("empty" ^ string_of_int(!hiddenNodeCount  - 1))
let inputNode()        = inputNodeCount   := !inputNodeCount  + 1; ("i"     ^ string_of_int(!inputNodeCount   - 1))
let outputNode()       = outputNodeCount  := !outputNodeCount + 1; ("o"     ^ string_of_int(!outputNodeCount  - 1))
let wire()             = wireCount        := !wireCount       + 1; ("mod_morphism_wire_" ^ string_of_int(!wireCount - 1))
let module_morph()     = moduleMorphismCount  := !moduleMorphismCount + 1; ("mod_morphism_box_" ^ string_of_int(!moduleMorphismCount- 1))

(* Option monadic bind - nearly *)
let (>>=) f = function
  | None -> f []
  | Some x -> f x

(* Prefixes for the file generation *)
let tex_open = "\\documentclass{article}\n" ^
    "\\usepackage[utf8]{inputenc}\n"    ^
    "\\usepackage{textcomp}\n"          ^
    "\\usepackage{tikz}\n"              ^
    "\\usetikzlibrary{shapes,arrows}\n" ^

    "\\title{Diagram}\n"                ^
    "%\\author{The Category Compiler}\n"^
    "\\date{}\n"                        ^

    "\\begin{document}\n"

let prefix b_size =
    "\n\n\\tikzstyle{morphism}=[minimum size =" ^ (b_size |> string_of_float )^"cm,rectangle, draw=black, fill=white, thick]\n"  ^
    "\\tikzstyle{empty}   =[circle, draw=white, fill=white, thick]\n" ^
    "\\begin{tikzpicture}\n\n"

let suffix  = "\\end {tikzpicture}\n\\end{document}\n"


(**************  PREPROCESSING ******************)

(* Automatically generate ports on boxes based on box descriptions *)
(* These then get compiled down into nodes, which can be connected using the search algorithm *)
let rec generate_nodes node_generator = function
  | 0 -> []
  | n -> let n' = abs(n) in (node_generator ()) :: generate_nodes (node_generator) (n'-1)

(* Loop through the list of morphisms, adding each one to the hashtbl *)
let rec update_morphism_table = function
  | []        -> ()
  | (Box(name, ins, outs, _)::xs) ->
          Hashtbl.add morphisms name (ins, outs);
          update_morphism_table xs

(* Wrap the ports with a port type *)
let rec type_inputs = function
  | []        -> []
  | x::xs     -> try let n = int_of_string x in (Number n):: (type_inputs xs)
                 with
                  | Failure _ -> (String x) :: (type_inputs xs)

(* Return list from Option *)
let list_of = function
  | None -> []
  | Some(xs) -> xs

(* Unwrap Point type *)
let get_coords (Point(x,y)) = (x,y)

(* Draw any wires that are left unconnected as simple inputs/outputs *)
let rec draw_dangling_wires = function
  | []    -> ""
  | x::xs -> (match x.[0] with
                | 'i' ->
                  let (x,y) = Hashtbl.find nodes x in
                  "\t\\draw[black] \t(" ^ x ^","^ y ^ ") -- (" ^ ((float_of_string x) -. !pixel_b_size |> string_of_float) ^ "," ^  y  ^ ");\n"
                | 'o' ->
                  let (x,y) = Hashtbl.find nodes x in
                  "\t\\draw[black] \t(" ^ x ^","^ y ^ ") -- (" ^ ((float_of_string x) +. !pixel_b_size |> string_of_float) ^ "," ^  y  ^ ");\n"

                | _   -> printf "WARNING: PORT TYPE INVALID %s\n" x; ""
              ) ^ (draw_dangling_wires xs)

(* Return a list of 1s for the number of ports specified *)
let rec ones = function
  | 0 -> []
  | 1 -> [Number 1]
  | n -> Number 1 :: (ones (n-1))

(* Map the function above over the list of ports as needed *)
(* e.g. if we have a box `f[2,x]` then we convert it to `f[1,1,x]` *)
let rec flatten_port_list = function
  | [] -> []
  | x::xs ->
    ( match x with
        | Number x ->  (ones x) @ (flatten_port_list xs)
        | String s ->  (String s) ::(flatten_port_list xs)
      )

let rec print_list = function
  | [] -> ()
  | [x] -> printf "%s\n" x
  | x::xs -> printf "%s," x; print_list xs

(* Hacky but useful *)
let round_to_2dp f =
  let f'  = f *. 10.0 |> int_of_float |> float_of_int in
  f' /. 10.0

let gen_inputs x y = function
  | []     -> ""
  | xs'    ->
            let xs = List.rev xs' in
            let spacing = !box_size /. float ((List.length xs) + 1) in
            let base = y -. (!pixel_b_size) in
            for i = 0 to (List.length xs - 1) do
              let curr_input = List.nth xs i in

              let from_x   = x  |> string_of_float in

              let from_y'  = (base +. float (i+1) *. spacing) in
              let from_y'' = from_y'  *. 10.0 |> int_of_float |> float_of_int in
              let from_y'''= from_y'' /. 10.0 in  (* rounded to 2dp *)
              let from_y   = from_y''' |> string_of_float in

              let to_x = x +. (!box_spacing) -. (!pixel_b_size) |> string_of_float in
              let to_y = from_y in

              let fst_x = (from_x |> float_of_string) +. (!box_spacing /. 4.0) |> string_of_float in
              let snd_x = (to_x   |> float_of_string) -. (!box_spacing /. 4.0) |> string_of_float in

              let upper_join = from_y''' +. (spacing *. 0.5) |> round_to_2dp |> string_of_float in
              let lower_join = from_y''' -. (spacing *. 0.5) |> round_to_2dp |> string_of_float in

              (match curr_input.[String.length curr_input - 1] with
                | '>' -> (* Read this as a JOIN *)
                        "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ upper_join ^ ") -- (" ^ fst_x ^ "," ^ upper_join ^ ") -- (" ^ snd_x ^ "," ^ from_y ^ ") -- ( " ^ to_x ^ "," ^ from_y ^ ");\n" ^
                        "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ lower_join ^ ") -- (" ^ fst_x ^ "," ^ lower_join ^ ") -- (" ^ snd_x ^ "," ^ from_y ^ ") -- ( " ^ to_x ^ "," ^ from_y ^ ");\n" |> Buffer.add_string temporary;
                        Hashtbl.add nodes (curr_input ^ "UPPER") (from_x, upper_join);
                        Hashtbl.add nodes (curr_input ^ "LOWER") (from_x, lower_join)
                | '<' -> (* Read this as a FORK *)
                        "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ from_y ^ ") -- (" ^ fst_x ^ "," ^ from_y ^ ") -- (" ^ snd_x ^ "," ^ upper_join ^ ") -- (" ^ to_x ^ "," ^ upper_join ^ ");\n" ^
                        "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ from_y ^ ") -- (" ^ fst_x ^ "," ^ from_y ^ ") -- (" ^ snd_x ^ "," ^ lower_join ^ ") -- (" ^ to_x ^ "," ^ lower_join ^ ");\n" |> Buffer.add_string temporary;
                        Hashtbl.add nodes curr_input (from_x, from_y);
                | _   -> Hashtbl.add nodes curr_input (to_x, to_y)
              )
            done;
            let wire_drawing = Buffer.contents temporary in
            Buffer.clear temporary;
            wire_drawing

(* Generate the output ports for a given box at location (x,y), given a list of outputs *)
let gen_outputs x y = function
  | [] -> ""
  | xs -> let flat_list = List.rev xs  in
          let num_outputs = List.length flat_list in
          let spacing = !box_size /. float (num_outputs + 1) in
          let base = y -. (!pixel_b_size) in
          for i = 0 to (List.length flat_list - 1) do
            let curr_input = List.nth flat_list i in

            let from_x = x +. !box_spacing +. !pixel_b_size |> string_of_float in
            let from_y' = (base +. float (i+1) *. spacing) in
            let from_y'' = from_y' *. 10.0 |> int_of_float |> float_of_int in
            let from_y =  from_y'' /. 10.0 |> string_of_float in

            let upper_join = (from_y |> float_of_string) +. (spacing *. 0.5) |> round_to_2dp |> string_of_float in
            let lower_join = (from_y |> float_of_string) -. (spacing *. 0.5) |> round_to_2dp |> string_of_float in

            let to_x   = x +. !box_spacing +. !pixel_b_size +. (!box_spacing /. 2.0) |> string_of_float in
            let to_y = from_y in
            let fst_x = (from_x |> float_of_string) +. (!box_spacing /. 4.0) |> string_of_float in
            let snd_x = (to_x   |> float_of_string) -. (!box_spacing /. 4.0) |> string_of_float in

            (match curr_input.[String.length curr_input - 1] with
              | '>' -> (* Read this as a JOIN *)
                  "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ upper_join ^ ") -- (" ^ fst_x ^ "," ^ upper_join ^ ") -- (" ^ snd_x ^ "," ^ from_y ^ ") -- (" ^ to_x ^ "," ^ from_y ^ ");\n" ^
                  "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ lower_join ^ ") -- (" ^ fst_x ^ "," ^ lower_join ^ ") -- (" ^ snd_x ^ "," ^ from_y ^ ") -- (" ^ to_x ^ "," ^ from_y ^ ");\n" |> Buffer.add_string temporary;
                  Hashtbl.add nodes curr_input (to_x, to_y);
              | '<' -> (* Read this as a FORK *)
                  "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ from_y ^ ") -- (" ^ fst_x ^ "," ^ from_y ^ ") -- (" ^ snd_x ^ "," ^ upper_join ^ ") -- ( " ^ to_x ^ "," ^ upper_join ^ ");\n" ^
                  "\\draw[black, rounded corners = 3pt] (" ^ from_x ^ "," ^ from_y ^ ") -- (" ^ fst_x ^ "," ^ from_y ^ ") -- (" ^ snd_x ^ "," ^ lower_join ^ ") -- ( " ^ to_x ^ "," ^ lower_join ^ ");\n" |> Buffer.add_string temporary;
                  Hashtbl.add nodes (curr_input ^ "UPPER") (to_x, upper_join);
                  Hashtbl.add nodes (curr_input ^ "LOWER") (to_x, lower_join)
              | _   -> Hashtbl.add nodes curr_input (from_x, from_y)
            )
          done;
          let wire_drawing = Buffer.contents temporary in
          Buffer.clear temporary;
          wire_drawing

(* This function returns the physical width of a diagram so that the size of the bitmap can be calculated *)
let rec width = function
  | Identity (_,_,_) -> !box_spacing +. !pixel_b_size
  | Morphism(_, _, _) -> !box_spacing +. !pixel_b_size +. !box_spacing
  | Tensor(a,b) -> max (width a) (width b)
  | Composition(a,b) -> width a +. width b
  | Subdiagram(_,diag,_,_) -> width diag

(* The structural width of a diagram *)
let rec width' = function
  | Identity (_,_,_)  -> 1
  | Morphism(_, _, _) -> 1
  | Tensor(a,b) -> max (width' a) (width' b)
  | Composition(a,b) -> width' a + width' b
  | Subdiagram(_, diag,_,_) -> width' diag

(* The structural height of a diagram *)
let rec height = function
  | Identity (_,_,_) -> 1
  | Morphism(_,_,_) -> 2
  | Tensor(a,b) -> height a + height b
  | Composition(a,b) -> max (height a) (height b)
  | Subdiagram(_, diag,_,_) -> height diag

let draw_morphism m x y styles =
  if List.mem m styles then
    ("\t\\node[" ^ m ^ "] (" ^ m ^ ")\tat (" ^ (x|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{$" ^ m ^"$};\n")
  else
    ("\t\\node[morphism] (" ^ m ^ ")\tat (" ^ (x|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{$" ^ m ^"$};\n")

(* Used for renaming morphisms in modules using a context list of boxes *)
let rec lookup e = function
  | [] -> failwith ("Could not find box " ^ e ^ " in the module definition.")
  | (Box(name,ins,outs,_))::xs  ->  if name = e then (ins,outs) else lookup e xs

(* Preprocess the descriptions of the nodes given by the parser into a compilable format *)
(* Nodes are originally given as ` None f None` or `None f Some(1,x)` etc.  *)
(* We want them in the format ` [i1,i2] f [o1, x] ` for a box f with two inputs and two outputs *)
(* nodegen is a function for generating the nodes - inputs or outputs *)
let rec fix_nodes num_nodes nodegen = function
  | None -> generate_nodes (nodegen) num_nodes
  | Some(xs) ->
  (match xs with
    | [] -> []
    | x::xs -> (match (List.hd (type_inputs [x])) with
                    | Number n -> generate_nodes (nodegen) n @ fix_nodes num_nodes nodegen (Some xs)
                    | String s -> s :: fix_nodes num_nodes nodegen (Some xs) ))

(* Replace the port names for all of the inputs and outputs to a box/morphism *)
(* This updates the morphism to the 'fixed' version where we use the definition of the box to set up any ports needed *)
let rec replace_morphism_ports = function
  | Identity (str,ins,outs)   -> Identity (str,Some(fix_nodes 1 (inputNode) ins),Some(fix_nodes 1 (outputNode) outs))
  | Morphism (m,ins,outs)     ->  if Hashtbl.mem morphisms m then
                                    let (is,os) = Hashtbl.find morphisms m in
                                    let ins'  = fix_nodes is (inputNode) ins in
                                    let outs' = fix_nodes os (outputNode) outs in
                                    Morphism(m,Some(ins'),Some(outs'))
                                  else
                                    let m' = Hashtbl.find morphism_names m in
                                    let (is,os) = Hashtbl.find morphisms m' in
                                    let ins'  = fix_nodes is (inputNode) ins in
                                    let outs' = fix_nodes os (outputNode) outs in
                                    Morphism(m,Some(ins'),Some(outs'))

  | Tensor(a,b)                  -> Tensor((replace_morphism_ports a),(replace_morphism_ports b))
  | Composition(f,g)             -> Composition((replace_morphism_ports f),(replace_morphism_ports g))
  | Subdiagram(name, diagram,ins,outs) -> Subdiagram(name, (replace_morphism_ports diagram),ins,outs)

(* Recurse over the tree updating any occurrences of subdiagrams with each box and wire renamed *)
let rec replace_subdiagrams = function
  | Identity (str,ins,outs)      -> Identity (str,ins,outs)
  | Morphism (m,ins,outs)        -> Morphism (m,ins,outs)
  | Tensor(a,b)                  -> Tensor((replace_subdiagrams a),(replace_subdiagrams b))
  | Composition(f,g)             -> Composition((replace_subdiagrams f),(replace_subdiagrams g))
  | Subdiagram(name, _,_,_) ->
      try let (Subdiagram(name', diagram',ins',outs')) = Hashtbl.find modules name in
            Subdiagram(name',diagram',ins',outs')
      with
        | Not_found -> failwith "Failed to preprocess subdiagrams. Are all modules are defined and referenced correctly?"

(* Rename wire to new internal representation of wires *)
let rename_wire x = match (List.hd (type_inputs [x])) with
  | Number n -> n |> string_of_int
  | String s -> let new_name = wire() in
                Hashtbl.add wire_names s new_name;
                new_name

let rec rename_links = function
  | []    -> []
  | (Wire(x,y))::xs -> try
                        let i = Hashtbl.find wire_names x in
                        let o = Hashtbl.find wire_names y in
                            (Wire(i,o)) :: rename_links xs
                      with
                        | Not_found -> failwith "Could not preprocess wires defined in modules. Are all of the wires called correctly?"

let rec rename_ports = function
  | Identity (str,ins,outs) -> Identity (str,ins,outs)
  | Morphism(m,ins,outs) ->  let (i,o) = ((List.length >>= ins), (List.length >>= outs)) in
                             let ins' = fix_nodes i (inputNode) ins in
                             let outs'= fix_nodes o (outputNode) outs in
                             Morphism(m, Some(ins'), Some(outs'))
  | Tensor(d1,d2)           -> Tensor(rename_ports d1, rename_ports d2)
  | Composition(d1,d2)      -> Composition(rename_ports d1, rename_ports  d2)
  | Subdiagram(name, diagram, ins,outs)  -> Subdiagram(name, rename_ports diagram, ins, outs)

(* Same as rename_morphism_ports but for modules *)
let rec rename_morphs boxes = function
  | Identity (str,ins,outs) -> Identity (hiddenNode(),Some(fix_nodes 1 (inputNode) ins),Some(fix_nodes 1 (inputNode) ins))
  | Morphism(name,ins,outs) -> let new_name = module_morph() in
                               let new_ins  = (match ins with
                                | None      -> None
                                | Some(xs)  -> Some(List.map rename_wire xs)) in
                               let new_outs = (match outs with
                                | None      -> None
                                | Some(xs)  -> Some(List.map rename_wire xs)) in
                               let (i,o) = lookup name boxes in
                               let ins' = fix_nodes i (inputNode) new_ins in
                               let outs'= fix_nodes o (outputNode) new_outs in
                               Hashtbl.add morphisms new_name (i,o);
                               Hashtbl.add morphism_names name new_name;
                               Morphism(name, Some(ins'), Some(outs'))
  | Tensor(d1,d2)           -> Tensor(rename_morphs boxes d1, rename_morphs boxes d2)
  | Composition(d1,d2)      -> Composition(rename_morphs boxes d1, rename_morphs boxes d2)
  | Subdiagram(name, diagram, ins,outs)  -> Subdiagram(name, rename_morphs boxes diagram, ins, outs)

let rec name = function
  | Identity (str,ins,outs)          -> str
  | Morphism (m, _, _)               -> m
  | Tensor (f,g)                     -> "Tensor(" ^ name f ^ ", " ^ name g ^ ")"
  | Composition (f,g)                -> "Composition(" ^ (name f) ^ "," ^ (name g) ^")"
  | Subdiagram (n, _, _, _) -> n

let rec getOutputs = function
  | Identity (str,ins,outs)-> list_of outs
  | Morphism(_,_,outs)     -> list_of outs
  | Tensor(a,b)            -> getOutputs a @ getOutputs b
  | Composition(f,g)       -> getOutputs f @ getOutputs g
  | Subdiagram(_,_,_,outs) -> list_of outs

let rec getInputs = function
  | Identity (str,ins,outs) -> list_of ins
  | Morphism(m,ins,_)     -> list_of ins
  | Tensor(a,b)           -> getInputs a @ getInputs b
  | Composition(f,g)      -> getInputs f @ getInputs g
  | Subdiagram(_,_,ins,_) -> list_of ins

let rec connect  = function
  | [], [] -> ()
  | [], _  -> ()
  | _, []  -> ()
  | (o::os),(i::is) -> if String.contains o '<' || String.contains i '>' then
                          connect (os,is)
                       else begin
                          Hashtbl.add links o i; connect (os,is)
                       end

let rec draw_structurally x y tree styles =
  match tree with
    | Identity (str,ins,outs)    ->
        let empty_left  = str ^ "i" in
        let empty_right = str ^ "o" in
        let x' = round_to_2dp x in
        let y' = round_to_2dp y in
        Hashtbl.add nodes (List.hd >>= ins) ((string_of_float x'),(string_of_float y'));
        Hashtbl.add nodes (List.hd >>= outs) ((string_of_float (x'+.(!box_spacing))), (string_of_float y'));
        "\t\\draw [black] (" ^ (x' |> string_of_float) ^ ", " ^ (y' |> string_of_float) ^ ") -- (" ^ (x' +. !box_spacing |> string_of_float) ^ "," ^ (y' |> string_of_float)^ ");\n"
    | Morphism (m,ins,outs) ->
        let morph   = draw_morphism m (x+.(!box_spacing)) y styles in
        let inps  = gen_inputs  x y (list_of ins) in
        let outps = gen_outputs x y (list_of outs) in
        Hashtbl.add morphismLocations m (Point((x+.(!box_spacing)), y));
        inps ^ outps ^ morph ^  "\n"
    | Tensor(a,b)               ->
        let b' = draw_structurally x y b styles in
        let a' = draw_structurally x (y+.(!box_spacing *. 2.0)) a styles in
        a' ^ b'
    | Composition(f,g) ->
        let f' = draw_structurally x y f styles in
        let g' = draw_structurally (x+.(width f)) y g styles in
        let outs = getOutputs f in
        let ins  = getInputs  g in
        let ls = (Hashtbl.fold (fun k v acc -> (k :: [v]) @ acc) links []) in
        connect ((Bitmap.remove ls outs),(Bitmap.remove ls ins));
        f' ^ g'
    | Subdiagram(_,diagram,_,_) ->
        draw_structurally x y diagram styles

let tup = function
  | Wire(inp, outp) -> (inp,outp)

(* When calling List.map with a function that returns type unit, Core types this as a unit list, where ocamlbuild types as unit *)
(* This function is used to make sure that both compilers will compile the maps *)
let un = function
  | _ -> ()

(* Cases that matter: inp last is < (then we use upper and lower) *)
(*                    outp last is > *)
let preprocess_explicit_link = function
  | Wire(inp,outp) -> if inp.[String.length inp - 1] = '<' then
                        if outp.[String.length outp - 1] = '>' then begin
                          Hashtbl.add links (inp ^ "UPPER") (outp ^ "UPPER");
                          Hashtbl.add links (inp ^ "LOWER") (outp ^ "LOWER")
                        end else begin

                        end
                      else begin
                        Hashtbl.add links inp outp
                      end

let unwrap_def_list = function
  | [] -> []
  | (Definition(b_list, w_list)::_) ->
      un (List.map preprocess_explicit_link (list_of w_list));
      b_list (* we can discard the tail because the list is only being used so we either have 0 or 1 definitions in the parser *)

let add_styles (Box (b,_, _, style)) = match style with
  | None -> ""
  | Some(colour, shape) ->
    "\\tikzstyle{" ^ b ^ "} = [minimum size ="^ (!box_size |> string_of_float)^"cm, thick " |> Buffer.add_string temporary;
    let shp = (match shape with
      | None -> ",rectangle"
      | Some("CIRCLE") -> ",circle"
      | Some(s)        -> printf "Trying to match shape %s\n" s; "")
    in shp |> Buffer.add_string temporary;
    let col = (match colour with
      | None -> ",draw=black, fill=white]"
      | Some("BLACK") -> ",draw=black, fill=black]\n"
      | Some("RED")   -> ",draw=red, fill=red]\n"
      | Some("BLUE")  -> ",draw=blue, fill=blue]\n"
      | Some("WHITE") -> ",draw=black, fill=white]\n"
      | _             -> failwith "Colour not recognised")
    in col |> Buffer.add_string temporary;
    let styles = Buffer.contents temporary in
    Buffer.clear temporary;
    styles

let rec get_boxes_with_styling = function
  | [] -> []
  | (Box (b,_, _, style))::xs -> (match style with
                                    | None -> get_boxes_with_styling xs
                                    | Some(_) -> b :: get_boxes_with_styling xs)

let rec extract_boxes ms bs = match (ms, bs) with
  | [], [] -> []
  | [], (Definition(b_list', _)::xs) -> b_list' @ (extract_boxes [] xs)
  | (Module(_, b_list, _, _)::xs), [] -> b_list @ (extract_boxes xs [])
  | (Module(_, b_list, _, _)::xs), (Definition(b_list', _)::xs') -> b_list @ b_list' @ (extract_boxes xs xs')

(* Get the locations of all of the ports for the bitmap *)
let rec getNodeLocations = function
  | []                         -> []
  | ((x, y)::xs) -> try
                      let (from_nx,from_ny)    = (Hashtbl.find nodes x) in
                      let (from_nx', from_ny') = ((from_nx |> float_of_string),(from_ny |> float_of_string)) in
                      let (to_nx, to_ny)       = (Hashtbl.find nodes y) in
                      let (to_nx', to_ny')     = ((to_nx   |> float_of_string),(to_ny   |> float_of_string)) in
                      Hashtbl.remove nodes x;
                      Hashtbl.remove nodes y;
                      ((to_nx',to_ny'),(from_nx', from_ny') ) :: (getNodeLocations xs)
                    with
                      | Not_found -> failwith ("Failed to link wire " ^ x ^ " to wire " ^ y ^ ". Are all of the explicit ports named correctly?")

let path_prefix (x,y) =
  "\t\\draw[black, rounded corners = 3pt] (" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ") -- "
(*, rounded corners = 3pt*)

let rec print_path = function
  | []        -> "\n"
  | [(x,y)]   -> "(" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ")"
  | (x,y)::xs -> "(" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ") -- " ^ print_path xs

let rec print_path' = function
    | [] -> printf "\n"
    | (x,y)::xs -> printf "(%f,%f) --" x y; print_path' xs

let draw = function
  | [] -> "\n"
  | xs -> let (sx,sy) = List.hd xs in
          path_prefix (sx,sy) ^ print_path (List.tl xs) ^";\n"

let rec print_links_list = function
  | [] -> ()
  | (x,y)::xs -> printf "Link x to y:\t\t (%s,%s)\n" x y; print_links_list xs

let module_to_subdiag (Module(name,box_list,wire_list',diagram)) =
  let wire_list = list_of wire_list' in
  (* Need to rename the boxes/morphisms and the links so that they don't get confused with other modules *)
  let renamed_morphs = rename_morphs box_list diagram in
  let renamed_links  = rename_links wire_list in
  (* Add all the renamed links to the hashtable*)
  un (List.map (fun wire -> let (inp, outp) = tup wire in Hashtbl.add links inp outp) renamed_links);
  let inputs  = getInputs  renamed_morphs in
  let outputs = getOutputs renamed_morphs in
  Hashtbl.add modules name (Subdiagram(name,renamed_morphs,Some(inputs),Some(outputs)))

let compile_program = function
  | Program(module_list, def_list, diag') ->
          let start = Unix.gettimeofday () in

          printf "\n\n %s \n\n" (Ast.string_of_diagram(diag'));
          (* Find the width and box sizing for the diagram *)
          let w = width' diag' in
          if w <= 4 then
            ()
            (* if there are less than 5 boxes then leave the box size and spacing at the default *)
          else
            box_size     := 5.0 /. (float w);
            pixel_b_size := !box_size *. 0.5;
            box_spacing  := !box_size *. 1.2;
          (* Module Preprocessing *)
          (* need to compile all of the modules into subdiagrams of the form Subdiagram(name,diag,ins,outs) *)
          (* We use `un` to fake a unit type for the Core compiler (explained in the comments on the `un` function) *)
          un(List.map module_to_subdiag module_list);

          let diag = replace_subdiagrams diag' in

          (* Box Preprocessing *)
          let boxes = extract_boxes module_list def_list in
          let boxes_with_styling = get_boxes_with_styling boxes in
          let styling = boxes |> List.map add_styles |> List.fold_left (^) "" in
          update_morphism_table (unwrap_def_list def_list);
          let fixed_diag = replace_morphism_ports diag in

          printf "Preprocessed diag: \n\n %s \n" (Ast.string_of_diagram fixed_diag);

          (* Draw out the body and connect all of the connectable and dangling ports *)
          let body        = (draw_structurally 0.0 5.0 fixed_diag boxes_with_styling)  in
          let links_list  = (Hashtbl.fold (fun k v acc -> (k, v) :: acc) links [])  in (* (string * string) list *)
          print_links_list links_list;
          let box_list    = (Hashtbl.fold (fun _ v acc -> (get_coords v) :: acc) morphismLocations [])  in
          let links_list' = getNodeLocations links_list in

          let dangling_ports = (Hashtbl.fold (fun k _ acc -> k :: acc) nodes []) in
          let dangling_wires = draw_dangling_wires dangling_ports in

          let paths = Bitmap.find_routes links_list' 15 (height diag) (!box_size *. 10.0) box_list in
          let string_drawing_of_wires = List.map draw paths |> List.fold_left (^) ""  in
          let whole = tex_open ^ styling ^ (prefix !box_size) ^ string_drawing_of_wires ^ dangling_wires   ^ body  ^ suffix in

          let stop = Unix.gettimeofday () in
          printf "\n\n\nExecution time: %fs\n\n\n%!" (stop -. start);

          whole
