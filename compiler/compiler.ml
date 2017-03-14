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
let morphism_names    = Hashtbl.create 64
let wire_names        = Hashtbl.create 64
let modules           = Hashtbl.create 64
let temporary         = Buffer.create  64         (* temporary Buffer *)

let ports = Hashtbl.create 64

let box_size     = ref 1.25
let pixel_b_size = ref 0.62 (* always half of the box size since the boxes are placed from the centre *)
let box_spacing  = ref 1.0

let hiddenNodeCount = ref 0
let inputNodeCount  = ref 0
let outputNodeCount = ref 0

let wireCount = ref 0
let moduleMorphismCount = ref 0

let hiddenNode()       = hiddenNodeCount  := !hiddenNodeCount + 1; ("empty" ^ string_of_int(!hiddenNodeCount  - 1))
let inputNode()        = inputNodeCount   := !inputNodeCount  + 1; ("i" ^ string_of_int(!inputNodeCount - 1))
let outputNode()       = outputNodeCount  := !outputNodeCount + 1; ("o" ^ string_of_int(!outputNodeCount - 1))
let module_morph()     = moduleMorphismCount  := !moduleMorphismCount + 1; ("mod_morphism_box_" ^ string_of_int(!moduleMorphismCount- 1))
let wire()             = wireCount  := !wireCount + 1; ("mod_morphism_box_" ^ string_of_int(!wireCount- 1))

(* Option monadic bind - nearly *)
let (>>=) f = function
  | None -> f []
  | Some x -> f x

type port =
  | Number of int
  | String of string

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

let rec generate_input_nodes = function
  | 0 -> []
  | n -> inputNode() :: generate_input_nodes (n-1)

let rec generate_output_nodes = function
  | 0 -> []
  | n -> outputNode() :: generate_output_nodes (n-1)

let rec update_morphism_table = function
  | []        -> ()
  | (Box(name, ins, outs, _)::xs) ->
          Hashtbl.add morphisms name (ins, outs);
          update_morphism_table xs

let rec type_inputs = function
  | []        -> []
  | x::xs     -> try let n = int_of_string x in (Number n):: (type_inputs xs)
                 with
                  | Failure _ -> (String x) :: (type_inputs xs)

let type_port x  = try let n = int_of_string x in (Number n)
                   with
                    | Failure _ -> (String x)

let list_of = function
  | None -> []
  | Some(xs) -> xs

let get_coords (Point(x,y)) = (x,y)

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

let rec ones = function
  | 0 -> []
  | 1 -> [Number 1]
  | n -> Number 1 :: (ones (n-1))

let rec flatten_port_list = function
  | [] -> []
  | x::xs ->
    ( match x with
        | Number x ->  (ones x) @ (flatten_port_list xs)
        | String s ->  (String s) ::(flatten_port_list xs)
      )

let gen_inputs x y = function
  | []    -> ""
  | xs'    ->
            let xs = List.rev xs' in
            let spacing = !box_size /. float ((List.length xs) + 1) in
            let base = y -. (!pixel_b_size) in
            for i = 0 to (List.length xs - 1) do
              let curr_input = List.nth xs i in

              (* let from_x = x  |> string_of_float in *)
              let from_y' = (base +. float (i+1) *. spacing) in
              let from_y'' = from_y' *. 10.0 |> int_of_float |> float_of_int in
              let from_y = from_y'' /. 10.0 |> string_of_float in

              (*let to_x' = x +. (!box_spacing) -. (!pixel_b_size) in *)
              (* let to_x'' = to_x' *. 10.0 |> int_of_float |> float_of_int in*)
              (*let to_x = to_x'' /. 10.0 |> string_of_float in *)
              let to_x = x +. (!box_spacing) -. (!pixel_b_size) |> string_of_float in
              let to_y = from_y in

              (match curr_input with
                | str -> printf "\tAdding node %s to hashtbl\n" str; Hashtbl.add nodes str (to_x, to_y)
              )
            done;
            let wire_drawing = Buffer.contents temporary in
            Buffer.clear temporary;
            wire_drawing

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
            (*let to_x   = x +. !box_spacing +. !pixel_b_size +. (!box_spacing /. 2.0) |> string_of_float in *)
            (*let to_y   = from_y in *)
            ( match curr_input with
              | str -> printf "\tAdding node %s to hashtbl\n" str; Hashtbl.add nodes str (from_x, from_y)
            )
          done;
          let wire_drawing = Buffer.contents temporary in
          Buffer.clear temporary;
          wire_drawing

let rec width = function
  | Identity  -> 2.0
  | Morphism(_, _, _) -> !box_spacing +. !pixel_b_size +. !box_spacing
  | Tensor(a,b) -> max (width a) (width b)
  | Composition(a,b) -> width a +. width b
  | Subdiagram(_,diag,_,_) -> width diag

let rec width' = function
  | Identity  -> 1
  | Morphism(_, _, _) -> 1
  | Tensor(a,b) -> max (width' a) (width' b)
  | Composition(a,b) -> width' a + width' b
  | Subdiagram(_, diag,_,_) -> width' diag

let rec height = function
  | Identity -> 1
  | Morphism(_,_,_) -> 2
  | Tensor(a,b) -> height a + height b
  | Composition(a,b) -> max (height a) (height b)
  | Subdiagram(_, diag,_,_) -> height diag

let draw_morphism m x y styles =
  if List.mem m styles then
    ("\t\\node[" ^ m ^ "] (" ^ m ^ ")\tat (" ^ (x|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{$" ^ m ^"$};\n")
  else
    ("\t\\node[morphism] (" ^ m ^ ")\tat (" ^ (x|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{$" ^ m ^"$};\n")

let rec lookup e = function
  | [] -> failwith "Could not find elem in list"
  | (Box(name,ins,outs,_))::xs  ->  if name = e then (ins,outs) else lookup e xs

let rec fix_inputs (ins,outs) = function
  | None -> generate_input_nodes ins
  | Some(xs) ->
  (match xs with
    | [] -> []
    | x::xs -> (match type_port x with
                    | Number n -> generate_input_nodes n @ fix_inputs (ins,outs) (Some xs)
                    | String s -> s :: fix_inputs (ins,outs) (Some xs) ))

let rec fix_outputs (ins,outs) = function
  | None -> generate_output_nodes outs
  | Some(xs) ->
  (match xs with
    | [] -> []
    | x::xs -> (match type_port x with
                    | Number n -> generate_output_nodes n @ fix_outputs (ins,outs) (Some xs)
                    | String s -> s :: fix_outputs (ins,outs) (Some xs) ))

let rec replace_morphism_ports = function
  | Identity                  -> Identity
  | Morphism (m,ins,outs)     ->  if Hashtbl.mem morphisms m then
                                    let (is,os) = Hashtbl.find morphisms m in
                                    let ins'  = fix_inputs (is,os) ins in
                                    let outs' = fix_outputs (is,os) outs in
                                    Hashtbl.add ports m (ins',outs');
                                    Morphism(m,Some(ins'),Some(outs'))
                                  else
                                    let r = 10 in
                                    printf "Looking up %s\n" m;
                                    let m' = Hashtbl.find morphism_names m in
                                    printf "Looking up %s\n" m';
                                    let (is,os) = Hashtbl.find morphisms m' in
                                    let ins'  = fix_inputs (is,os) ins in
                                    let outs' = fix_outputs (is,os) outs in
                                    Hashtbl.add ports m (ins',outs');
                                    Morphism(m,Some(ins'),Some(outs'))

  | Tensor(a,b)                  -> Tensor((replace_morphism_ports a),(replace_morphism_ports b))
  | Composition(f,g)             -> Composition((replace_morphism_ports f),(replace_morphism_ports g))
  | Subdiagram(name, diagram,ins,outs) -> Subdiagram(name, (replace_morphism_ports diagram),ins,outs)

let rec replace_subdiagrams = function
  | Identity                  -> Identity
  | Morphism (m,ins,outs)     -> Morphism (m,ins,outs)
  | Tensor(a,b)                  -> Tensor((replace_subdiagrams a),(replace_subdiagrams b))
  | Composition(f,g)             -> Composition((replace_subdiagrams f),(replace_subdiagrams g))
  | Subdiagram(name, _,_,_) ->
      try let (Subdiagram(name', diagram',ins',outs')) = Hashtbl.find modules name in
            Subdiagram(name',diagram',ins',outs')
      with
        | Not_found -> failwith "Could not replace subdiagrams"

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
                        | Not_found -> failwith "Could not find wires"

let rec rename_ports = function
  | Identity                -> Identity
  | Morphism(m,ins,outs) ->  let (i,o) = ((List.length >>= ins), (List.length >>= outs)) in
                             let ins' = fix_inputs (i,o) ins in
                             let outs'= fix_outputs (i,o) outs in
                             Morphism(m, Some(ins'), Some(outs'))
  | Tensor(d1,d2)           -> Tensor(rename_ports d1, rename_ports d2)
  | Composition(d1,d2)      -> Composition(rename_ports d1, rename_ports  d2)
  | Subdiagram(name, diagram, ins,outs)  -> Subdiagram(name, rename_ports diagram, ins, outs)

let rec rename_morphs boxes = function
  | Identity                -> Identity
  | Morphism(name,ins,outs) -> let new_name = module_morph() in
                               let new_ins  = (match ins with
                                | None      -> None
                                | Some(xs)  -> Some(List.map rename_wire xs)) in
                               let new_outs = (match outs with
                                | None      -> None
                                | Some(xs)  -> Some(List.map rename_wire xs)) in
                               let (i,o) = lookup name boxes in
                               let ins' = fix_inputs (i,o) new_ins in
                               let outs'= fix_outputs (i,o) new_outs in
                               printf "Adding %s --> %s\n" name new_name;
                               Hashtbl.add morphisms new_name (i,o);
                               Hashtbl.add morphism_names name new_name;
                               Morphism(name, Some(ins'), Some(outs'))
  | Tensor(d1,d2)           -> Tensor(rename_morphs boxes d1, rename_morphs boxes d2)
  | Composition(d1,d2)      -> Composition(rename_morphs boxes d1, rename_morphs boxes d2)
  | Subdiagram(name, diagram, ins,outs)  -> Subdiagram(name, rename_morphs boxes diagram, ins, outs)

let rec name = function
  | Identity                         -> "Identity"
  | Morphism (m, _, _)          -> m
  | Tensor (f,g)                     -> "Tensor(" ^ name f ^ ", " ^ name g ^ ")"
  | Composition (f,g)                -> "Composition(" ^ (name f) ^ "," ^ (name g) ^")"
  | Subdiagram (n, _, _, _) -> n

let rec getOutputs = function
  | Identity             -> []
  | Morphism(_,_,outs) -> list_of outs
  | Tensor(a,b)          -> getOutputs a @ getOutputs b
  | Composition(f,g)     -> getOutputs f @ getOutputs g
  | Subdiagram(_,_,_,outs) -> list_of outs

let rec getInputs = function
  | Identity             -> []
  | Morphism(m,ins,_) -> printf "\nGetting inputs from %s\n" m; list_of ins
  | Tensor(a,b)          -> getInputs a @ getInputs b
  | Composition(f,g)     -> printf "\nComposition of %s ; %s\n" (name f) (name g); getInputs f @ getInputs g
  | Subdiagram(_,_,ins,_) -> list_of ins

let rec connect  = function
  | [], [] -> ()
  | [], _ -> ()
  | _, [] -> ()
  | (o::os),(i::is) -> Hashtbl.add links o i; connect (os,is)

let rec draw_structurally x y tree styles =
  (* Create a grid of max_width * max_height *)
  (* Scan left to right adding morphisms to boxes *)
  match tree with
    | Identity                  ->
        let empty_left = hiddenNode() in
        let empty_right = hiddenNode() in
        "\t\\node (" ^ empty_left  ^ ")\tat (" ^ (x |> string_of_float) ^ "," ^ (y |> string_of_float) ^ ")\t\t{}\n" ^
        "\t\\node (" ^ empty_right ^ ")\tat (" ^ ((x+.(!box_spacing))|>string_of_float) ^ "," ^ (y|>string_of_float) ^ ")\t\t{}\n" ^
        "\t\\draw [black] (" ^ empty_left ^ ".east) -- (" ^ empty_right ^ ".west);\n"
    | Morphism (m,ins,outs) ->
        let morph   = draw_morphism m (x+.(!box_spacing)) y styles in
        printf "Adding morph %s to table\n" m;
        let inps  = gen_inputs  x y (list_of ins) in
        let outps = gen_outputs x y (list_of outs) in
        printf "\n";
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
        (* Diagram needs to be updated here to have new port names *)
        draw_structurally x y (diagram) styles

let tup = function
  | Wire(inp, outp) -> (inp,outp)

let un = function
  | _ -> ()

let unwrap_def_list = function
  | [] -> []
  | (Definition(b_list, w_list)::_) ->
      printf "Length of wire list: %i\n" (List.length (list_of w_list));
      un (List.map (fun wire -> let (inp, outp) = tup wire in printf "Adding link %s -> %s\n" inp outp; Hashtbl.add links inp outp) (list_of w_list));
      b_list (* we can discard the tail because the list is only being used so we either have 0 or 1 definitions *)

let add_styles (Box (b,_, _, style)) = match style with
  | None -> ""
  | Some(colour, shape) ->
    "\\tikzstyle{" ^ b ^ "} = [minimum size ="^ (!box_size |> string_of_float)^"cm, right = 10mm, thick " |> Buffer.add_string temporary;
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

let rec getNodeLocations = function
  | []                         -> []
  | ((x, y)::xs) -> try
                      printf "linking %s to %s\n" x y;
                      let (from_nx,from_ny)    = (Hashtbl.find nodes x) in
                      let (from_nx', from_ny') = ((from_nx |> float_of_string),(from_ny |> float_of_string)) in
                      let (to_nx, to_ny)       = (Hashtbl.find nodes y) in
                      let (to_nx', to_ny')     = ((to_nx   |> float_of_string),(to_ny   |> float_of_string)) in
                      Hashtbl.remove nodes x;
                      Hashtbl.remove nodes y;
                      ((to_nx',to_ny'),(from_nx', from_ny') ) :: (getNodeLocations xs)
                    with
                      | Not_found -> failwith "Could not get node loc"

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
  | ((x,y),(x',y'))::xs -> printf "Link x to y:\t\t (%f,%f) -- (%f,%f)\n" x y x' y'; print_links_list xs

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
          (* Find the width and box sizing for the diagram *)
          let w = width' diag' in
          if w <= 4 then
            ()  (* if there are less than 5 boxes then leave the box size and spacing at the default *)
          else
            box_size     := 5.0 /. (float w);
            pixel_b_size := !box_size *. 0.5;
            box_spacing  := !box_size *. 1.2;

          (* Module Preprocessing *)
          (* need to compile all of the modules into subdiagrams of the form Subdiagram(name,diag,ins,outs) *)
          un(List.map module_to_subdiag module_list);

          let diag = replace_subdiagrams diag' in

          (* Box Preprocessing *)
          let boxes = extract_boxes module_list def_list in
          let boxes_with_styling = get_boxes_with_styling boxes in
          let styling = boxes |> List.map add_styles |> List.fold_left (^) "" in
          update_morphism_table (unwrap_def_list def_list);
          let fixed_diag = replace_morphism_ports diag in

          printf "\n--------------\n%s\n--------------\n" (name fixed_diag);

          (* Draw out the body and connect all of the connectable and dangling ports *)
          let body = (draw_structurally 0.0 5.0 fixed_diag boxes_with_styling)  in
          let links_list = (Hashtbl.fold (fun k v acc -> (k, v) :: acc) links [])  in (* (string * string) list *)
          let box_list   = (Hashtbl.fold (fun _ v acc -> (get_coords v) :: acc) morphismLocations [])  in
          let links_list' = getNodeLocations links_list in

          let dangling_ports = (Hashtbl.fold (fun k _ acc -> k :: acc) nodes []) in
          let dangling_wires = draw_dangling_wires dangling_ports in

          let paths = Bitmap.find_routes links_list' 15 (height diag) (!box_size *. 10.0) box_list in
          let string_drawing_of_wires = List.map draw paths |> List.fold_left (^) ""  in
          let whole = tex_open ^ styling ^ (prefix !box_size) ^ string_drawing_of_wires ^ dangling_wires   ^ body  ^ suffix in
          whole
