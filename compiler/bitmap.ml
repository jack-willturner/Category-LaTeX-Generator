open Ast
open Printf
open List
open PrioQueue

(* Graph is a hashtable of adjacency lists *)
let graph = Hashtbl.create 10000

(* Convert a coordinate (x,y) to a string : e.g. (1,1) becomes x1y1 *)
let string_of_coord x y = "x" ^ (x |> string_of_int)  ^ "y" ^ (y |> string_of_int)

(* Convert a string x0y0 to a coordinate (0,0) etc. *)
let coord_of_string str =
 (* get rid of leading 'x' *)
 let tail   = String.sub str 1 ((String.length str) - 1) in
 let coords = String.split_on_char 'y' tail in
 (* list should have length 2 - x coord and y coord *)
 (match coords with
   | x::xs  -> ((x |> int_of_string),((List.hd xs) |> int_of_string))
   | _     -> failwith "coord_of_string")



(* Each adjacency list is made up of 8 elements describing the successors of the square clockwise  *)
let generate_adjacency_lists x y =
 for i = 0 to x do
   for j = 0 to y do
     let name' = string_of_coord i j in
     let succ =
     (match (i, j) with
       | 0,0   ->  [Some("x0y1");Some("x1y1");Some("x1y0");None;None;None;None;None]
       | 0,n   -> (* along the left hand side of the grid *)
                   if n = y
                   then [None;None;Some(string_of_coord 1 n);Some(string_of_coord 1 (n-1));Some(string_of_coord 0 (n-1));None;None;None]
                   else [Some(string_of_coord 0 (n+1)); Some(string_of_coord 1 (n+1)); Some(string_of_coord 1 n); Some(string_of_coord 1 (n-1)); Some(string_of_coord 0 (n-1)); None; None; None]
       | n,0   -> (* along the bottom line of the grid *)
                   if n = x
                   then [Some(string_of_coord n 1); None; None; None; None; None;Some(string_of_coord (n-1) 0); Some(string_of_coord (n-1) 1)]
                   else [Some(string_of_coord n 1); Some(string_of_coord (n+1) 1); Some(string_of_coord (n+1) 0); None; None; None; Some(string_of_coord (n-1) 0); Some(string_of_coord (n-1) 1)  ]
       | x', y' ->  (* check if on top or right edge *)
                   if x' = x then
                     if y' = y then
                       (* top right hand corner *)
                       [None; None; None; None; Some(string_of_coord x' (y'-1)); Some(string_of_coord (x'-1) (y'-1)); Some(string_of_coord (x'-1) y); None ]
                     else
                       (* right hand side of grid but not in either corner *)
                       [Some(string_of_coord x' (y'+1)); None; None; None; Some(string_of_coord x' (y'-1)); Some(string_of_coord (x'-1) (y'-1)); Some(string_of_coord (x'-1) y); Some(string_of_coord (x'-1) (y+1)) ]
                   else
                     if y' = y then
                       (* along the top of the grid but not near either corner *)
                       [None; None;Some(string_of_coord (x'+1) y'); Some(string_of_coord (x'+1) (y'-1));Some(string_of_coord x' (y'-1));Some(string_of_coord (x'-1) (y'-1)); Some(string_of_coord (x'-1) y);   None]
                     else
                       (* somewhere in the centre of the grid *)
                       [Some(string_of_coord x' (y'+1));
                        Some(string_of_coord (x'+1) (y'+1));
                        Some(string_of_coord (x'+1) y');
                        Some(string_of_coord (x'+1) (y'-1));
                        Some(string_of_coord x' (y'-1));
                        Some(string_of_coord (x'-1) (y'-1));
                        Some(string_of_coord (x'-1) y');
                        Some(string_of_coord (x'-1) (y'+1))]
     ) in
     Hashtbl.add graph name' {
                               name = name';
                               xLoc = i;
                               yLoc = j;
                               status = Free;
                               successors = succ;
                               parent = "";
                               cost = 0;
                             }
   done;
 done

let block_coord coord =
 try
   let {name; xLoc; yLoc; status; successors; parent;cost} = Hashtbl.find graph coord in
   let status = Blocked in
   Hashtbl.replace graph coord {name; xLoc; yLoc; status; successors;parent;cost}
 with
   | Not_found -> () (* ignore if the coordinate can't be found - this just means we're trying to block a coordinate that *)
                     (* isn't in the grid. If this happens then it may be because of the for loops below, but if it is a  *)
                     (* more serious issue then an exception will be raised when the coordinate is accessed               *)

(* box_size -> morphism_list -> unit *)
(* Given a coordinate (x,y) and the size of a box, block out the coordinates that the box occupoies *)
let rec place_morphisms box_size = function
 | []              -> ()
 | (x,y)::xs  ->
   let x' = x - (box_size / 2) in
   let y' = y - (box_size / 2) in
   (* assume x y have been scaled and represent the left-bottom-aligned origin of the box *)
   (* now we mark the every node as occupied in the hashtable *)
   for i = (x'-2) to (x'+box_size+2) do
     for j = (y'-2 ) to (y'+box_size+2) do
         string_of_coord i j |> block_coord  (* type unit *)
     done;
   done;
   place_morphisms box_size xs

(* Remove elements in ls from fromls *)
let remove ls fromls = filter (fun x -> not(mem x ls)) fromls

(* Unwrap option type - may be better replaced using monadic bind *)
let rec clean = function
 | [] -> []
 | (None)::xs   -> clean xs
 | (Some x)::xs -> x :: clean xs

(* Given a coordinate, return the direction that it faces in the current path *)
 let direction vertex =
  let (x,y) = coord_of_string vertex in
  let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph vertex in
  if parent = "" then
    OccupiedHorizontal
  else begin
    let (fst_x,fst_y) = coord_of_string parent  in
    let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph parent in
    if parent = "" then
      if(x = fst_x) then
       OccupiedVertical
      else begin
        OccupiedHorizontal
      end
    else begin
      let (snd_x, snd_y)= coord_of_string parent in
      if x = fst_x && fst_x = snd_x then
        (* x is constant -> must be moving vertically *)
        OccupiedVertical
      else begin
        if y = fst_y && y = snd_y then
          (* y is static (must be changing x dir)*)
          OccupiedHorizontal
        else begin
          Blocked
        end
      end
    end
  end


let direc fst_c mid_c lst_c =
  let (fst_x,fst_y) = coord_of_string fst_c
  and (x,y)         = coord_of_string mid_c
  and (snd_x,snd_y) = coord_of_string lst_c in
  (* Compute direction of middle coordinate *)
  if x = fst_x && x = snd_x then
    (* x is constant -> must be moving vertically *)
    OccupiedVertical
  else begin
    if y = fst_y && y = snd_y then
      (* y is static (must be changing x dir)*)
      OccupiedHorizontal
    else begin
     (* We know that there *)
      Blocked
    end
  end

let rec mark_dirs' = function
  | []          -> ()
  | [x]         -> let dir = direction x in
                   let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph x in
                   Hashtbl.replace graph x {name; xLoc; yLoc; status = dir; successors;parent;cost}
  | x::[y]      -> let dir = direction x in
                   let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph x in
                   Hashtbl.replace graph x {name; xLoc; yLoc; status = dir; successors;parent;cost};
                   mark_dirs' [y]
  | x::y::z::xs -> let dir = direc x y z in
                   let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph y in
                   Hashtbl.replace graph y {name; xLoc; yLoc; status = dir; successors;parent;cost};
                   mark_dirs' (y::z::xs)

let rec mark_directions = function
 | []    -> ()
 | x::xs -> let dir = direction x in
            let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph x in
            Hashtbl.replace graph x {name; xLoc; yLoc; status = dir; successors;parent;cost};
            mark_directions xs

(* How we expand a node in the A* search algorithm *)
let expand vertex visited = try let {name; xLoc; yLoc; status; successors;parent;cost} =Hashtbl.find graph vertex in
     if List.mem vertex visited then
       []
     else
       let new_successors = (match status with
           | Free                 -> clean [(List.nth successors 2);(List.nth successors 0);(List.nth successors 4);(List.nth successors 6)] |> remove visited
           | OccupiedHorizontal   -> clean [(List.nth successors 0);(List.nth successors 4)] (* can only go up/down *)    |> remove visited
           | OccupiedVertical     -> clean [(List.nth successors 2);(List.nth successors 6)] (* can only go left/right *) |> remove visited
           | Blocked              -> [] ) in  
         for i = 0 to (List.length new_successors - 1) do
           let {name; xLoc; yLoc; status; successors;  parent; cost} = Hashtbl.find graph (List.nth new_successors i) in
           Hashtbl.replace graph (List.nth new_successors i) {name = name;
                                                              xLoc = xLoc;
                                                              yLoc = yLoc;
                                                              status = status;
                                                              successors = successors;
                                                              parent = vertex;
                                                              cost; }
         done;
         remove visited new_successors
      with
        | Not_found -> [""]

let rec strategy' oldf newf visited goal cost_so_far = match newf with
 | []        -> oldf
 | x::xs     ->
                if (String.length x >= 4) then begin
                  let (node_x',node_y') = coord_of_string x in
                  let (goal_x',goal_y') = coord_of_string goal in

                  let (node_x,node_y) = ((float_of_int node_x'), (float_of_int node_y')) in
                  let (goal_x,goal_y) = ((float_of_int goal_x'), (float_of_int goal_y')) in

                  let manhattan_dist = (abs(node_x' - goal_x') + abs(node_y' - goal_y')) * 10 in

                  (* let manhattan_dist = sqrt((node_x -. goal_x)**2. +. (node_y -. goal_y)**2.) |> int_of_float in *)

                  let {name; xLoc; yLoc; status; successors; parent;cost} = Hashtbl.find graph x in
                  let n = name
                  and xL = xLoc
                  and yL = yLoc
                  and stat = status
                  and succ = successors
                  and p = parent
                  and c = cost in
                  if parent = "" then
                    let parent_cost = 10 in
                    Hashtbl.replace graph x {name; xLoc; yLoc; status; successors; parent;cost=(parent_cost)};
                    strategy' (PrioQueue.insert oldf (manhattan_dist + (parent_cost)) x visited) xs visited goal cost_so_far
                  else
                    let {name; xLoc; yLoc; status; successors; parent;cost} = Hashtbl.find graph parent in
                    let parent_cost = cost + 10 in
                    if direction x = Blocked then begin
                      (* If x is trying to turn a corner, then we add an additional cost of 5 to smooth out paths *)
                      Hashtbl.replace graph x {name = n; xLoc = xL; yLoc = yL; status = stat; successors = succ; parent = p;cost=(parent_cost+70)};
                      strategy' (PrioQueue.insert oldf (manhattan_dist + (parent_cost+70)) x visited) xs visited goal cost_so_far
                    end
                    else begin
                      Hashtbl.replace graph x {name = n; xLoc = xL; yLoc = yL; status = stat; successors = succ; parent = p;cost=(parent_cost)};
                      strategy' (PrioQueue.insert oldf (manhattan_dist + (parent_cost)) x visited) xs visited goal cost_so_far
                    end
                end else begin
                  oldf
                end

let scale_down box_size (x,y) =
      let x' = float (x) /. 10.0 in
      let y' = float (y) /. 10.0 in
      (x',y')

let rec scale_up box_size = function
| []                  -> []
| ((x,y),(x',y'))::xs -> let xx  =  x  *. 10.0           |> int_of_float in
                         let yy  =  y  *. 10.0           |> int_of_float in
                         let xx' =  x' *. 10.0           |> int_of_float in
                         let yy' =  y' *. 10.0           |> int_of_float in
                         ((xx,yy),(xx',yy')):: scale_up box_size xs

(* THIS SCALES UP BOXES, NOT WIRES *)
let rec scale_up' box_size = function
| []                  -> []
| (x,y)::xs -> let xx = int_of_float (x *. 10.0) in
              let yy = y *. 10.0  |> int_of_float in
              ((xx,yy)):: scale_up' box_size xs

(*)
let rec corners'' = function
  | []          -> []
  | [x]         -> [x]
  | x::y::xs    -> let {name; xLoc; yLoc; status; successors;parent;cost} = Hashtbl.find graph x in
                   if status=Blocked then
                      x::corners'' xs
                   else corners'' (y::xs)
*)
let rec corners'' = function
| []          -> []
| [x]         -> [x]
| (x,y)::[(x',y')]                -> (x,y)::[(x',y')]
| (x,y)::(x',y')::(x'',y'')::xs  -> if x == x' && x == x'' then
                                      corners'' ((x',y')::(x'',y'')::xs)
                                    else
                                      if y == y' && y == y'' then
                                        corners'' ((x',y')::(x'',y'')::xs)
                                      else
                                        (x,y) :: corners'' xs

let last ls = List.rev ls |> List.hd

let corners = function
  | []   -> []
  | [x]     -> [x]
  | x::xs -> let start = x in
             (match List.rev xs with
               | []    -> []
               | y::ys -> x :: (corners'' (List.rev ys)) @ [y] )

let rec remove_duplicates = function
 | []                  -> []
 | [x]                 -> [x]
 | (x,y)::(x',y')::xs  -> if (x = x' && y = y')
                          then remove_duplicates ((x',y')::xs)
                          else (x,y) :: (remove_duplicates ((x',y')::xs))

(* reduce path of many points to just the essential ones  i.e. changes in direction *)
(* type : int * int list *)
let corners' ls = remove_duplicates ls

let rec print_path = function
 | [] -> printf "\n"
 | s::xs -> let (x,y) = coord_of_string s in printf "(%i,%i) --" x y; print_path xs

let rec find elt ls = match ls with
 | []        -> failwith "could not find"
 | (o,i)::xs -> if o = elt then i else find elt xs

let rec reconstruct_path elt goal =
         if elt = goal then
           [goal]
         else
           let ({name;xLoc;yLoc;status;successors;parent;cost}) = Hashtbl.find graph elt in
           name :: reconstruct_path parent goal

let rec search start goal fringe visited = match fringe with
 | PrioQueue.Empty    -> failwith "No route exists"
 | PrioQueue.Node(prio,x,left,right) ->
   let ({name;xLoc;yLoc;status;successors;parent;cost}) = Hashtbl.find graph x in
   if goal = name then
      reconstruct_path name start
   else
     (search start goal (strategy' (PrioQueue.remove_top (PrioQueue.Node(prio,x,left,right))) (expand x (visited)) (x::visited) goal (cost+1)) (x::visited))

let free_coord (x,y) =
   for i = x  to (x+3) do
     let c = string_of_coord i y in
     try let {name;xLoc;yLoc;status;successors;parent;cost} = Hashtbl.find graph c in
     Hashtbl.replace graph c {name;xLoc;yLoc;status = Free;successors;parent;cost}
     with
       | Not_found -> ()
   done

let last ls = List.rev ls |> List.hd

let reset_costs = Hashtbl.iter (fun n {name;xLoc;yLoc;status;successors;parent;cost} ->  Hashtbl.replace graph n {name;xLoc;yLoc;status;successors;parent="";cost=0}) graph

let rec find_route = function
 | []    -> [[]]
 | ((from_x, from_y),(to_x,to_y))::xs ->
   free_coord (from_x, from_y);
   free_coord ((from_x-3), from_y);
   free_coord (to_x, to_y); (* always want to link left to right *)
   free_coord ((to_x-3), to_y);
   (* Ensure that the port cannot be entered from below or above - this forces the wires to connect horizontally which gives nicer curves*)
   block_coord (string_of_coord to_x (to_y - 1)); block_coord (string_of_coord to_x (to_y + 1));
   block_coord (string_of_coord from_x (from_y - 1)); block_coord (string_of_coord from_x (from_y + 1));
   let goal = string_of_coord to_x to_y in
   let start = string_of_coord from_x from_y in
   let fringe = expand start [] in
   for i = 0 to (List.length fringe - 1) do
     let {name;xLoc;yLoc;status;successors;parent;cost} = Hashtbl.find graph (List.nth fringe i) in
     Hashtbl.replace graph (List.nth fringe i) {name;xLoc;yLoc;status;successors;parent;cost}
   done;
   let fringe' = strategy' PrioQueue.Empty fringe [] goal 1  in
   let route   = (search start goal fringe' [start] ) in
   mark_dirs' route;
   reset_costs;
   ((*corners*)(route @[start]))  :: (find_route xs)

let find_routes wires width height bx_size boxes =
   let width'   = width  * 10 in (* width of the whole frame *)
   let box_size = int_of_float bx_size in
   let height'  = height * 10  + (box_size * 10) in (* height of the whole frame *)
   generate_adjacency_lists (width') (height');
   place_morphisms (box_size) (scale_up' box_size boxes);
   scale_up (float box_size) wires |> find_route |> List.map (List.map coord_of_string) |> List.map (List.map (scale_down box_size)) |> List.map corners |> List.map remove_duplicates
