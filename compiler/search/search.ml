open Ast
open Printf
open List
open PrioQueue

let graph = Hashtbl.create 10000

let xLoc = ref 0
let yLoc = ref 0

let new_x() = xLoc := !xLoc + 1; (string_of_int(!xLoc -1))
let new_y() = yLoc := !yLoc + 1; (string_of_int(!yLoc -1))

let string_of_coord x y = "x" ^ (x |> string_of_int)  ^ "y" ^ (y |> string_of_int)

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
                                cost = 0;
                                parent = "";
                              }
    done;
  done

let block_coord coord =
  try
    let {name; xLoc; yLoc; status; successors;cost; parent} = Hashtbl.find graph coord in
    let status = Blocked in
    Hashtbl.replace graph coord {name; xLoc; yLoc; status; successors;cost;parent}
  with
    | Not_found -> ()

(* box_size -> morphism_list -> unit *)
let rec place_morphisms box_size = function
  | []              -> ()
  | (x,y)::xs  ->

    let x' = x - (box_size / 2) in
    let y' = y - (box_size / 2) in
    printf "PLACING MORPHISM AT (%i,%i)\n" x' y';
    (* assume x y have been scaled and represent the left-bottom-aligned origin of the box *)
    (* now we mark the every node as occupied in the hashtable *)
    for i = (x') to (x'+box_size) do
      for j = (y') to (y'+box_size) do
          string_of_coord i j |> block_coord  (* type unit *)
      done;
    done;
    place_morphisms box_size xs

let rec remove ls fromls = filter (fun x -> not(mem x ls)) fromls

let rec clean = function
  | [] -> []
  | (None)::xs   -> clean xs
  | (Some x)::xs -> x :: clean xs

let rec expand vertex visited =
    let {name; xLoc; yLoc; status; successors; cost;parent} = Hashtbl.find graph vertex in
    printf "Currently expanding %s with parent %s with total cost %i\n" name parent cost;
    let parent_cost = cost in
    let new_successors = (match status with
        | Free                 -> clean [(List.nth successors 2);(List.nth successors 0);(List.nth successors 4); (List.nth successors 6)] |> remove visited
        | OccupiedHorizontal   -> clean [(List.nth successors 0);(List.nth successors 4)] (* can only go up/down *) |> remove visited
        | OccupiedVertical     -> clean [(List.nth successors 2);(List.nth successors 5)] (* can only go left/right *) |> remove visited
        | Blocked              -> [] ) in
      for i = 0 to (List.length new_successors -1) do
        let {name; xLoc; yLoc; status; successors; cost; parent} = Hashtbl.find graph (List.nth new_successors i) in
        let vertex_cost = cost in
        Hashtbl.replace graph (List.nth new_successors i) {name = name;
                                                           xLoc = xLoc;
                                                           yLoc = yLoc;
                                                           status = status;
                                                           successors = successors;
                                                           cost = (parent_cost + 10);
                                                           parent = vertex}
      done;
      remove visited new_successors

let rec strategy' oldf newf visited goal cost_so_far = match newf with
  | []        -> oldf
  | x::xs     -> let (node_x,node_y) = coord_of_string x in
                 let (goal_x,goal_y) = coord_of_string goal in
                 let manhattan_dist = abs(node_x - goal_x) + abs(node_y - goal_y) * 10 in
                 let {name; xLoc; yLoc; status; successors; cost; parent} = Hashtbl.find graph x in
                 strategy' (PrioQueue.insert oldf (manhattan_dist + cost_so_far) x visited) xs visited goal cost_so_far



let rec corners = function
  | []          -> []
  | [x]         -> [x]
  | (x,y)::[(x',y')]                -> (x,y)::[(x',y')]
  | (x,y)::(x',y')::(x'',y'')::xs  -> if x == x' && x == x'' then
                                        corners ((x',y')::(x'',y'')::xs)
                                      else
                                        if y == y' && y == y'' then
                                          corners ((x',y')::(x'',y'')::xs)
                                        else
                                          (x,y) :: corners xs

let rec remove_duplicates = function
  | []                  -> []
  | [x]                 -> [x]
  | (x,y)::(x',y')::xs  -> if (x = x' && y = y')
                           then remove_duplicates ((x',y')::xs)
                           else (x,y) :: (remove_duplicates ((x',y')::xs))

(* reduce path of many points to just the essential ones  i.e. changes in direction *)
(* type : int * int list *)
let rec corners' ls = remove_duplicates ls

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
            let ({name;xLoc;yLoc;status;successors;cost;parent}) = Hashtbl.find graph elt in
            name :: reconstruct_path parent goal

let rec search start goal fringe visited cost = match fringe with
  | PrioQueue.Empty    -> failwith "No route exists"
  | PrioQueue.Node(prio,x,left,right) ->
    let ({name;xLoc;yLoc;status;successors;cost;parent}) = Hashtbl.find graph x in
    if goal = name then
      reconstruct_path name start
    else
      (search start goal (strategy' (PrioQueue.remove_top (PrioQueue.Node(prio,x,left,right))) (expand x (visited)) (x::visited) goal cost) (x::visited) (cost+10))

let reset_costs = Hashtbl.iter (fun n {name;xLoc;yLoc;status;successors;cost;parent} ->  Hashtbl.replace graph n {name;xLoc;yLoc;status;successors;cost=0;parent=""}) graph

let free_coord (x,y) =
    for i = x to (x) do
      let c = string_of_coord i y in
      let {name;xLoc;yLoc;status;successors;cost;parent} = Hashtbl.find graph c in
      Hashtbl.replace graph c {name;xLoc;yLoc;status = Free;successors;cost;parent}
    done

let rec find_route = function
  | []    -> [[]]
  | ((from_x, from_y),(to_x,to_y))::xs ->
    printf "Linking x to y:\t\t (%i,%i) -- (%i,%i)\n" from_x from_y to_x to_y;
    free_coord (from_x, from_y);
    free_coord ((to_x - 3), to_y);
    let goal = string_of_coord to_x to_y in
    let start = string_of_coord from_x from_y in
    let fringe = expand start [] in
    for i = 0 to (List.length fringe - 1) do
      let {name;xLoc;yLoc;status;successors;cost;parent} = Hashtbl.find graph (List.nth fringe i) in
      Hashtbl.replace graph (List.nth fringe i) {name;xLoc;yLoc;status;successors;cost = 10;parent}
    done;
    let fringe' = strategy' PrioQueue.Empty fringe [] goal 0  in
    let route   = (search start goal fringe' [start] 0) in
    print_path route;
    reset_costs;
    route :: find_route xs


let _ =
  let boxes = [] in
  let width = 5
  and height = 5 in
  let box_size = 1 in
  generate_adjacency_lists width height;
  place_morphisms box_size boxes;
  let route = List.hd (find_route [(1,2),(4,2)]) in
  route