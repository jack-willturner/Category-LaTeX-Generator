open Ast

(* TODO calculate coordinates of ports *)

let graph = Hashtbl.create 10000

let xLoc = ref 0
let yLoc = ref 0

let new_x() = xLoc := !xLoc + 1; (string_of_int(!xLoc -1))
let new_y() = yLoc := !yLoc + 1; (string_of_int(!yLoc -1))


let string_of_coord x y = "x" ^ (x |> string_of_int)  ^ "y" ^ (y |> string_of_int)

(* Each adjacency list is made up of 8 elements describing the successors of the square clockwise  *)
let generate_adjacency_lists x y =
  for i = 0 to x do
    for j = 0 to y do
      let n = string_of_coord x y in
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
                        [Some(string_of_coord x' (y+1)); None; None; None; Some(string_of_coord x' (y'-1)); Some(string_of_coord (x'-1) (y'-1)); Some(string_of_coord (x'-1) y); Some(string_of_coord (x'-1) (y+1)) ]
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
                         Some(string_of_coord (x'-1) y);
                         Some(string_of_coord (x'-1) (y'+1))]
      ) in
      Hashtbl.add graph n {
                                name = n;
                                xLoc = i;
                                yLoc = j;
                                status = Free;
                                successors = succ;
                              }
    done;
  done

let block_coord coord =
  let {name; xLoc; yLoc; status; successors} = Hashtbl.find graph coord in
  let status = Blocked in
  Hashtbl.replace graph coord {name; xLoc; yLoc; status; successors}

(* box_size -> morphism_list -> unit *)
let rec place_morphisms box_size = function
  | []              -> ()
  | (x,y)::xs  ->
    (* assume x y have been scaled and represent the left-bottom-aligned origin of the box *)
    (* now we mark the every node as occupied in the hashtable *)
    for i = x to (x+box_size) do
      for j = y to (y+box_size) do
          string_of_coord i j |> block_coord  (* type unit *)
      done;
    done

let rec expand vertex = let {name; xLoc; yLoc; status; successors} = Hashtbl.find graph vertex in
                        successors

let strategy oldf newf = newf @ oldf (* TODO - change this to priority queue *)

let rec clean = function
  | [] -> []
  | (None)::xs -> clean xs
  | (Some x)::xs-> x :: clean xs

(* reduce path of many points to just the essential ones  i.e. changes in direction *)
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

let rec search goal fringe path = match fringe with
  | []    -> failwith "No route exists"
  | x::xs -> let ({name;xLoc;yLoc;status;successors}) = Hashtbl.find graph x in
                                    if goal == name then [(xLoc,yLoc)]
                                    else
                                      let fringe' = (match status with
                                        | Free                 -> clean [(List.nth successors 1);(List.nth successors 3);(List.nth successors 5);(List.nth successors 7)]
                                        | OccupiedHorizontal   -> clean [(List.nth successors 1);(List.nth successors 5)] (* can only go up/down *)
                                        | OccupiedVertical     -> clean [(List.nth successors 3);(List.nth successors 7)] (* can only go left/right *)
                                        | Blocked              -> [] ) in
                                      search goal (strategy fringe fringe') (path@[(xLoc,yLoc)])

let rec find_route = function
  | []    -> [[]]
  | ((from_x, from_y),(to_x,to_y))::xs ->
    let goal = string_of_coord to_x to_y in
    let start = string_of_coord from_x from_y in
    corners (search goal (expand start |> clean) [(from_x, from_y)]) :: find_route xs

let rec scale_up = function
  | []                  -> []
  | ((x,y),(x',y'))::xs -> let xx = int_of_float (x *. 200.0) in
                           let yy = y *. 200.0  |> int_of_float in
                           let xx' = x' *. 200.0 |> int_of_float in
                           let yy' = y' *. 200.0 |> int_of_float in
                           ((xx,yy),(xx',yy')):: scale_up xs

let rec scale_up' = function
 | []                  -> []
 | (x,y)::xs -> let xx = int_of_float (x *. 200.0) in
                  let yy = y *. 200.0  |> int_of_float in
                  ((xx,yy)):: scale_up' xs

let scale_down (x,y) = let x' = x/ 200 in
                       let y' = y/ 200 in
                       (x',y')

let find_routes wires width height boxes =
  let box_size = float width /. float(boxes |> List.length)
  and width'   = width  * 200
  and height'  = height * 200
  in
  generate_adjacency_lists (width') (height');
  place_morphisms (int_of_float box_size) (scale_up' boxes);
  scale_up wires |> find_route |> List.map (List.map scale_down )
