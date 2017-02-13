(* TODO calculate coordinates of ports *)

let graph = Hashtbl.create 10000

let xLoc = ref 0
let yLoc = ref 0

let new_x() = xLoc := !xLoc + 1; (string_of_int(!xLoc -1))
let new_y() = yLoc := !yLoc + 1; (string_of_int(!yLoc -1))

type node = {
    name : string;
    xLoc : int;
    yLoc : int;
    status : int;
    successors : string list; (* might be better as a hashtable *)
}

(* Priorities -
      0       - Free
      1       - Taken by horizontal wire
      2       - Taken by vertical wire
      3       - Taken by wire corner or box
*)

let string_of_coord x y = "x" ^ (i |> string_of_int)  ^ "y" ^ (j |> string_of_int)

let generate_adjacency_lists x y =
  for i = 0 to x do
    for j = 0 to y do
      let n = string_of_coord x y in
      and succ =
      (match i j with
        | 0,0   ->  ["x1y0";"x0y1";"x1y1"]
        | 0,n   -> (* only want above, below and right of *)
                    if n = y
                    then [(string_of_coord 0 (n-1)); (string_of_coord 1 n); (string_of_coord 1 (n-1))]
                    else [(string_of_coord 0 (n+1)); (string_of_coord 0 (n-1)); (string_of_coord 1 n+1); (string_of_coord 1 n); (string_of_coord 1 (n-1))]
        | n,0   -> (* only want left, right and above *)
                    if n = x
                    then [(string_of_coord (n-1) 0); (string_of_coord (n-1) 1); (string_of_coord n 1)]
                    else [(string_of_coord (n-1) 0); (string_of_coord (n-1) 1); (string_of_coord n 1); (string_of_coord (n+1) 1); (string_of_coord (n+1) 0)]
        | x', y' ->  (* check if on top or right edge *)
                    if x' = x then
                      if y' = y then
                        (* top right hand corner *)
                        [(string of_coord (x'-1) y); (string_of_coord (x'-1) (y'-1)); (string_of_coord x' (y'-1))]
                      else
                        (* right hand side of grid but not in either corner *)
                        [(string of_coord (x'-1) y); (string_of_coord (x'-1) (y'-1)); (string_of_coord x' (y'-1)); (string_of_coord (x'-1) (y+1)); (string_of_coord x' (y+1))]
                    else
                      if y' = y then
                        (* along the top of the grid but not near either corner *)
                        [(string_of_coord (x'-1) y); (string_of_coord (x'-1) (y'-1)); (string_of_coord x' (y'-1)); (string_of_coord (x'+1) (y'-1)); (string_of_coord (x'+1) y')]
                      else
                        (* somewhere in the centre of the grid *)
                        [(string_of_coord (x'-1) y); (string_of_coord (x'-1) (y'-1)); (string_of_coord x' (y'-1)); (string_of_coord (x'+1) (y'-1)); (string_of_coord (x'+1) y'); (string_of_coord (x'+1) (y'+1)); (string_of_coord x' (y'+1)); (string_of_coord (x'-1) (y'+1))]
      ) in
      Hashtbl.add graph n {
                                name = n;
                                xLoc = i;
                                yLoc = j;
                                status = 0;
                                successors = succ;
                              }
    done;
  done

let block_coord coord x y =
  let {name, xL, yL, status, succ} = Hashtbl.find graph coord in
  Hashtbl.replace graph coord {name, xL, yL, 3, succ}

(* box_size -> morphism_list -> unit *)
let rec place_morphisms box_size = function
  | []              -> ()
  | (m, (x,y))::xs  ->
    (* assume x y have been scaled and represent the left-bottom-aligned origin of the box *)
    (* now we mark the every node as occupied in the hashtable *)
    for i = x to (x+box_size) do
      for j = y to (y+box_size) do
          string_of_coord x y |> block_coord i j  (* type unit *)
      done;
    done

let rec expand vertex = let {_,_,_,_,succ} = Hashtbl.find graph vertex in
                        succ

let strategy oldf newf = newf @ oldf (* TODO - change this to priority queue *)


let rec search start goal fringe path = match fringe with
  | []    -> failwith "No route exists"
  | ({name,x,y,status,succ})::xs -> if goal == name then [(x,y)]
                                    else
                                      (match status with
                                        | 0 -> succ
                                        | 1 -> (* can only go up/down *)
                                        | 2 -> (* can only go left/right *)
                                        | 3 -> [] )

(* TODO - add heuristics *)
let rec search fringe start goal path =
  match fringe with
    | []    -> failwith "No route exists"
    | x::xs -> if goal x then [x]
               else search graph (strategy xs (expand x graph) goal (path@[x])

let rec find_route = function
  | []           -> [[]]
  | (out,in)::xs -> (search [] src dest []) :: find_route xs

let find_routes wires width height boxes =
  let box_size = width /. float(boxes |> List.length)
  and width'   = width * 200
  and height'  = height * 200
  in
  generate_adjacency_lists width' height';
  place_morphisms box_size boxes;
  paths (* TODO: scale back to tikz scheme *)
