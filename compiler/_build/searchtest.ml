open Printf
open Ast
open List

let graph = Hashtbl.create 200

let rec string_of_succ = function
  | [] -> printf "\n"
  | x::xs -> printf "%s, " x; string_of_succ xs


let string_of_coord x y = "x" ^ (x |> string_of_int)  ^ "y" ^ (y |> string_of_int)


let rec clean = function
  | [] -> []
  | (None)::xs   -> clean xs
  | (Some x)::xs -> x :: clean xs

  let rec remove fromls ls = filter (fun x -> not(mem x ls)) fromls



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
                         Some(string_of_coord (x'-1) y);
                         Some(string_of_coord (x'-1) (y'+1))] ) in
            printf "Coordinate %s has successors " name'; (string_of_succ (clean succ));
            Hashtbl.add graph name' {
                                     name = name';
                                     xLoc = i;
                                     yLoc = j;
                                     status = Free;
                                     successors = succ;
                                   }
        done;
      done

      let strategy oldf newf visited = remove (oldf @ newf) visited (* TODO - change this to priority queue *)

      let rec expand vertex = let {name; xLoc; yLoc; status; successors} = Hashtbl.find graph vertex in
        (match status with
            | Free                 -> clean [(List.nth successors 2);(List.nth successors 0);(List.nth successors 4); (List.nth successors 6)]
            | OccupiedHorizontal   -> clean [(List.nth successors 0);(List.nth successors 4)] (* can only go up/down *)
            | OccupiedVertical     -> clean [(List.nth successors 2);(List.nth successors 5)] (* can only go left/right *)
            | Blocked              -> [] )


            let rec print_path = function
              | [] -> printf "\n"
              | (x,y)::xs -> printf "(%i,%i) --" x y; print_path xs


(* returns a path of int * int list *)
let rec search goal fringe path visited = match fringe with
  | []    -> failwith "No route exists"
  | x::xs -> let ({name;xLoc;yLoc;status;successors}) = Hashtbl.find graph x in
                                    printf "%s\n" name;
                                    if goal = name
                                    then
                                    let end_path = [(xLoc,yLoc)] in
                                    print_path (path @ end_path);
                                    end_path
                                    else search goal (strategy xs (expand x) (x::visited)) ((xLoc,yLoc)::path) (x::visited)


let _ =
  generate_adjacency_lists 10 10;
  let path = search "x10y10" (strategy [] (expand "x5y5") []) [] [] in
  print_path path;
  let done_s = "Done" in
  done_s
