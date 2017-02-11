let adj_list = Hashtbl.create 10000

let xLoc = ref 0
let yLoc = ref 0

let new_x() = xLoc := !xLoc + 1; (string_of_int(!xLoc -1))
let new_y() = yLoc := !yLoc + 1; (string_of_int(!yLoc -1))

type node = {
    name : string;
    xLoc : int;
    yLoc : int;
    successors : string list; (* might be better as a hashtable *)
}

(* Priorities -
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
      Hashtbl.add adj_list n {
                                name = n;
                                xLoc = i;
                                yLoc = j;
                                successors = succ;
                              }
    done;
  done

(* Takes a list of morphisms and produces a graph *)
let rec place_morphisms box_size bitmap = function
  | []              -> bitmap
  | (m, (x,y))::xs  ->
    


    (*
    (* assume x and y have been scaled *)
    (* box_size is the number of squares that the box should occupy *)
      let mx = m.(x) in
      mx.(y) <- 3;
      (* this represents the left-bottom aligned origin of the box *)
      (* thus we want to loop right and up (it doesn't matter that our array indexes will be flipped) *)
      for i = x to (x+box_size) do
        for j = y to (y+box_size) do
          let mi = m.(i) in
          mi.(j) <- 3;
        done;
      done;
      place_morphisms box_size bitmap xs (* xs is the list of morphisms *) *)
