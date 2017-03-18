open Hashtbl

let nodes = Hashtbl.create 100

let gen_inputs x y = function
  | []     -> ""
  | xs'    ->
            let xs = List.rev xs' in
            let spacing = 1.25 /. float ((List.length xs) + 1) in
            let base = y -. (0.62) in
            for i = 0 to (List.length xs - 1) do
              let curr_input = List.nth xs i in

              let from_y' = (base +. float (i+1) *. spacing) in
              let from_y'' = from_y' *. 10.0 |> int_of_float |> float_of_int in
              let from_y = from_y'' /. 10.0 |> string_of_float in

              let to_x = x +. (1.25) -. (0.62) |> string_of_float in
              let to_y = from_y in

              (match curr_input with
                | str -> Hashtbl.add nodes str (to_x, to_y)
              )
            done;
						let inputs = Hashtbl.fold (fun k v acc -> (k,v) :: acc) nodes [] in
						Hashtbl.clear nodes;
						List.fold_left (fun (k,(x,y)) acc -> ("Node " ^ k ^ "has location (" ^ x ^ ","^y^")\n") ^ acc) "" inputs

let _ =
  printf "%s\n" (gen_inputs 5 5 [1,x,1])
