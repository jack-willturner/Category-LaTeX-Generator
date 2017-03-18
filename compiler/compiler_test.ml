open Lexing
open Lexer
open Compiler
open Printf
open Ast

(* This file contains tests for each individual non-trivial function in the compiler  *)
let failures  = Hashtbl.create 100 (* Function name | (actual output, expected output) *)
let nodes 		= Hashtbl.create 100 (* for testing the gen_inputs function *)

let test func actual expected =
	if actual = expected then () else Hashtbl.add failures func "Failure"

let rec test_l func actual expected = match (actual, expected) with
	| [], [] 			-> ()
	| x::xs,y::ys -> if x = y then test_l func xs ys
									 else begin
									 	Hashtbl.add failures func ("Failure");
										test_l func xs ys
									end

let type_inputs_test =
	let typed_input1 = Compiler.type_inputs ["1"] 				in
	let typed_input2 = Compiler.type_inputs ["x"] 				in
	let typed_input3 = Compiler.type_inputs []  					in
	let typed_input4 = Compiler.type_inputs ["2";"x";"2"] in
	let typed_input5 = Compiler.type_inputs ["x";"y";"z"] in
	test_l "type_inputs" typed_input1 [(Number 1)];
	test_l "type_inputs" typed_input2 [(String "x")];
	test_l "type_inputs" typed_input3 [];
	test_l "type_inputs" typed_input4 [(Number 2);(String "x");(Number 2)];
	test_l "type_inputs" typed_input5 [(String "x");(String "y");(String "z")]

let generate_nodes_test =
	let node_list1 = Compiler.generate_nodes (Compiler.inputNode) 0     in
	let node_list2 = Compiler.generate_nodes (Compiler.inputNode) 10    in
	let node_list3 = Compiler.generate_nodes (Compiler.inputNode) (-10) in
	let node_list4 = Compiler.generate_nodes (Compiler.outputNode) 0    in
	let node_list5 = Compiler.generate_nodes (Compiler.outputNode) 10   in
	let node_list6 = Compiler.generate_nodes (Compiler.outputNode) (-10) in
	test "generate_nodes" (List.length node_list1) (0);
	test "generate_nodes" (List.length node_list2) (10);
	test "generate_nodes" (List.length node_list3) (10);
	test "generate_nodes" (List.length node_list4) (0);
	test "generate_nodes" (List.length node_list5) (10);
	test "generate_nodes" (List.length node_list6) (10)

(* Copied from Compiler - algorithmically identical but output changed from unit
   adding to Hashtbl to a (string * string) list for ease of testing *)
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

              let to_x = x +. (!box_spacing) -. (!pixel_b_size) |> string_of_float in
              let to_y = from_y in

              (match curr_input with
                | str -> Hashtbl.add nodes str (to_x, to_y)
              )
            done;
						let inputs = Hashtbl.fold (fun k v acc -> (k,v) :: acc) nodes [] in
						Hashtbl.clear nodes;
						inputs

let gen_inputs_test =
	let input_drawing1 = gen_inputs 5 5 [] 		 in
	let input_drawing2 = gen_inputs 5 5 [1] 		in
	let input_drawing3 = gen_inputs 5 5 [x]     in
	let input_drawing4 = gen_inputs 5 5 [1,x]   in
	let input_drawing5 = gen_inputs 5 5 [x,1]   in
	let input_drawing6 = gen_inputs 5 5 [1,x,1] in
	test "gen_inputs"


(* TODO:
		- gen_inputs
		- gen_outputs
		- fix_nodes
		- replace_morphism_ports
		- replace_subdiagrams
*)

let _ =
	generate_nodes_test;
	type_inputs_test;
	let failure_list = (Hashtbl.fold (fun k v acc -> (k, v) :: acc) failures [])  in
	if List.length failure_list = 0 then
		printf "All compiler tests passed\n"
	else begin
		printf "Some failures occured:\n";
		(List.map (fun (k,v) -> printf "\tFailure in %s\n" k) failure_list; ())
	end
