open Lexing
open Lexer
open Compiler
open Printf
open Ast

(* This file contains tests for each individual non-trivial function in the compiler  *)
let failures  = Hashtbl.create 100 (* Function name | (actual output, expected output) *)
(*let nodes 		= Hashtbl.create 100  for testing the gen_inputs function *)

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

let _ =
	generate_nodes_test;
	type_inputs_test;
	let failure_list = (Hashtbl.fold (fun k v acc -> (k, v) :: acc) failures [])  in
	if List.length failure_list = 0 then
		printf "All compiler tests passed\n"
	else begin
		printf "Some failures occured:\n";
		(List.map (fun (k,v) -> printf "\tFailure in %s\n" k) failure_list; (); ())
	end
