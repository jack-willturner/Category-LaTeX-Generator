open Lexing
open Lexer
open Printf
open Ast

(* This file compiles and tests the parser *)

let fileno = ref 0

let test_files = ["test/parser/test1";"test/parser/test2";"test/parser/test3";"test/parser/test4";"test/parser/test5";"test/parser/test6";"test/parser/test7";"test/parser/test8";"test/parser/test9";"test/parser/test10"]

let expected_output1 = "Composition(f,Composition(g,Composition(g,h)))"
let expected_output2 = "Tensor(Tensor(Composition(f,Composition(g,g)),Composition(g,Composition(g,g))),Composition(g,Composition(g,h)))"
let expected_output3 = "Composition(Tensor(Tensor(f,f),f),Composition(Tensor(Tensor(g,g),g),Tensor(Tensor(h,h),h)))"
let expected_output4 = "f"
let expected_output5 = "Composition(f,g)"
let expected_output6 = "Composition(f,g)"
let expected_output7 = "Composition(f,g)"
let expected_output8 = "Composition(f,Composition(g,h))"
let expected_output9 = "Tensor(Subdiagram(m),-1-)"
let expected_output10 = "Composition(f,Composition(f,Composition(f,Composition(f,Composition(f,Composition(f,Composition(f,Composition(f,Composition(f,Composition(f,f))))))))))"

let expected_outputs = [expected_output1;expected_output2;expected_output3;expected_output4;expected_output5;expected_output6;expected_output7;expected_output8;expected_output9;expected_output10]

let rec read_to_empty buf in_channel =
	Lexer.lineno := 1;
	try
		let s = input_line in_channel in
		if s = "" then buf
		else (Buffer.add_string buf s;
			  Buffer.add_string buf "\n";
			  read_to_empty buf in_channel)
	with End_of_file ->
		close_in in_channel; buf

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  eprintf "Line: %d, File: %d, Position: %d\n" !Lexer.lineno !fileno pos.pos_cnum


let parse_with_error lexbuf =
  try Parser.top Lexer.read lexbuf with
  | SyntaxError msg  -> prerr_string (msg ^ ": ");
                       print_position lexbuf;
                       exit (-1)
  | Parser.Error   ->  prerr_string "Parse error: ";
                       print_position lexbuf;
                       exit (-1)

let run_test filename =
  fileno := !fileno + 1;
	let in_channel = open_in filename in
	let actual_result =
	read_to_empty (Buffer.create 1) in_channel
	|> Buffer.contents
	|> Lexing.from_string
	|> parse_with_error
	|> Ast.string_of_top in
	let expected_result = (List.nth expected_outputs (!fileno - 1)) in
  if actual_result = expected_result then
		printf "%s passed test\n" filename
	else
		printf "%s failed test\nOUTPUT:   %s\nEXPECTED: %s\n\n" filename actual_result expected_result


let main = List.map run_test test_files

let rec map_with_print f ls =
	match ls with
	| [] 		-> []
	| x::xs 	-> f x :: map_with_print f xs

let _ = main
