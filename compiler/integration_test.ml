open Lexing
open Lexer
open Printf
open Ast


(* This file compiles and tests the entire compiler *)


let fileno = ref 0

let test_files = ["test/integration_tests/demo1.cat";"test/integration_tests/demo2.cat";"test/integration_tests/demo3.cat";"test/integration_tests/demo4.cat";"test/integration_tests/demo5.cat";"test/integration_tests/demo6.cat";"test/integration_tests/demo7.cat";"test/integration_tests/demo8.cat"]

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
  |> Compiler.compile_program in
  let output_file = filename ^ ".tex" in
	let oc = open_out output_file in
	fprintf oc "%s\n" actual_result;
	close_out oc


let main = List.map run_test test_files

let rec map_with_print f ls =
	match ls with
	| [] 		-> []
	| x::xs 	-> f x :: map_with_print f xs

let _ = main
