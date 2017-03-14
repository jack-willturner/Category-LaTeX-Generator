open Core
open Lexing
open Lexer
open Compiler
open Printf
open Ast

let fileno = ref 0

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

let run_test filename () =
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

	let command =
	  Command.basic
	    ~summary:"A compiler from monoidal category descriptions to LaTeX representations of the string-diagrams"
	    ~readme:(fun () -> "More detailed information on github")
	    Command.Spec.(empty +> anon ("filename" %: string))
	    run_test

	let () =
	  Command.run ~version:"1.0" ~build_info:"RWO" command
