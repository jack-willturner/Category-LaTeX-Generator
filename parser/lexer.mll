{
open Lexing
open Parser

exception SyntaxError of string

let lineno = ref 1

let next_line lexbuf =
	let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* Define basic data types *)

let int = '-'? ['0'-'9'] ['0'-'9']*

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = letter+ digit*

let whitespace = [' ' '\t']+
let newline = "\r" | "\n" | "\r\n"

rule read =
	parse
	| whitespace                    {read lexbuf}
	| newline 											{incr lineno; read lexbuf}
	| "(*"		  	                 	{comment lexbuf; read lexbuf}
	| "*)"        	                {read lexbuf}
	| int                           {INT (int_of_string)}
	| id 														{ID id}
	| '+'                           {TENSOR}
	| ';'                           {COMPOSE}
	| '.'    												{DOT}
	| ','														{COMMA}
	| '|'														{BAR}
	| '['														{OPEN_SQ}
	| ']'														{CLOSE_SQ}
	| ':'														{COLON}
	| '=' 													{EQUAL}
	| "->" 													{ARROW}
	| "Morphisms"                   {MORPHISMS}
	| "Wires"                      	{WIRES}
	| "Diagram" 										{DIAGRAM}
	| "Inputs" 											{INPUTS}
	| "Outputs"											{OUTPUTS}
	| eof                           {EOF}
	| _ 		  		 									{ raise (SyntaxError ("Unexpected char: " ^
									 									Lexing.lexeme lexbuf)) }
and comment =
	parse
	| "*)" 			{ () }
	| "\n"			{ incr lineno; comment lexbuf }
	| _ 			{ comment lexbuf }
	| eof 			{ () }
