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

let whitespace = [' ' '\t']+
let newline = "\r" | "\n" | "\r\n"

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ digit*

rule read =
	parse
	| whitespace                    {read lexbuf}
	| newline 											{incr lineno; read lexbuf}
	| "(*"		  	                 	{comment lexbuf; read lexbuf}
	| "*)"        	                {read lexbuf}
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
	| id 	 	  											{STRING (Lexing.lexeme lexbuf)}
	| eof														{EOF}
	| _ 		  		 									{ raise (SyntaxError ("Unexpected char: " ^
									 									Lexing.lexeme lexbuf)) }
and comment =
	parse
	| "*)" 													{ () }
	| "\n"													{ incr lineno; comment lexbuf }
	| _ 														{ comment lexbuf }
	| eof 													{ () }
