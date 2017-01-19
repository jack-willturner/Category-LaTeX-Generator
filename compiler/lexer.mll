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
	| '.'    												{DOT}
	| ','														{COMMA}
	| ';'														{SEMICOLON}
	| '|'														{BAR}
	| ':'														{COLON}
	| "->" 													{ARROW}
	| "box"			                    {BOX}
	| "link"                      	{LINK}
	| "in" 													{INPUTS}
	| "out"													{OUTPUTS}
	| "-1-"														{IDENTITY}
	| digit													{INT (int_of_string (Lexing.lexeme lexbuf))}
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
