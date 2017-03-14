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
let id = (['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9']) (['a'-'z'] | ['A'-'Z'] | '_' | '\'' | ['0'-'9'])*

rule read =
	parse
	| whitespace                    {read lexbuf}
	| newline 											{incr lineno; read lexbuf}
	| "(*"		  	                 	{comment lexbuf; read lexbuf}
	| "*)"        	                {read lexbuf}
	| '.'    												{DOT}
	| ','														{COMMA}
	| '{'														{OPEN_BRACE}
	| '}'														{CLOSE_BRACE}
	| '['														{OPEN_SQUARE}
	| ']'														{CLOSE_SQUARE}
	| '('														{OPEN_BRACKET}
	| ')'														{CLOSE_BRACKET}
	| ';'														{SEMICOLON}
	| '|'														{BAR}
	| ':'														{COLON}
	| '<'														{OPEN_ANGLE}
	| '>'														{CLOSE_ANGLE}
	| "->" 													{ARROW}
	| "box"			                    {BOX}
	| "link"                      	{LINK}
	| "module"											{MODULE}
	| "boxcolour"										{BOXCOLOUR}
	| "boxshape"										{BOXSHAPE}
	| "-1-"													{IDENTITY}
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
