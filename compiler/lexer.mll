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
let id = (['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9']) (['a'-'z'] | ['A'-'Z'] | '_' | '\'' | '<' | '>' | ['0'-'9'])*

rule read =
	parse
	| whitespace                    {read lexbuf}
	| newline 											{incr lineno; read lexbuf}
	| "(*"		  	                 	{comment 0 lexbuf}
	| "*)"        	                {read lexbuf}
	| '.'    												{DOT}
	| ','														{COMMA}
	| '$'														{DOLLAR}
	| '{'														{OPEN_BRACE}
	| '}'														{CLOSE_BRACE}
	| '['														{OPEN_SQUARE}
	| ']'														{CLOSE_SQUARE}
	| '('														{OPEN_BRACKET}
	| ')'														{CLOSE_BRACKET}
	| ';'														{SEMICOLON}
	| '|'														{BAR}
	| ':'														{COLON}
	| "->" 													{ARROW}
	| "box"			                    {BOX}
	| "link"                      	{LINK}
	| "module"											{MODULE}
	| "boxcolour"										{BOXCOLOUR}
	| "boxshape"										{BOXSHAPE}
	| "id"													{IDENTITY}
	| id 	 	  											{STRING (Lexing.lexeme lexbuf)}
	| eof														{EOF}
	| _ 		  		 									{ raise (SyntaxError ("Unexpected char: " ^
									 									Lexing.lexeme lexbuf)) }

and comment lvl =
	parse
	| "(*" 													{ comment (lvl + 1) lexbuf }
	| "*)"												  { if lvl <= 0 then read lexbuf else comment (lvl - 1) lexbuf }
	|	newline												{ incr lineno; Lexing.new_line lexbuf; comment lvl lexbuf }
	| _ { comment lvl lexbuf }
