
(* The type of tokens. *)

type token = 
  | TENSOR
  | STRING of (string)
  | SEMICOLON
  | OPEN_SQUARE
  | OPEN_BRACKET
  | OPEN_BRACE
  | MODULE
  | LINK
  | INT of (int)
  | INPUTS
  | IDENTITY
  | EOF
  | DOT
  | COMMA
  | COLON
  | CLOSE_SQUARE
  | CLOSE_BRACKET
  | CLOSE_BRACE
  | BOXSHAPE
  | BOXCOLOUR
  | BOX
  | BAR
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
