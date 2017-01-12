open Ast
open Lexer
open Parser

let rec read_to_empty buf =
    let s = read_line () in
    if s = "" then buf
    else (Buffer.add_string buf s;
          Buffer.add_string buf "\n";
          read_to_empty buf)
let _ =
  read_to_empty (Buffer.create 1)
  |> Buffer.contents
  |> Lexing.from_string
  |> Parser.top Lexer.read
  |> List.map string_of_ast
  |> String.concat ",\n"
  |> print_endline
