%{
  open Ast
%}

(* TODO make wire syntax clearer (especially for the dangling case) *)

(***** DATA TYPES *****)
%token  <int>    INT
%token  <string> STRING

(***** TOKENS *****)
%token IDENTITY
%token TENSOR COMPOSE
%token MORPHISMS WIRES DIAGRAM
%token DOT COMMA BAR COLON ARROW EQUAL
%token OPEN_SQ CLOSE_SQ
%token INPUTS OUTPUTS
%token EOF

(***** PRECEDENCE RULES *****)
%left  TENSOR
%left  COMPOSE
%right EQUAL
%right OPEN_SQ


(***** PARSING RULES *****)
%start <Ast.expression> top
%%

morphism_def:
  | morphism_id = string; COLON;
    BAR; INPUTS;  ARROW; i_list = separated_list(string, comma);
    BAR; OUTPUTS; ARROW; o_list = separated_list(string, comma)

    {Morphism(morphism_id, i_list, o_list)}

(* This syntax is ugly for defining dangling wires - TODO *)
wire_def:
  | wire_id   = string; EQUAL;
    from_exp  = string; DOT; from_port = string;
    ARROW;
    to_exp    = string; DOT; to_port = string

    {Wire(wire_id, from_exp, from_port, to_exp, to_port)}

diagram:
  | IDENTITY                                {Identity}
  | OPEN_SQ; morph_id = string; CLOSE_SQ    {Morphism_ID morph_id}
  | wire_id = string                        {Wire_ID wire_id}
  | e=diagram;    COMPOSE; f = diagram      {Composition(e,f)}
  | e=diagram;    TENSOR;  f = diagram      {Tensor (e,f)}

program:
  | MORPHISMS; COLON; BAR; m_list = separated_list(morphism_def,BAR);
    WIRES;     COLON; BAR; w_list = separated_list(wire_def,BAR);
    DIAGRAM;   COLON; d = diagram           {Program(m_list, w_list, d)}
