%{
  open Ast
%}

(* TODO make wire syntax clearer (especially for the dangling case) *)

(***** DATA TYPES *****)
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


(***** PARSING RULES *****)
%start <Ast.program> top
%%

morphism_def:
  | morphism_id = STRING; COLON;
    BAR; INPUTS;  ARROW; i_list = separated_list(COMMA, STRING);
    BAR; OUTPUTS; ARROW; o_list = separated_list(COMMA, STRING);

    {Morphism(morphism_id, i_list, o_list)}

wire_def:
  | wire_id   = STRING; EQUAL;
    from_exp  = STRING; DOT;
    from_port = STRING; ARROW;
    to_exp    = STRING; DOT; to_port = STRING

    {Wire(wire_id, from_exp, from_port, to_exp, to_port)}

diagram:
  | IDENTITY                                {Identity}
  | OPEN_SQ; morph_id = STRING; CLOSE_SQ    {Morphism_ID morph_id}
  | wire_id = STRING                        {Wire_ID wire_id}
  | e=diagram;    COMPOSE; f = diagram      {Composition(e,f)}
  | e=diagram;    TENSOR;  f = diagram      {Tensor (e,f)}

top:
  | MORPHISMS; COLON; BAR; m_list = separated_list(BAR,morphism_def);
    WIRES;     COLON; BAR; w_list = separated_list(BAR,wire_def);
    DIAGRAM;   COLON; d = diagram; EOF      {Program(m_list, w_list, d)}
