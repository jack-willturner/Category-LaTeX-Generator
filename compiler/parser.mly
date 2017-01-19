%{
  open Ast
%}

(***** DATA TYPES *****)
%token  <string>  STRING
%token  <int>     INT

(***** TOKENS *****)
%token IDENTITY
%token TENSOR
%token BOX LINK
%token DOT COMMA BAR COLON ARROW SEMICOLON
%token INPUTS OUTPUTS
%token EOF

(***** PRECEDENCE RULES *****)
%left  TENSOR

(***** PARSING RULES *****)
%start <Ast.program> top
%%

morphism_def:
  | BOX; morphism_id = STRING; COLON;
     inputs=INT; ARROW; outputs=INT;

    {Box(morphism_id, inputs, outputs)}

wire_def:
  | from_exp = STRING; to_exp = STRING;   {Wire(from_exp, to_exp)}

diagram:
  | IDENTITY                                {Identity}
  | OUTPUTS; outs=list(STRING); DOT; d1 = diagram; BAR; INPUTS; ins=list(STRING); DOT; d2 = diagram
    (* out x y z . f | in a b c . g -> Composition(e,f,ins,outs) *)
    {Composition(d1, d2, ins, outs)}
  | e=diagram;    TENSOR;  f = diagram      {Tensor (e,f)}
  | morphID = STRING                        {Morphism(morphID)}

top:
  | m_list = separated_list(DOT,morphism_def); SEMICOLON;
    LINK; w_list = separated_list(COMMA,wire_def); DOT;
    d = diagram; EOF      {Program(m_list, w_list, d)}
