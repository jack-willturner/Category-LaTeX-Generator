%{
  open Ast
%}

(***** DATA TYPES *****)
%token  <string>  STRING
%token  <int>     INT

(***** TOKENS *****)
%token IDENTITY
%token TENSOR
%token OPEN_SQUARE CLOSE_SQUARE
%token OPEN_BRACE CLOSE_BRACE
%token BOX LINK
%token MODULE
%token DOT COMMA BAR COLON ARROW SEMICOLON EQUAL
%token BOXCOLOUR BOXSHAPE
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
  | d1 = diagram; SEMICOLON; d2 = diagram
    {Composition(d1, d2)}
  | e=diagram;    TENSOR;  f = diagram      {Tensor (e,f)}
  | option(OPEN_SQUARE); ins = separated_list(COMMA, STRING); option(CLOSE_SQUARE);
    morphID = STRING;
    option(OPEN_SQUARE); outs = separated_list(COMMA,STRING); option(CLOSE_SQUARE);

    {Morphism(morphID, ins, outs)}

module_def :
  | MODULE; m_name = STRING; OPEN_BRACE; b_list = separated_list(DOT,morphism_def); SEMICOLON;
    LINK; w_list = separated_list(COMMA,wire_def); DOT;
    d = diagram; CLOSE_BRACE;               {Module(m_name, b_list, w_list, d)}

definition:
  | m_list = separated_list(DOT,morphism_def); SEMICOLON;
    LINK; w_list = separated_list(COMMA,wire_def); DOT;
    d = diagram;                            {Diagram(m_list, w_list, d)}

top:
  | module_list = list(module_def); definition_list = list(definition); EOF  {Program(module_list, definition_list)}
