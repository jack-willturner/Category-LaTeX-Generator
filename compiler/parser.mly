%{
  open Ast
%}

/***** DATA TYPES *****/
%token  <string>  STRING

/****** TOKENS ******/
%token IDENTITY
%token OPEN_SQUARE CLOSE_SQUARE OPEN_ANGLE CLOSE_ANGLE
%token OPEN_BRACE CLOSE_BRACE
%token OPEN_BRACKET CLOSE_BRACKET
%token BOX LINK
%token MODULE
%token BOXSHAPE BOXCOLOUR
%token DOT COMMA BAR COLON ARROW SEMICOLON
%token EOF

/***** PRECEDENCE RULES *****/
%left BAR
%right SEMICOLON

/***** PARSING RULES *****/
%start <Ast.program> top
%%

styling:
  | OPEN_BRACE;  BOXCOLOUR; COLON; bcolour = option(STRING); SEMICOLON;
                 BOXSHAPE;  COLON; bshape = option(STRING);
    CLOSE_BRACE;
    {(bcolour, bshape)}

morphism_def:
  | BOX; morphism_id = STRING; boxstyle = option(styling); COLON;
     inputs=STRING; ARROW; outputs=STRING;

    {Box(morphism_id, (int_of_string inputs), (int_of_string outputs), boxstyle)}

wire_def:
  | from_exp = STRING; to_exp = STRING;   {Wire(from_exp, to_exp)}

params:
  | OPEN_SQUARE; params = separated_list(COMMA, STRING); CLOSE_SQUARE;     {params}

diagram:
  | IDENTITY                                                       {Identity}
  | ins = option(params); morphID = STRING; outs = option(params)  {Morphism(morphID, ins, outs)}
  | OPEN_BRACKET; d = diagram; CLOSE_BRACKET;                      {d}
  | d1 = diagram; SEMICOLON; d2 = diagram                          {Composition(d1, d2)}
  | e=diagram;    BAR;  f = diagram                                {Tensor (e,f)}
  | ins = option(params); OPEN_ANGLE; subdiagramID = STRING; CLOSE_ANGLE; outs = option(params)  {Subdiagram(subdiagramID, Identity, ins, outs)}

link_list:
  | LINK; w_list = separated_list(COMMA,wire_def); DOT              {w_list}

module_def :
  | MODULE; m_name = STRING; OPEN_BRACE; b_list = separated_list(SEMICOLON,morphism_def); DOT;
    ww_list = option(link_list);
    d = diagram; CLOSE_BRACE;               {Module(m_name, b_list, ww_list, d)}

definition:
  | m_list = separated_list(SEMICOLON,morphism_def); DOT;
    ww_list = option(link_list);             {Definition(m_list, ww_list)}

top:
  | module_list = list(module_def); definition_list = list(definition); d = diagram; EOF  {Program(module_list, definition_list,d )}
