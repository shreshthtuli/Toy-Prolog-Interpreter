%{
  open Solver
  open Printf
%}


%token END
%token SEP
%token <string> VAR CONST
%token COMMA
%token O_PAREN
%token C_PAREN
%token O_SQ
%token C_SQ
%token CONS
%token EOL
%token WHITESPACE
%token EOF
%token EQUAL
%token NOT_EQUAL
%token CUT
%token FAIL
%token <string> NUM
%token PLUS
%token MIN
%token MUL
%token DIV
%token FAIL

%nonassoc EQUAL
%nonassoc NOT_EQUAL
%start main
%start goal

%type <Solver.program> main
%type <Solver.goal> goal

%%

main :
  | EOF 		{[]}
  | clause main {($1)::($2)}
;

goal:
  | atom_list END          { $1 }

clause:
  | atom END               	   { Fact($1) }
  | atom SEP atom_list END     { Rule(($1),($3)) }
;

atom:
  | CONST O_PAREN term_list C_PAREN        { Atom($1,$3) }
  | term EQUAL term                        { Atom("$eq",[$1;$3]) }
  | term NOT_EQUAL term                    { Atom("$neq",[$1;$3]) }
  | O_PAREN atom C_PAREN                   { $2 }
  | CUT					   { Cut }
  | FAIL				   { Fail }
;

atom_list:
  | atom                   { [$1] }
  | atom COMMA atom_list   { ($1)::($3) }
;

term_list:
  | term                   { [$1] }
  | term COMMA term_list   { ($1)::($3) }
;

term:
  | VAR                    		  { Var($1) }
  | numeral_e                  		  { Const($1) }
  | CONST O_PAREN term_list C_PAREN       { Sym($1, $3) }
  | CONST 	           		  { Sym($1, []) }
  | O_SQ term CONS term C_SQ	  	  { PListp($2,$4) }
  | O_SQ term_list C_SQ 		  { PList($2) }
  | O_SQ C_SQ 				  { PList([]) }
;

numeral_e:
  | numeral_t				  { ($1) }
  | numeral_t PLUS numeral_e		  { ($1 + $3) }
;

numeral_t:
  | numeral_f				  { ($1) } 
  | numeral_f MUL numeral_t        	  { ($1 * $3) }
;

numeral_f:
  | NUM					  { (int_of_string($1)) }
  | O_PAREN numeral_e C_PAREN             { ($2) }
;



