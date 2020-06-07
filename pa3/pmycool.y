/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* memory */
#define YYINITDEPTH 10000
#define YYMAXDEPTH 10000

extern char *curr_filename;

void yyerror(const char *s);        /*  defined below; called for each parse error */
extern int yylex();           /*  the entry point to the lexer  */

#define YYLTYPE int              /* the type of locations */
#define cool_yylloc curr_lineno
extern int node_lineno;          /* set before constructing a tree node
                                    to whatever you want the line number
                                    for the tree node to be */

/* The default action for locations.  Use the location of the first
   terminal/non-terminal and set the node_lineno to that value. */
#define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current = Rhs[1];                             \
  node_lineno = Current;

#define SET_NODELOC(Current)  \
  node_lineno = Current;


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

Program ast_root;             /* the result of the parse  */
Classes parse_results;        /* for use in semantic analysis */
int omerrs = 0;               /* number of errors in lexing and parsing */
%}

/* A union of all the types that can be the result of parsing actions. Don't change this.*/
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class

/* You will want to change the following line. */
%type <feature> feature
%type <features> dummy_feature_list

%type <case_> case
%type <cases> case_list

%type <formal> formal
%type <formals> formal_list 

%type <expression> expression
%type <expressions> expression_list
%type <expression> let_expr
%type <expressions> dummy_expression_list
%type <expression> dispatch

/* Precedence declarations go here. */
%right LET IN
%right ASSIGN
%left NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'


%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program :
	 class_list    { /* make sure bison computes location information */
                          @$ = @1;
                          ast_root = program($1); }
        ;

class_list
        : class ';'                /* single class */
                { $$ = single_Classes($1);
                  parse_results = $$; }
        | class ';' class_list    /* several classes */
                { $$ = append_Classes(single_Classes($1), $3); 
                  parse_results = $$; }
	| error ';' class_list
		{}
        ;

/* If no parent is specified, the class inherits from the Object class. */
class   : CLASS TYPEID '{' dummy_feature_list '}' 
                { $$ = class_($2,idtable.add_string("Object"),$4,
                              stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}' 
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
	| CLASS TYPEID '{' '}' 
		{ $$ = class_($2, idtable.add_string("Object"), nil_Features(),
				stringtable.add_string(curr_filename));}
	| CLASS TYPEID INHERITS TYPEID '{' '}' 
		{ $$ = class_($2, $4, nil_Features(), stringtable.add_string(curr_filename));}
        
	;

/* Feature list may be empty, but no empty features in list. */
dummy_feature_list:             /* empty */
	  feature ';'
		{ $$ = single_Features($1);}
	| feature ';' dummy_feature_list
		{ $$ = append_Features(single_Features($1), $3);}
	| error ';' dummy_feature_list
		{}
	| error ';'
		{}
	;

feature 
	: OBJECTID '(' ')' ':' TYPEID '{' expression '}'
		{ $$ = method($1, nil_Formals(), $5, $7);}
	| OBJECTID '(' formal ')' ':' TYPEID '{' expression '}'
		{ $$ = method($1, single_Formals($3), $6, $8);}	
	| OBJECTID '(' formal formal_list ')' ':' TYPEID '{' expression '}'
		{ $$ = method($1, append_Formals(single_Formals($3), $4), $7, $9);}
	| OBJECTID ':' TYPEID 
		{ $$ = attr($1, $3, no_expr());}
	| OBJECTID ':' TYPEID ASSIGN expression
		{ $$ = attr($1, $3, $5);}
	;

formal
	: OBJECTID ':' TYPEID
		{ $$ = formal($1,$3);}
	;

/* formal list in function declration */
formal_list
	: ',' formal 
		{ $$ = single_Formals($2); }
	| ',' formal formal_list
		{ $$ = append_Formals(single_Formals($2), $3);}

	;

let_expr
	: OBJECTID ':' TYPEID IN expression
		{ $$ = let($1, $3, no_expr(), $5);}
	| OBJECTID ':' TYPEID ASSIGN expression IN expression
		{ $$ = let($1, $3, $5, $7);}
	| OBJECTID ':' TYPEID ',' let_expr
		{ $$ = let($1, $3, no_expr(), $5);}
	| OBJECTID ':' TYPEID ASSIGN expression ',' let_expr
		{ $$ = let($1, $3, $5, $7);}
	| error let_expr {} 
	;

dummy_expression_list
	:
	 	  expression
		{ $$ = single_Expressions($1);}
	| expression ',' dummy_expression_list
		{ $$ = append_Expressions(single_Expressions($1), $3);}
	;

expression_list // at least one expression
	: expression ';'/*single expression*/
		{ $$ = single_Expressions($1);}
	| expression ';' expression_list
		{ $$ = append_Expressions(single_Expressions($1), $3);}
	| error ';' {}
	| error ';' expression_list {}
	
	;

dispatch
	: expression '.' OBJECTID '('  ')'
		{ $$ = dispatch($1, $3, nil_Expressions());}
	| expression '.' OBJECTID '(' dummy_expression_list ')'
		{ $$ = dispatch($1, $3, $5);}
	| expression '@' TYPEID '.' OBJECTID '(' dummy_expression_list ')'
		{ $$ = static_dispatch($1, $3, $5, $7);}
	| expression '@' TYPEID '.' OBJECTID '('  ')'
		{ $$ = static_dispatch($1, $3, $5, nil_Expressions());}
	| OBJECTID '(' dummy_expression_list ')'/*shorhand for self.f(...)*/
		{ $$ = dispatch(object(idtable.add_string("self")), $1, $3);}
	| OBJECTID '('  ')'/*shorhand for self.f(...)*/
		{ $$ = dispatch(object(idtable.add_string("self")), $1, nil_Expressions());}
	;

expression
	: dispatch 
		{ $$ = $1;}
	| IF expression THEN expression ELSE expression FI
		{ $$ = cond($2, $4, $6);}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
	| WHILE expression LOOP expression POOL
		{ $$ = loop($2, $4); }
	| '{' expression_list '}'
		{ $$ = block($2);}
	| LET let_expr
		{ $$ = $2;}
	| LET error
		{}
	| CASE expression OF case_list ESAC
		{ $$ = typcase($2, $4);}
	| OBJECTID ASSIGN expression
		{ $$ = assign($1, $3);}
	| NEW TYPEID
		{ $$ = new_($2);}
	| ISVOID expression
		{ $$ = isvoid($2);}
	| expression '+' expression
		{ $$ = plus($1, $3);}
	| expression '-' expression
		{ $$ = sub($1, $3);}
	| expression '*' expression
		{ $$ = mul($1, $3);}
	| expression '/' expression
		{ $$ = divide($1, $3);}
	| '~' expression
		{ $$ = neg($2); }
	| expression '<' expression
		{ $$ = lt($1, $3);}
	| expression LE expression
		{ $$ = leq($1, $3);}
	| expression '=' expression
		{ $$ = eq($1, $3);}
	| NOT expression
		{ $$ = comp($2);}
	| '(' expression ')' 
		{ $$ = $2;} 
	| OBJECTID /*object id*/
		{ $$ = object($1);}
	| INT_CONST /*integer*/
		{ $$ = int_const($1); }
	| STR_CONST/*string*/
		{ $$ = string_const($1);}
	| BOOL_CONST /*boolean*/
		{ $$ = bool_const($1);}
	;


case 
	: OBJECTID ':' TYPEID DARROW expression
		{ $$ = branch($1, $3, $5);}
	; 
	
case_list
	: case ';'
		{ $$ = single_Cases($1);}
	| case_list case ';'
		{ $$ = append_Cases($1, single_Cases($2));}
	;

/* end of grammar */ 
%%

/* This function is called automatically when Bison detects a parse error. Don't change this. */
void yyerror(const char *s)
{
  extern int curr_lineno;

  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}

