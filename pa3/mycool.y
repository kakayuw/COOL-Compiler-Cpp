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
%type <features> dummy_feature_list
%type <features> feature_list
%type <feature> feature

%type <formals> parameter_list
%type <formal> parameter

%type <expression> exp
%type <expressions> exp_parameter_list
%type <expressions> block
%type <cases> branch
%type <expression> let_stmt

/* Precedence declarations go here. */
%right LET IN
%right ASSIGN
%left NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '/' '*'
%left ISVOID
%left '~'
%left '@'
%left '.'

%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list    { /* make sure bison computes location information */
                          @$ = @1;
                          ast_root = program($1); }
        ;

class_list
        : class                 /* single class */
                { $$ = single_Classes($1);
                  parse_results = $$; }
        | class_list class      /* several classes */
                { $$ = append_Classes($1,single_Classes($2)); 
                  parse_results = $$; }
        ;

/* If no parent is specified, the class inherits from the Object class. */
class   : CLASS TYPEID '{' dummy_feature_list '}' ';'
                { $$ = class_($2,idtable.add_string("Object"),$4, stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        | CLASS TYPEID '{'  feature_list  '}' ';'
                { $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename)); }
				| error	';'
								{ /* Skip error */ }
        ;

/* Feature list may be empty, but no empty features in list. */
dummy_feature_list:   
                {  $$ = nil_Features(); }

/* Feature could be method definition or attribute. */
feature  :      OBJECTID '('  ')' ':' TYPEID  '{' exp '}'
                                              { $$ = method($1, nil_Formals(), $5, $exp); }
            |   OBJECTID '(' parameter_list ')' ':' TYPEID '{' exp '}'
                                              { $$ = method($1, $3, $6, $exp); }
            |   OBJECTID  ':' TYPEID  ASSIGN  exp
                                              { $$ = attr($1, $3, $exp); }
            |   OBJECTID  ':' TYPEID  
                                              { $$ = attr($1, $3, no_expr()); }
						|		error
																							{ /* Skip and continue next correct class parsing. */ }
            ;

/* Feature list consists of one or more features. */
feature_list  :   feature ';'
                                        { $$ = single_Features($1); }
              |   feature_list feature ';' 
                                        { $$ = append_Features($1, single_Features($feature)); }
							;


/* Single parameter in method definition. */
parameter : OBJECTID ':' TYPEID
                            { $$ = formal($1, $3); }

/* Multiple parameters in list form. */
parameter_list  : parameter
                            { $$ = single_Formals($1); }
                | parameter_list  ',' parameter
                            { $$ = append_Formals($1, single_Formals($parameter)); }            
                ;

/* Expressions */
exp   :   OBJECTID ASSIGN exp[object]          
                                        { $$ = assign($1, $object); }
      |   exp '.' OBJECTID  '(' ')'     
                                        { $$ = dispatch($1, $3, nil_Expressions()); }
      |   exp '.' OBJECTID '('  exp_parameter_list  ')'
                                        { $$ = dispatch($1, $3, $5); }
      |   exp '@' TYPEID '.' OBJECTID '(' ')'
                                        { $$ = static_dispatch($1, $3, $5, nil_Expressions()); }
      |   exp '@' TYPEID '.' OBJECTID '(' exp_parameter_list[para] ')'
                                        { $$ = static_dispatch($1, $3, $5, $para); }
      |   OBJECTID  '(' ')'
                                        { $$ = dispatch(object(idtable.add_string("self")), $1, nil_Expressions()); }
      |   OBJECTID  '(' exp_parameter_list  ')'
                                        { $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
      |   IF  exp[if_exp] THEN  exp[then_exp] ELSE  exp[else_exp] FI
                                        { $$ = cond($if_exp, $then_exp, $else_exp); }
      |   WHILE exp[cond] LOOP  exp[body] POOL
                                        { $$ = loop($cond, $body); }
      |   '{' block '}'
                                        { $$ = block($block); }
      |   LET let_stmt
                                        { $$ = $let_stmt; }
      |   CASE  exp[casetype] OF branch ESAC
                                        { $$ = typcase($casetype, $branch); }
      |   NEW TYPEID      
                                        { $$ = new_($2); }
      |   ISVOID  exp     
                                        { $$ = isvoid($2); }
      |   exp[left] '+' exp[right]   
                                        { $$ = plus($left, $right); }
      |   exp[left] '-' exp[right]   
                                        { $$ = sub($left, $right); }
      |   exp[left] '*' exp[right]
                                        { $$ = mul($left, $right); }
      |   exp[left] '/' exp[right]
                                        { $$ = divide($left, $right); }
      |   '~' exp
                                        { $$ = neg($2); }
      |   exp[left] '<' exp[right]   
                                        { $$ = lt($left, $right); }
      |   exp[left] LE  exp[right]    
                                        { $$ = leq($left, $right); }
      |   exp[left] '=' exp[right]
                                        { $$ = eq($left, $right); }
      |   NOT exp
                                        { $$ = comp($2); }
      |   '(' exp ')'
                                        { $$ = $2; }
      |   OBJECTID
                                        { $$ = object($1); }
      |   INT_CONST 
                                        { $$ = int_const($1); }
      |   STR_CONST 
                                        { $$ = string_const($1); }
      |   BOOL_CONST 
                                        { $$ = bool_const($1); }
      ;

/* Expressions in parameter list */
exp_parameter_list  :
                        exp
                                                    { $$ = single_Expressions($1); }
                      | exp_parameter_list  ',' exp
                                                    { $$ =  append_Expressions($1, single_Expressions($3)); }
                      ;

/* Blocks of statements */
block :
          	exp ';'
                          { $$ = single_Expressions($1); }
        |   block exp ';'
                          { $$ = append_Expressions($1, single_Expressions($2)); }
        |		error	';'
						  						{	/* Do nothing when error encountered */	}
				| 	block	error	';'
													{ }
				;

/* CASE branches */
branch  : 
              OBJECTID  ':' TYPEID  DARROW  exp ';'
                                                      { $$ = single_Cases(branch($1, $3, $exp)); }
          |   branch  OBJECTID ':'  TYPEID DARROW exp ';'
                                                      { $$ = append_Cases($1, single_Cases(branch($2, $4, $exp))); }
          ;
          
/* Let statement */
let_stmt  :   OBJECTID ':' TYPEID IN exp
                                          { $$ = let($1, $3, no_expr(), $5); }
          |   OBJECTID ':' TYPEID ASSIGN exp[head] IN exp[body]
                                          { $$ = let($1, $3, $head, $body); }
          |   OBJECTID ':' TYPEID ',' let_stmt[rec]
                                          { $$ = let($1, $3, no_expr(), $rec); }
          |   OBJECTID ':' TYPEID ASSIGN exp ',' let_stmt[rec]
                                          { $$ = let($1, $3, $exp, $rec); }
					|		error	let_stmt
																					{ /* Skip when error occurs. */ }
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

