/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

%option noyywrap

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
        if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
                YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int nested_comment_count = 0;
%}

/*
 * Define names for regular expressions here.
 */
%x COMMENT
%x INLINE_COMMENT
%x QUOTE
  /* DESSERTED mode used when invalid chars in string report an ERROR */
%x DESSERTED
  /* Double char operators */
DARROW_SYM      "=>"
ASSIGN_SYM      "<-"
LE_SYM          "<="
 /* Single char char operators and special chars */
NEWLINE         \n
LPAREN          "("
RPAREN          ")"
ADD             "+"
DIVIDE          "/"
MINUS           "-"
TIMES           "*"
EQUAL           "="
LESS            "<"
DOT             "."
COUNTER         "~"
COMMA           ","
SEMIL           ";"
COLON           ":"
AT              "@"
LBRACE          "{"
RBRACE          "}"
 /* Boolean constants */
BOOL_TRUE       true
BOOL_FALSE      false
 /* Object and class */
OBJECT_NAME     [a-z][A-Za-z0-9_]*
CLASS_NAME      [A-Z][A-Za-z0-9_]*
INT             [0-9]+
 /* Special chars that be escaped in String */
STRING_SLASH_N  \\n
STRING_SLASH_T  \\t
STRING_SLASH_R  \\r
STRING_SLASH_B  \\b
STRING_SLASH_F  \\f
STR_ESC_CHAR    \\(.|\n)
STRING_NULL     \0
  /* ERROR CONTROL */
ILLEGAL_SLASH   \\
UNMATCHED_COM   "*)"

%%

 /*
  *  Nested comments
  */

"--"                    { BEGIN(INLINE_COMMENT); }
<INLINE_COMMENT>{
  {NEWLINE}             { curr_lineno++; BEGIN(INITIAL); }
  .                     {}
}

"(*"                    { BEGIN(COMMENT);
                          nested_comment_count = 1; }

<COMMENT>{
  {NEWLINE}             curr_lineno++;
  "(*"                  nested_comment_count++;
  "*)"                  { nested_comment_count--;
                          if (nested_comment_count == 0) BEGIN(INITIAL); }
  <<EOF>>               { yylval.error_msg = "EOF in comment";
                          BEGIN(INITIAL);
                          return(ERROR); }
  .                     {}
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  */
\"                      { string_buf_ptr = string_buf; 
                          BEGIN(QUOTE); }
<QUOTE>{
  \"                    { BEGIN(INITIAL);     /* Closing quote */
                          *string_buf_ptr = '\0';
                          yylval.symbol = stringtable.add_string(string_buf, strlen(string_buf));
                          return (STR_CONST); }
  {NEWLINE}             { curr_lineno++;
                          yylval.error_msg = "Unterminated string constant";
                          BEGIN(INITIAL);
                          return (ERROR); }
  {STRING_SLASH_N}      { *string_buf_ptr++ = '\n'; }
  {STRING_SLASH_T}      { *string_buf_ptr++ = '\t'; }
  {STRING_SLASH_B}      { *string_buf_ptr++ = '\b'; }
  {STRING_SLASH_F}      { *string_buf_ptr++ = '\f'; }
  {STR_ESC_CHAR}        { *string_buf_ptr++ = yytext[1];
                          if (yytext[1]=='\n') curr_lineno++; }
  {STRING_NULL}         { yylval.error_msg = "String contains null character.";
                          BEGIN(DESSERTED);
                          return(ERROR);}
  <<EOF>>               { yylval.error_msg = "EOF in string constant";
                          BEGIN(INITIAL);
                          return(ERROR); }
  .                     { if (string_buf_ptr - string_buf >= MAX_STR_CONST) {
                            yylval.error_msg = "String constant too long";
                            BEGIN(DESSERTED);
                            return(ERROR);
                          } else {
                            *string_buf_ptr++ = yytext[0];
                          } }
}

 /*
  *  DESSERTED mode are activated when ERROR occur in String constant scanning.
  *  Eat up all chars until '\n' or tailing '"'
  */
<DESSERTED>{
  {STR_ESC_CHAR}        { if (yytext[1]=='\n') curr_lineno++; } /* eat \" in source code except "\\n" */
  {NEWLINE}             { curr_lineno++; BEGIN(INITIAL); }
  <<EOF>>               |
  \"                    { BEGIN(INITIAL); }
  .                     /* eat anything that's not a '"' or '\n' */
}

 /*
  *  The multiple-character operators.
  */
{DARROW_SYM}            { return (DARROW); }
{ASSIGN_SYM}            { return (ASSIGN); }
{LE_SYM}                { return (LE); }

 /*
  * Handle single char  
  */
{NEWLINE}               { curr_lineno++; }
{LPAREN}                { return '('; }
{RPAREN}                { return ')'; }
{ADD}                   { return '+'; }
{DIVIDE}                { return '/'; }
{MINUS}                 { return '-'; }
{TIMES}                 { return '*'; }
{EQUAL}                 { return '='; }
{LESS}                  { return '<'; }
{DOT}                   { return '.'; }
{COUNTER}               { return '~'; }
{COMMA}                 { return ','; }
{SEMIL}                 { return ';'; }
{COLON}                 { return ':'; }
{AT}                    { return '@'; }
{LBRACE}                { return '{'; }
{RBRACE}                { return '}'; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:CLASS)              { return (CLASS); }
(?i:ELSE)               { return (ELSE); }
(?i:IF)                 { return (IF); }
(?i:FI)                 { return (FI); }
(?i:IN)                 { return (IN); }
(?i:INHERITS)           { return (INHERITS); }
(?i:LET)                { return (LET); }
(?i:LOOP)               { return (LOOP); }
(?i:POOL)               { return (POOL); }
(?i:THEN)               { return (THEN); }
(?i:WHILE)              { return (WHILE); }
(?i:CASE)               { return (CASE); }
(?i:ESAC)               { return (ESAC); }
(?i:OF)                 { return (OF); }
(?i:NEW)                { return (NEW); }
(?i:NOT)                { return (NOT); }
(?i:ISVOID)             { return (ISVOID); }

 /* Handle boolean constants, class and object tokens */
{BOOL_TRUE}|{BOOL_FALSE}  { cool_yylval.boolean = strcmp(yytext, "true")==0 ? 1 : 0; 
                            return (BOOL_CONST); }
{INT}                 { cool_yylval.symbol = inttable.add_string(yytext, yyleng);
                        return (INT_CONST); }
{CLASS_NAME}          { cool_yylval.symbol = idtable.add_string(yytext, yyleng);
                        return (TYPEID); }
{OBJECT_NAME}         { cool_yylval.symbol = idtable.add_string(yytext, yyleng);
                        return (OBJECTID); }

 /* Error handling */
{ILLEGAL_SLASH}       { yylval.error_msg = "\\"; return(ERROR); }
{UNMATCHED_COM}       { yylval.error_msg = "Unmatched *)"; return(ERROR); }
               
[ \t\r\f\v]+          /* eat the whitespace */
.                     { yylval.error_msg = yytext; return(ERROR); }





%%