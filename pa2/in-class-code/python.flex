/*
very limited Python-style indentation parser

flex python.flex  && g++ ./lex.yy.c  && ./a.out < python.py

*/


%option noyywrap


%{
#include <iostream>
#include <string>
#include <vector>
using namespace std;
#define YYSTYPE string
string yylval;
vector<int> indents;
int dedent_count=0;
#define IF 256
#define DEF 257
#define ELSE 258
#define IDENT 259
#define CLASS 260
#define INDENT 261
#define DEDENT 262
%}

%% 

                        {
                            if (dedent_count>0) {
                                dedent_count--;
                                return DEDENT;
                            }
                        }
if                      {return IF;}
else                    {return ELSE;} 
def                     {return DEF;}
class                   {return CLASS;}
[_a-zA-Z][_a-zA-Z0-9]*  {yylval=yytext; return IDENT;}
[\:\.\,\(\)\*]          {return *yytext;}
\n[ ]*                  {
                            if (indents.size()==0) {
                                indents.push_back(yyleng);
                                return INDENT;
                            }
                            if (yyleng > indents.back()) {
                                indents.push_back(yyleng);
                                return INDENT;
                            }
                            else {
                                while (yyleng < indents.back()) {
                                    indents.pop_back();
                                    dedent_count++;
                                }
                            }
                            if (dedent_count>0) {
                                dedent_count--;
                                return DEDENT;
                            }
                        }
[ \t]+                  {}

%%

string toktype(int tok) {
    switch (tok) {
        case IF:
            return "IF";
            break;
        case DEF:
            return "DEF";
            break;
        case ELSE:
            return "ELSE";
            break;
        case CLASS:
            return "CLASS";
            break;
        case IDENT:
            return "IDENT(" + yylval + ")";
            break;
        case INDENT:
            return "Begin Indentation";
            break;
        case DEDENT:
            return "End Indentation";
            break;
        default:
            return "Punctuation("+string(1,(char)tok)+")";
    }
}

int main() {
    while (int tok = yylex()) {
        cout << "Got token " << toktype(tok) << endl;
    }
    return 0;
}