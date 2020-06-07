
%option noyywrap

%{ 

#include <iostream>

#define ERROR 1
#define NUMBER 2
#define OPERATOR 3

%} 

%% 

-?[0-9]+      {return NUMBER;}
[+\-*]        {return OPERATOR;}
[ \t\n]       {}
.             {return ERROR;} 

%% 
  
int main(){ 
  
    while (int res = yylex()) {
        switch (res) {
            case ERROR:
                std::cout << "Got an unexpected character, terminating: " << yytext << std::endl;
                return 1;
            case OPERATOR:
                std::cout << "Found an operator: " << yytext << std::endl;
                break;
            case NUMBER:
                std::cout << "Found a number: " << yytext << std::endl;
                break;
        }
    }

    return 0; 
} 
