/* First part of the file: options and definitions*/

%option noyywrap

/* Code between %{ and %} is ouptut verbatim.*/
%{ 

#include <iostream>

#define WORD 1
#define NUMBER 2
#define MIXED 3

%} 

/* We have one definition, named LETTERSORNUMBERS.
 It is defined as any number (including zero) of
 uppercase letters, lowercase letters, and digits. */
LETTERSORNUMBERS [A-Za-z0-9]*

/* Second part of file begins here: patterns and actions */
%% 

[A-Za-z]+[0-9]+{LETTERSORNUMBERS}  {return MIXED;}
[0-9]+[A-Za-z]+{LETTERSORNUMBERS}  {return MIXED;}
[A-Za-z]+    {return WORD;}
[0-9]+       {return NUMBER;}
.            {} 

%% 
/* Third part of file begins here: normal code*/
  
int main(){ 
  
    while (int res = yylex()) {
        switch (res) {
            case MIXED:
                std::cout << "Found a combination word/number: " << yytext << std::endl;
                break;
            case WORD:
                std::cout << "Found a word: " << yytext << std::endl;
                break;
            case NUMBER:
                std::cout << "Found a number: " << yytext << std::endl;
                break;
        }
    }

    return 0; 
} 
