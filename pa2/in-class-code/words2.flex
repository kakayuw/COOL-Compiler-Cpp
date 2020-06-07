
%option noyywrap

%{ 

#include <iostream>

#define WORD 1
#define NUMBER 2
#define MIXED 3

%} 

LETTERSORNUMBERS [A-Za-z0-9]*

/*define an exclusive state*/
%x WORDSONLY

%% 

[A-Za-z]+[0-9]+{LETTERSORNUMBERS}  {return MIXED;}
[0-9]+[A-Za-z]+{LETTERSORNUMBERS}  {return MIXED;}
[A-Za-z]+    {return WORD;}
[0-9]+       {return NUMBER;}
\[            { /*Enter a state where we ignore numbers*/
                BEGIN(WORDSONLY);}
.            {} 

<WORDSONLY>{
    [A-Za-z]+    {return WORD;}
    \] { /*Return to normal state*/
         BEGIN(INITIAL);}
}

%% 
  
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
