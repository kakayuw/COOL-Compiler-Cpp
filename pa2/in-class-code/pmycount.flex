%option noyywrap

%{ 

#include <iostream>

int lettercount = 0; 
int linecount = 0; 
%} 
  
%% 

(01|10)         		{std::cout << "apple" << std::endl; } 
1(01)*0 				{std::cout << "banana"<< std::endl; } 
(1011*0|0100*1)       {std::cout << "coconont" << std::endl; } 
[\n]					{}
.						{}
%% 
  
int main(){ 
  
    yylex(); 

    return 0; 
} 
