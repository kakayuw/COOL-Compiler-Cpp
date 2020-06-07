%option noyywrap

%{ 

#include <iostream>

int lettercount = 0; 
int linecount = 0; 
%} 
  
%% 

(01|10)    {printf("apple\n");} 
1(01)*0          {printf("banana\n");} 
(1011*0)|(0100*1)           {printf("coconut\n");} 
. {}
%% 
  
int main(){ 
  
    yylex(); 


    return 0; 
} 
