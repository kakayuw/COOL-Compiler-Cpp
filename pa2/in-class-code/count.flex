%option noyywrap

%{ 

#include <iostream>

int lettercount = 0; 
int linecount = 0; 
%} 
  
%% 

[A-Za-z]    {lettercount++;} 
\n          {linecount++;} 
.           {} 

%% 
  
int main(){ 
  
    yylex(); 

    std::cout << "lettercount="<<lettercount << std::endl; 
    std::cout << "linecount="<<linecount << std::endl; 

    return 0; 
} 
