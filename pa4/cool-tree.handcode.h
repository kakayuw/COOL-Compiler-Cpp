//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include "symtab.h"
#define yylineno curr_lineno
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
        { stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;



#define Program_EXTRAS                          \
virtual void semant() = 0;                      \
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void semant();                                  \
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                    \
Symbol get_classname() { return name; }                \
Symbol get_parentname() { return parent; }             \
Features get_features() { return features; }           \
SymbolTable<Symbol, Entry>  objTable;                 \
SymbolTable<Symbol, tree_node>  methodTable;


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0;               \
virtual bool is_method() = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);                                 


#define method_EXTRAS                                      \
bool is_method() { return true; }                          \
Symbol get_methodname() { return name; }                   \
Formals get_formals() { return formals; }                  \
Symbol get_returntype() { return return_type; }            \
Expression get_expr() { return expr; }

#define attr_EXTRAS                                      \
bool is_method() { return false; }                       \
Symbol get_attrname() { return name; }                   \
Symbol get_typedecl() { return type_decl; }              \
Expression get_initexpr() { return init; }


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
Symbol get_formal_name() { return name; }       \
Symbol get_type_decl() {return type_decl; }          

#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;\
virtual Symbol semant_expression() = 0;

#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);                    \
Symbol get_name() { return name;}                       \
Symbol get_type_decl() { return type_decl; }            \
Expression get_expr() { return expr; }                  \
Symbol semant_expression();

#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual Symbol semant_expression() = 0;

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);        \
Symbol semant_expression();


#define assign_EXTRAS                           \
Symbol get_name() { return name; }              \
Expression get_expr() { return expr; }

#define plus_EXTRAS                             \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }      

#define sub_EXTRAS                              \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }      

#define mul_EXTRAS                              \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }      

#define divide_EXTRAS                           \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }     

#define neg_EXTRAS                              \
Expression get_e1() { return e1; }              \

#define lt_EXTRAS                               \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }     

#define eq_EXTRAS                               \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }     

#define leq_EXTRAS                              \
Expression get_e1() { return e1; }              \
Expression get_e2() { return e2; }    

#define comp_EXTRAS                             \
Expression get_e1() { return e1; }  

#define typcase_EXTRAS                          \
SymbolTable<Symbol, Entry>  caseTable;                 

#endif
