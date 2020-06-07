#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "list.h"
#include <stack>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// def Class_ implement class class_
typedef class__class *class_c;
typedef method_class *method_;
typedef attr_class *attr_;
typedef formal_class *formal_;
typedef branch_class *branch_;

typedef SymbolTable<Symbol, tree_node>& symtab;
typedef SymbolTable<Symbol, Entry>& vartab;

// Add global symtab
SymbolTable<Symbol, tree_node> class_tab; 
class_c current_class = NULL;
ClassTable* sc;
int inherit_error_count = 0;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  void semant_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream& semant_error(Class_ c, tree_node *t);
  bool checkMain(); 
  int check_cycle(class_c cur);
  bool report_inheritance_error(std::stack<class_c>& reverse_errors);
  // add custom node sement analyser
  void semant_analyse(Classes);  // entrypoint of semant classes
  bool semant(class_c cur);  // class sement checker (1p)
  void semant(class_c cur, attr_ attr);  // method sement checker (1p)
  void semant(class_c cur, method_ method); // attribute sement chekcer (1p)
  void semant_deep(class_c cur); // class attribute and expr sement checker (2p)
  void semant_deep(class_c cur, attr_ attr);  // method sement checker (2p)
  void semant_deep(class_c cur, method_ method); // attribute sement chekcer (2p)
  void inherit_features(class_c cur); // inherits attributes and methods in table envs (2p)
  bool semant_fml(class_c cur, formal_ formal); // parameter sement checker (2p)
  Symbol semant_exp(class_c cur, Expression exp); // expression sement checker (2p)
};

bool is_subclass(Symbol ancestor, Symbol child);  // check one class is subclass of the other
Symbol lca(Symbol a, Symbol b) ;
bool is_builtin_method(class_c builtin_class, Symbol method) ;
bool is_non_inherit_builtin_class(Symbol builtin);
bool is_builtin_class(Symbol builtin);
Symbol semant_expr_error(const char* msg, tree_node *node);
attr_ fetch_attr(class_c cur, Symbol name);
Symbol get_ancient_builtin(class_c cur);

#endif

