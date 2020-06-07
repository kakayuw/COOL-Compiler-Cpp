#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

#include <vector>
#include <string>
#include <map>
#include <algorithm>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

#define FILENAME "str_const0"
#define CONST_FALSE "bool_const0"
#define CONST_TRUE "bool_const1"
#define DISPATCH_ABORT "_dispatch_abort"
#define CASE_ABORT2 "_case_abort2"
#define CASE_ABORT "_case_abort"


class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

// define global variables
typedef char* REG;


class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

public:
   CgenClassTable(Classes, ostream& str);
   // generate classnameTab
   void gen_classname_tab(ostream& s);
   // genreate classobjTab
   void gen_classobj_tab(ostream& s);
   void code();
   CgenNodeP root();
   static int aiid;
};

// maintain vairable context
class MemoryContext {
  public:
   virtual void load_symbol(REG reg, ostream& s) = 0;
   virtual void store_symbol(REG reg, ostream& s) = 0;
};

// variable stored on stack
class FunctionContext : public MemoryContext {
  public:
   FunctionContext(int size, REG reg) : offset(size), from_reg(reg) {}
   void load_symbol(REG reg, ostream& s);
   void store_symbol(REG reg, ostream& s);
   int offset;
   REG from_reg;
};

// variable stored in object
class ObjectContext : public MemoryContext {
  public:
   ObjectContext(int size) : offset(size) {}
   void load_symbol(REG reg, ostream& s);
   void store_symbol(REG reg, ostream& s);
   int offset;
};

// variable stored in register
class RegisterContext : public MemoryContext {
  public:
   RegisterContext(REG reg) : reg(reg) {}
   void load_symbol(REG reg, ostream& s);
   void store_symbol(REG reg, ostream& s);
   REG reg;
};

int CgenClassTable::aiid = 0;

// help format dispatch table
class DispatchTab 
{
 public:
   DispatchTab() {};
   DispatchTab(const DispatchTab& dpt);
   int get_method_pos_by_name(Symbol name);
   int offset = 3;
   std::vector<method_class *> local_methods; //including local methods and overriden; not including inherited
   std::vector<std::pair<std::string, std::string>> all_method_names;  // including methods inherited
   std::vector<std::pair<std::string, attr_class*>> attrs;
   int attr_start = 0;
   SymbolTable<Symbol, MemoryContext> scope;
   
};



class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   // scan AST to emit dispatch table
   int classtag = -1;
   DispatchTab disp_info;
   CgenClassTableP ct;
   void cgen_init(ostream& s);
   void cgen_method(ostream &s);
   void emit_init_def(ostream& s);
   void emit_method_def(method_class* method, ostream& s);
   void gen_disp_tab(ostream& s);
   void prototype(ostream& s);
   void collect_attr(attr_class * attr);
   void collect_method(method_class * method);
   // the following helps set the subclass range
   void set_classtag();
   int rangeTag = 0;
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


SymbolTable<Symbol, MemoryContext> *cgen_context;
bool compareClassname(std::pair<int, Symbol> p1, std::pair<int, Symbol> p2);
bool compareStringEntryP(std::pair<CgenNodeP, StringEntryP> p1, std::pair<CgenNodeP, StringEntryP> p2);
bool compareBranch(branch_class* b1, branch_class* b2);
void emit_method_frame_enter(int space, ostream& s);
void emit_method_frame_exit(int space, ostream& s);
void emit_method_frame_exit_para(int space, int n, ostream& s);
static char* str2charp(std::string str);
static int label_count = 0;
static std::map<Symbol, CgenNodeP> nodes_map;
static CgenNodeP current_node;
// The following stores corresponding string const name
std::vector<std::pair<CgenNodeP, StringEntryP>> classname_strconst;
// The following stores stack frame tmp variable positions in each method
static int stack_frame = 1;
static bool ARITH_MODE = false;