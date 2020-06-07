
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
int curr_lineno;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{
   if (std::string(dest) == SP && std::string(dest) == SP && imm == 4) {
    //  stack_frame--;   // pop here
     ;
   } else if (std::string(dest) == SP && std::string(dest) == SP && imm == -4) {
    //  stack_frame++;   // push here
     ;
   }

   s << ADDIU << dest << " " << src1 << " " << imm << endl; 
}

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  /*if (source != A1)*/ emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

// Some self defined emit functions
static void emit_block_header(ostream& s) {
	s << ":" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << "String" << DISPTAB_SUFFIX << endl;                // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << "Int" << DISPTAB_SUFFIX << endl;               // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << "Bool" << DISPTAB_SUFFIX <<  endl;             // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
  // // add main entrance
  // str << GLOBAL;
  // str << "main" << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   
   root()->set_classtag();

   stringclasstag = nodes_map[Str]->classtag; /* Change to your String class tag here */;
   intclasstag =    nodes_map[Int]->classtag; /* Change to your Int class tag here */;
   boolclasstag =   nodes_map[Bool]->classtag; /* Change to your Bool class tag here */;

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  // generate class_nameTab
  gen_classname_tab(str);
  // generate class_objTab
  gen_classobj_tab(str);
  // generate dispatch tables
	root()->gen_disp_tab(str);
  // generate prototypes
  root()->prototype(str);

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
	root()->cgen_init(str);
  root()->cgen_method(str);
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

// generate class_nameTab
void CgenClassTable::gen_classname_tab(ostream &s) {
  s << CLASSNAMETAB;
  emit_block_header(s);
  std::sort(classname_strconst.begin(), classname_strconst.end(), compareStringEntryP);
    for (auto it = classname_strconst.begin(); it != classname_strconst.end(); it++) {
      s << WORD;     (*it).second->code_ref(s);     s << endl;
    }
}

// generate class_objTab
void CgenClassTable::gen_classobj_tab(ostream &s) {
  s << CLASSOBJTAB;
  emit_block_header(s);
  // use vector to sort map
	std::vector<std::pair<int, Symbol>> vec;
  for (auto it2 = nodes_map.begin(); it2 != nodes_map.end(); it2++) {
    vec.push_back(std::make_pair((*it2).second->classtag, (*it2).first));
  }
  std::sort(vec.begin(), vec.end(), compareClassname);
  // emit obj table
  for (auto it = vec.begin(); it != vec.end(); it++) {
    Symbol classname = (*it).second;
    s << WORD << classname << PROTOBJ_SUFFIX << endl;
    s << WORD << classname << CLASSINIT_SUFFIX << endl;
  }
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP classtable) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
	// ADD CLASS TABLE
	ct(classtable)
{ 
    // Add class name to string table
    if (name != prim_slot && name != SELF_TYPE && name != No_class) {
      nodes_map.emplace(name, this);
      StringEntryP str_ep = stringtable.add_string(name->get_string());
      classname_strconst.push_back(std::make_pair(this, str_ep));  
    }
}

// set classtag
void CgenNode::set_classtag() {
  this->classtag = CgenClassTable::aiid++;
  List<CgenNode> * children = get_children();
  while (children) {
    children->hd()->set_classtag();
    children = children->tl();
  }
  this->rangeTag = CgenClassTable::aiid-1;
}


//////////////////////////////////////////////////////////////
///       INIT CLASS REALTED            
//////////////////////////////////////////////////////////////

// code generator for class init
void CgenNode::cgen_init(ostream& s) {
  // context switch
  current_node = this;
  cgen_context = &disp_info.scope;
  // emit class init
	emit_init_def(s);
  // recurrence on child nodes
  List<CgenNode> * children = get_children();
  while (children) {
    children->hd()->cgen_init(s);
    children = children->tl();
  }
}

// code generator for class init
void CgenNode::cgen_method(ostream& s) {
  // context switch
  current_node = this;
  cgen_context = &disp_info.scope;
  // omit methods if its COOL basic class
  if (name == Object || name == IO || name == Str || name == Int || name == Bool) ;
  else {
    // emit method definition
    for (method_class* method: disp_info.local_methods)
      emit_method_def(method, s);
  }
  // recurrence on child nodes
  List<CgenNode> * children = get_children();
  while (children) {
    children->hd()->cgen_method(s);
    children = children->tl();
  }
}


// initializition of method
void CgenNode::emit_init_def(ostream& s){
	emit_init_ref(this->name, s);
	emit_block_header(s);
  // collect tmp variable number first
  int var_space = 0;
	for (size_t i = disp_info.attr_start; i < disp_info.attrs.size(); i++) {
    int tmp = disp_info.attrs[i].second->init->get_let();
    var_space = var_space > tmp ? var_space : tmp;
	}
	emit_method_frame_enter(var_space, s);
  // context enter
  cgen_context->enterscope();
	// run parent init method
  if (get_parent() != No_class) {
    std::string init_name = std::string(this->get_parent()->get_string()) + CLASSINIT_SUFFIX;
    emit_jal(str2charp(init_name), s);
  }
	// init attributes: only init attrs defined in current class
	for (size_t i = disp_info.attr_start; i < disp_info.attrs.size(); i++) {
    // check if no_expr, i.e. we use default value
    if (dynamic_cast<no_expr_class*>(disp_info.attrs[i].second->init) != nullptr)
      continue;
		disp_info.attrs[i].second->init->code(s);
		// ACC stores the result
    emit_store(ACC, 3 + i, SELF, s);
	}
  cgen_context->exitscope();
  // need to set ACC to self
  emit_move(ACC, SELF, s);
	emit_method_frame_exit(var_space, s);
}

// emit method definition
void CgenNode::emit_method_def(method_class* method, ostream& s) {
	// method def header
	emit_method_ref(this->name, method->name, s);
	emit_block_header(s);
	// register push stack
  int var_space = method->expr->get_let();
	emit_method_frame_enter(var_space, s);
	// formal push stack
	cgen_context->enterscope();
  int leng = method->formals->len();
  for (int i = 0; i < leng; i++) {
    auto formal = (formal_class *) method->formals->nth(i);
    cgen_context->addid(formal->name, new FunctionContext(leng-1 - i + 3, FP));
  }
  // generate code for block
  method->expr->code(s);
	// register pop stack
  cgen_context->exitscope();
	emit_method_frame_exit_para(var_space, leng, s);
}

// emit method framestack: entry part
void emit_method_frame_enter(int stack_size, ostream& s) {
	emit_addiu(SP, SP, -3*WORD_SIZE, s);
	// store registers into stack
	emit_store(FP, 3, SP, s);
	emit_store(SELF, 2, SP, s);
	emit_store(RA, 1, SP, s);
	// set frame pointer
	emit_addiu(FP, SP, WORD_SIZE, s);
	// record accumulator
	emit_move(SELF, ACC, s);
  // reset stack_frame counter and alloc space for tmp variable
  if (stack_size > 0) emit_addiu(SP, SP, -stack_size * WORD_SIZE, s); 
  stack_frame = 1;
};

// emit method framestack: restore part
void emit_method_frame_exit(int stack_size, ostream& s) {
  // reset stack_from counter and dealloc space
  if (stack_size > 0) emit_addiu(SP, SP, stack_size * WORD_SIZE, s); 
  stack_frame = 1;
	// retreive registres from stack
	emit_load(FP, 3, SP, s);
	emit_load(SELF, 2, SP, s);
	emit_load(RA, 1, SP, s);
	// restore ESP
	emit_addiu(SP, SP, 3*WORD_SIZE, s);
	// return
	emit_return(s);
};

// emit method framestrack: restore part with paras
void emit_method_frame_exit_para(int stack_size, int para_nums, ostream &s) {
  // reset stack_from counter and dealloc space
  if (stack_size > 0) emit_addiu(SP, SP, stack_size * WORD_SIZE, s); 
  stack_frame = 1;
	// retreive registres from stack
	emit_load(FP, 3, SP, s);
	emit_load(SELF, 2, SP, s);
	emit_load(RA, 1, SP, s);
	// restore ESP
	emit_addiu(SP, SP, (para_nums + 3)*WORD_SIZE, s);
	// return
	emit_return(s);
}
//////////////////////////////////////////////////////////////
///       DISPATCH TABLE RELATED            
//////////////////////////////////////////////////////////////
void CgenNode::gen_disp_tab(ostream& s) {
	disp_info = DispatchTab(get_parentnd()->disp_info);

	for (int i = features->first(); features->more(i); i = features->next(i)) {
		attr_class *attr = dynamic_cast<attr_class *> (features->nth(i));
		if (attr == NULL) {
			collect_method(dynamic_cast<method_class *> (features->nth(i)));
		} else {
			collect_attr(attr);
		}
	}
	// emit dispatch table
  emit_disptable_ref(this->name, s);
  emit_block_header(s);
  for (auto kv: disp_info.all_method_names) {
    s << WORD << kv.first << '.' << kv.second << endl;
  }

  // recurrence on child nodes
  List<CgenNode> * children = get_children();
  while (children) {
    children->hd()->gen_disp_tab(s);
    children = children->tl();
  }
}

// collect attribute information
void CgenNode::collect_attr(attr_class *attr) {
  disp_info.scope.addid(attr->name, new ObjectContext(disp_info.attrs.size() + 3));
  disp_info.attrs.push_back(make_pair(std::string(attr->name->get_string()), attr));
}

// collect method information
void CgenNode::collect_method(method_class *method) {
	// assemble method offset dispatch table
	this->disp_info.local_methods.push_back(method);
	// override inherited methods
  bool overriden = false;
  std::string this_name = std::string(method->name->get_string());
  for (size_t i = 0; i < disp_info.all_method_names.size(); i++) {
    if (disp_info.all_method_names[i].second == this_name) {
      overriden = true;
      disp_info.all_method_names[i] = make_pair(std::string(this->name->get_string()), disp_info.all_method_names[i].second);
    }
  }
  if (!overriden) {
    disp_info.all_method_names.push_back(make_pair(std::string(this->name->get_string()), this_name));
  }
}

//////////////////////////////////////////////////////////////
///       PROTOTPYE OBJECT RELATED         
//////////////////////////////////////////////////////////////
void CgenNode::prototype(ostream& s) {
  // add -1 eye catcher
  s << WORD << "-1" << endl;
  // emit protytpe
  emit_protobj_ref(this->name, s);
  emit_block_header(s);
  // emit tag
  s << WORD << this->classtag << endl;
  // emit object length
  s << WORD << disp_info.attrs.size() + 3 << endl;
  // emit dispatch table ref
  s << WORD << this->name << DISPTAB_SUFFIX << endl;
  // emit each attribute
  for (auto kv : disp_info.attrs) {
    if (kv.second->type_decl == Str) {
      auto str_entry = dynamic_cast<StringEntryP> (stringtable.lookup_string(""));
      s << WORD;
      str_entry->code_ref(s);
      s << endl;
    } else if (kv.second->type_decl == Int) {
      auto str_entry = dynamic_cast<IntEntryP> (inttable.lookup_string("0"));
      s << WORD;
      str_entry->code_ref(s);
      s << endl;
    } else if (kv.second->type_decl == Bool) {
      s << WORD << BOOLCONST_PREFIX << "0" << endl;
    } else {
      s << WORD << "0" << endl;
    }
  }
  // recurrence on child nodes
  List<CgenNode> * children = get_children();
  while (children) {
    children->hd()->prototype(s);
    children = children->tl();
  }
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

static void emit_pop(REG popto, ostream &s) {
  emit_load(popto, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
}

static void emit_init_default(Symbol type_decl, ostream &s) {
  if (type_decl == Int) {
    // load address
    s << LA << ACC << " "; inttable.lookup_string("0")->code_ref(s);  s<< endl;
    emit_push(T1, s);
    emit_push(T2, s);
    emit_jal("Object.copy", s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_jal("Int_init", s);
  } else if (type_decl == Bool) {
    s << LA << ACC << " "; falsebool.code_ref(s);  s<< endl;
    emit_push(T1, s);
    emit_push(T2, s);
    emit_jal("Object.copy", s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_jal("Bool_init", s);
  } else if (type_decl == Str) {
    s << LA << ACC << " "; stringtable.lookup_string("")->code_ref(s);  s<< endl;
    emit_push(T1, s);
    emit_push(T2, s);
    emit_jal("Object.copy", s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_jal("String_init", s);
  } else {
    emit_move(ACC, ZERO, s);
  }
}

static void emit_push_stack_regs(std::vector<REG> regs, ostream &s) {
  for (auto it =  regs.begin(); it != regs.end(); it++) {
    emit_push(*it, s);
  }
}

static void emit_pop_stack_regs(std::vector<REG> regs, ostream &s) {
  for (auto it = regs.rbegin(); it != regs.rend(); it ++) {
    emit_pop(*it, s);
  }
}

static CgenNodeP get_cgnode_by_expr(Expression_class * expr) {
  if (expr->get_type() == SELF_TYPE) {
    return current_node;
  } else {
    return nodes_map[expr->type];
  }
}




////////////////////////////////////////////////////////////////
void assign_class::code(ostream &s) {
  // execute expression
  expr->code(s);
  cgen_context->lookup(name)->store_symbol(ACC, s);
}

void static_dispatch_class::code(ostream &s) {
  std::vector<REG> regs = {T1};
  emit_push_stack_regs(regs, s);

  // arguments push stack
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
  }
  // ACC: new object
  expr->code(s);    
  // void dispatch detection
  emit_bne(ACC, ZERO, label_count, s);
  emit_load_address(ACC, FILENAME, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal(DISPATCH_ABORT, s);
  emit_label_def(label_count++, s);
  // running method
  int method_loc = get_cgnode_by_expr(expr)->disp_info.get_method_pos_by_name(name);
  char *dt = str2charp(std::string(type_name->get_string()) + DISPTAB_SUFFIX);
  emit_load_address(T1, dt, s);
  emit_load(T1, method_loc, T1, s);
  emit_jalr(T1, s);
  // ACC: dispatch return value
  
  emit_pop_stack_regs(regs, s);
}

void dispatch_class::code(ostream &s) {
  // store regs on stack
  // because they may get changed by recursive dispatch
  std::vector<REG> regs = {T1, T2, T3};
  emit_push_stack_regs(regs, s);

  // arguments push stack
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
  }
  // ACC: new object
  expr->code(s);
  // void dispatch detection
  emit_bne(ACC, ZERO, label_count, s);
  emit_load_address(ACC, FILENAME, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal(DISPATCH_ABORT, s);
  emit_label_def(label_count++, s);
  // running method
  int method_loc = get_cgnode_by_expr(expr)->disp_info.get_method_pos_by_name(name);
  emit_load(T1, 2, ACC, s);
  emit_load(T1, method_loc, T1, s);
  emit_jalr(T1, s);
  // ACC: dispatch return value
  
  emit_pop_stack_regs(regs, s);
}

void cond_class::code(ostream &s) {
  std::vector<REG> regs = {T1};
  emit_push_stack_regs(regs, s);
  // eval condition
  pred->code(s);
  emit_load(T1, 3, ACC, s);
  // def labels
  int f_label = label_count, t_label = label_count + 1;
  label_count += 2; // update value of lable_count before recursive code generation
  emit_beqz(T1, f_label, s);
  then_exp->code(s);
  emit_branch(t_label, s);
  emit_label_def(f_label, s);
  else_exp->code(s);
  emit_label_def(t_label, s);
  emit_pop_stack_regs(regs, s);
}

void loop_class::code(ostream &s) {
  std::vector<REG> regs = {T1};
  emit_push_stack_regs(regs, s);
  int loop = label_count, exit = label_count+1;
  label_count += 2;
  // label 1: while condition
  emit_label_def(loop, s);
  pred->code(s);
  emit_load(T1, 3, ACC, s);
  emit_beq(T1, ZERO, exit, s);
  body->code(s);
  emit_branch(loop, s);
  emit_label_def(exit, s);
  emit_move(ACC, ZERO, s);  // while return void 
  emit_pop_stack_regs(regs, s);
}

void typcase_class::code(ostream &s) {
  std::vector<REG> regs = {T1, T2, T3};
  emit_push_stack_regs(regs, s);
  // init evaluate
  expr->code(s);
  // emit jump one
  int label = label_count;
  label_count += cases->len() + 2;
  emit_bne(ACC, ZERO, label, s);
  // case abort2
  emit_load_address(ACC, FILENAME, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal(CASE_ABORT2, s);
  emit_label_def(label, s);
  // compare classtag
  emit_load(T2, 0, ACC, s);
  // branch sorted by classtag descending order
  std::vector<branch_class*> branches;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branches.push_back(static_cast<branch_class *>(cases->nth(i)));
  }
  std::sort(branches.begin(), branches.end(), compareBranch);
  // emit branch
  for (size_t i = 0; i < branches.size(); i++) {
    auto branch = branches[i];
    auto decl_class = nodes_map[branch->type_decl];
    emit_blti(T2, decl_class->classtag, label+i+1, s);
    emit_bgti(T2, decl_class->rangeTag, label+i+1, s);
    // use T3 to store tmp variable avoiding using stack
    emit_move(T3, ACC, s);
    cgen_context->enterscope();
    cgen_context->addid(branch->name, new RegisterContext(T3));
    // evaluate body
    branch->expr->code(s);
    cgen_context->exitscope();
    // return
    emit_branch(label+cases->len()+1, s);
    emit_label_def(label+i+1, s);
  }
  // case abort
  emit_jal(CASE_ABORT, s);
  // exit label
  emit_label_def(label+cases->len()+1, s);
  emit_pop_stack_regs(regs, s);
}

void block_class::code(ostream &s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
  // init; ACC <- result
  // if init expr is null, use default value
  if (dynamic_cast<no_expr_class*>(init) != nullptr) {
    emit_init_default(type_decl, s);
  } else {
    init->code(s);
  }
  // add counter first , then push value
  cgen_context->enterscope();
  cgen_context->addid(identifier, new FunctionContext(-stack_frame, FP));
  // push init value under frame point let space
  emit_store(ACC, -stack_frame++, FP, s);  // instead of  `emit_push(ACC, s);`
  // evaluate body
  body->code(s);
  cgen_context->exitscope();
  // pop identifier
  // no need to pop since all let stored in stack local  //`emit_addiu(SP, SP, 4, s);`
  stack_frame--;
}

static void emit_arith(char op, Expression e1, Expression e2, ostream &s) {
  // this step generate a new copyed object, stored in ACC
  e1->code(s);
  emit_load(T1, 3, ACC, s);
  emit_push(T1, s);
  e2->code(s);
  // store sum to a new int object
  emit_jal("Object.copy", s);
  emit_pop(T1, s);
  // use T3 reg instead of push/pop on stack
  emit_load(T2, 3, ACC, s);
  // add register and get sum
  if (op == '+') { emit_add(T1, T1, T2, s); }
  else if (op == '-') { emit_sub(T1, T1, T2, s); }
  else if (op == '*') { emit_mul(T1, T1, T2, s); }
  else if (op == '/') { emit_div(T1, T1, T2, s); }
  emit_store(T1, 3, ACC, s);
  // object address in acc
}

void plus_class::code(ostream &s) {
  char op = '+';
  if (!ARITH_MODE && get_depth() > 100) {
    ARITH_MODE = true;
    emit_arith(op, e1, e2, s);
    ARITH_MODE = false;
  } else if (ARITH_MODE) {
    emit_arith(op, e1, e2, s);
  } else {
    std::vector<REG> regs = {T1, T2};
    emit_push_stack_regs(regs, s);
    emit_arith(op, e1, e2, s);
    emit_pop_stack_regs(regs, s);
  }
}

// same process in add_class
void sub_class::code(ostream &s) {
  char op = '-';
  if (!ARITH_MODE && get_depth() > 100) {
    ARITH_MODE = true;
    emit_arith(op, e1, e2, s);
    ARITH_MODE = false;
  } else if (ARITH_MODE) {
    emit_arith(op, e1, e2, s);
  } else {
    std::vector<REG> regs = {T1, T2};
    emit_push_stack_regs(regs, s);
    emit_arith(op, e1, e2, s);
    emit_pop_stack_regs(regs, s);
  }
}

void mul_class::code(ostream &s) {
  char op = '*';
  if (!ARITH_MODE && get_depth() > 100) {
    ARITH_MODE = true;
    emit_arith(op, e1, e2, s);
    ARITH_MODE = false;
  } else if (ARITH_MODE) {
    emit_arith(op, e1, e2, s);
  } else {
    std::vector<REG> regs = {T1, T2};
    emit_push_stack_regs(regs, s);
    emit_arith(op, e1, e2, s);
    emit_pop_stack_regs(regs, s);
  }
}

void divide_class::code(ostream &s) {
  char op = '/';
  if (!ARITH_MODE && get_depth() > 100) {
    ARITH_MODE = true;
    emit_arith(op, e1, e2, s);
    ARITH_MODE = false;
  } else if (ARITH_MODE) {
    emit_arith(op, e1, e2, s);
  } else {
    std::vector<REG> regs = {T1, T2};
    emit_push_stack_regs(regs, s);
    emit_arith(op, e1, e2, s);
    emit_pop_stack_regs(regs, s);
  }
}


void neg_class::code(ostream &s) {
  // reg to use
  std::vector<REG> regs = {T1, T2, T3};
  emit_push_stack_regs(regs, s);
  e1->code(s);
  emit_load(T1, 3, ACC, s);
  emit_push(T1, s);
  emit_jal("Object.copy", s);
  emit_pop(T1, s);
  // neg T1
  emit_neg(T1, T1, s);
  emit_store(T1, 3, ACC, s);
  emit_pop_stack_regs(regs, s);
}

void lt_class::code(ostream &s) {
  // reg to use
  std::vector<REG> regs = {T1, T2};
  emit_push_stack_regs(regs, s);
  // eval e1 and T1 get value
  e1->code(s);
  emit_load(T1, 3, ACC, s);
  // eval e2 and T2 get value
  e2->code(s);
  emit_load(T2, 3, ACC, s);
  // compare and assign 
  emit_load_address(ACC, CONST_TRUE, s);
  emit_blt(T1, T2, label_count, s);
  emit_load_address(ACC, CONST_FALSE, s);
  // label def do nothing
  emit_label_def(label_count++, s);
  emit_pop_stack_regs(regs, s);
}

void eq_class::code(ostream &s) {
  std::vector<REG> regs = {T1, T2};
  emit_push_stack_regs(regs, s);
  if (e1->get_type() == Int || e1->get_type() == Bool) {
    // compare int value if it's Integer or Bool
    e1->code(s);
    emit_load(T1, 3, ACC, s);
    e2->code(s);
    emit_load(T2, 3, ACC, s);
    emit_load_address(ACC, CONST_TRUE, s);
    // replace lt with eq
    emit_beq(T1, T2, label_count, s);
    emit_load_address(ACC, CONST_FALSE, s);  
  } else {
    e1->code(s);
    emit_move(T1, ACC, s);
    e2->code(s);
    emit_move(T2, ACC, s);
    emit_load_address(ACC, CONST_TRUE, s);
    emit_beq(T1, T2, label_count, s);
    emit_load_address(A1, CONST_FALSE, s);
    emit_jal("equality_test", s);
  }
  emit_label_def(label_count++, s);
  emit_pop_stack_regs(regs, s);
}

void leq_class::code(ostream &s) {
  std::vector<REG> regs = {T1, T2};
  emit_push_stack_regs(regs, s);
  e1->code(s);
  emit_load(T1, 3, ACC, s);
  e2->code(s);
  emit_load(T2, 3, ACC, s);
  emit_load_address(ACC, CONST_TRUE, s);
  // replace lt with leq
  emit_bleq(T1, T2, label_count, s);
  emit_load_address(ACC, CONST_FALSE, s);
  emit_label_def(label_count++, s);
  emit_pop_stack_regs(regs, s);
}

void comp_class::code(ostream &s) {
  std::vector<REG> regs = {T1};
  emit_push_stack_regs(regs, s);
  e1->code(s);
  emit_load(T1, 3, ACC, s); // T1 <- expr bool value
  emit_load_address(ACC, CONST_TRUE, s);
  emit_beqz(T1, label_count, s);
  emit_load_address(ACC, CONST_FALSE, s);
  emit_label_def(label_count++, s);
  emit_pop_stack_regs(regs, s);
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  // find class on class_nameTab
  if (type_name == SELF_TYPE) {
    // reg to use
    std::vector<REG> regs = {T1, T2};
    emit_push_stack_regs(regs, s);
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    emit_push(T1, s);
    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);
    // init object
    emit_pop(T1, s);
    emit_load(T1, 1, T1, s);
    emit_jal(T1, s);
    emit_pop_stack_regs(regs, s);
  } else {
    std::string protObj = std::string(type_name->get_string()) + PROTOBJ_SUFFIX;
    std::string init = std::string(type_name->get_string()) + CLASSINIT_SUFFIX ;
    emit_load_address(ACC, str2charp(protObj), s);
    emit_jal("Object.copy", s);
    emit_jal(str2charp(init), s);
  }
}

void isvoid_class::code(ostream &s) {
  // reg to use
  std::vector<REG> regs = {T1};
  emit_push_stack_regs(regs, s);
  // code expression
  e1->code(s);
  // judge voidness
  emit_move(T1, ACC, s);
  emit_load_address(ACC, CONST_TRUE, s);
  emit_beqz(T1, label_count, s);
  emit_load_address(ACC, CONST_FALSE, s);
  // label def just exit
  emit_label_def(label_count++, s);
  emit_pop_stack_regs(regs, s);
}

void no_expr_class::code(ostream &s) {
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s) {
  if (name == self) {
    emit_move(ACC, SELF, s);
  } else {
    auto rst = cgen_context->lookup(name);
    rst->load_symbol(ACC, s);
  }
}

///////////////////////////////////////////////////////////////////
///					Get tmp vairable numb
///////////////////////////////////////////////////////////////////
int assign_class::get_let() { return expr->get_let(); }
int static_dispatch_class::get_let() { 
  int max = expr->get_let(); 
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    int tmp = actual->nth(i)->get_let();
    max = max > tmp ? max : tmp;
  }
  return max;
}
int dispatch_class::get_let() { 
  int max = expr->get_let(); 
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    int tmp = actual->nth(i)->get_let();
    max = max > tmp ? max : tmp;
  }
  return max;
}
int cond_class::get_let() {
  int max = pred->get_let();
  int tmp = then_exp->get_let();
  max = max > tmp ? max : tmp;
  tmp = else_exp->get_let();
  max = max > tmp ? max : tmp;
  return max;
}
int loop_class::get_let() {
  int max = pred->get_let();
  int tmp = body->get_let();
  max = max > tmp ? max : tmp;
  return max;
}
int typcase_class::get_let() {
  int max = expr->get_let();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    int tmp = cases->nth(i)->get_let();
    max = max > tmp ? max : tmp;
  }
  return max;
}
int branch_class::get_let() { return expr->get_let(); }
int block_class::get_let() {
  int max = 0;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    int tmp = body->nth(i)->get_let();
    max = max > tmp ? max : tmp;
  }
  return max;
}
int let_class::get_let() {
  int initial = init->get_let();
  int let_body = body->get_let() + 1;
  return initial > let_body ? initial : let_body;
}
int plus_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int sub_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int mul_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int divide_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int neg_class::get_let() { return e1->get_let(); }
int lt_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int eq_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int leq_class::get_let() {
  int left = e1->get_let();
  int right = e2->get_let() + 1;
  return left > right? left : right;
}
int comp_class::get_let() { return e1->get_let(); }
int int_const_class::get_let() { return 0; }
int string_const_class::get_let() { return 0; }
int bool_const_class::get_let() { return 0; }
int new__class::get_let() { return 0; }
int isvoid_class::get_let() { return e1->get_let(); }
int no_expr_class::get_let() { return 0; }
int object_class::get_let() { return 0; }

// register arith operation optimization
int plus_class::get_depth() {return e1->get_depth() + e2->get_depth() + 1;}
int sub_class::get_depth() {return e1->get_depth() + e2->get_depth() + 1;}
int mul_class::get_depth() {return e1->get_depth() + e2->get_depth() + 1;}
int divide_class::get_depth() {return e1->get_depth() + e2->get_depth() + 1;}
int assign_class::get_depth() { return 0; }
int static_dispatch_class::get_depth() { return 0; }
int dispatch_class::get_depth() { return 0; }
int cond_class::get_depth() { return 0; }
int loop_class::get_depth() { return 0; }
int typcase_class::get_depth() { return 0; }
int block_class::get_depth() { return 0; }
int let_class::get_depth() { return 0; }
int neg_class::get_depth() { return 0; }
int lt_class::get_depth() { return 0; }
int eq_class::get_depth() { return 0; }
int leq_class::get_depth() { return 0; }
int comp_class::get_depth() { return 0; }
int int_const_class::get_depth() { return 0; }
int string_const_class::get_depth() { return 0; }
int bool_const_class::get_depth() { return 0; }
int new__class::get_depth() { return 0; }
int isvoid_class::get_depth() { return 0; }
int no_expr_class::get_depth() { return 0; }
int object_class::get_depth() { return 0; }

///////////////////////////////////////////////////////////////////
///					some kit function
///////////////////////////////////////////////////////////////////
bool compareClassname(std::pair<int, Symbol> p1, std::pair<int, Symbol> p2) {
  return (p1.first < p2.first);
}

bool compareStringEntryP(std::pair<CgenNodeP, StringEntryP> p1, std::pair<CgenNodeP, StringEntryP> p2) {
  return (p1.first->classtag < p2.first->classtag);
}

bool compareBranch(branch_class* b1, branch_class* b2) {
  return (nodes_map[b1->type_decl]->classtag > nodes_map[b2->type_decl]->classtag);
}

static char* str2charp(std::string str) {
  return &str[0];
}

void FunctionContext::load_symbol(REG reg, ostream & s) {
	emit_load(reg, offset, from_reg, s);
}

void FunctionContext::store_symbol(REG to_reg, ostream &s) {
	emit_store(to_reg, offset, from_reg, s);
}

void ObjectContext::load_symbol(REG reg, ostream & s) {
  emit_load(reg, offset, SELF, s);
}

void ObjectContext::store_symbol(REG reg, ostream & s) {
	emit_store(reg, offset, SELF, s);
}

void RegisterContext::load_symbol(REG to_reg, ostream & s) {
  emit_move(to_reg, reg, s);
}

// never used
void RegisterContext::store_symbol(REG reg, ostream & s) {
	emit_move(ACC, reg, s);
}

DispatchTab::DispatchTab(const DispatchTab& dpt) {
  this->all_method_names = dpt.all_method_names;
  this->attrs = dpt.attrs;
  this->scope = dpt.scope;
  this->attr_start = this->attrs.size();
  this->scope.enterscope();
};

int DispatchTab::get_method_pos_by_name(Symbol name) {
  int pos = 0;
  std::string name_str = std::string(name->get_string());
  for (auto it = all_method_names.begin(); it != all_method_names.end(); it++, pos++) {
    if ((*it).second == name_str) return pos;
  }
  return -1;
}

