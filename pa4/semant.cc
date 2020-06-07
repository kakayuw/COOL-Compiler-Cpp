

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <set>      // std::set

extern int semant_debug;
extern char *curr_filename;
int curr_lineno;
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    // semantic analyse basic classes
    install_basic_classes();
    semant_basic_classes();
    // check redefinition and construct class_table
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class_c current = (class_c) classes->nth(i);
        Symbol classname = current->get_classname();
        if (class_tab.lookup(classname) != NULL) {
            ostream& os = semant_error(current);
            if (is_builtin_class(classname))    os << "Redefinition of basic class " << classname << "." << endl;
            else    os << "Class " << classname << " was previously defined." << endl;
        } else {
            class_tab.addid(classname, current);
        }
    }
}

void ClassTable::semant_analyse(Classes classes) {

    // first pass: check class definition 
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class_c current = (class_c) classes->nth(i);
        if (! semant(current)) return;
    }

    // check main before inherits
    if (!checkMain()) return;

    // check inheritance is acyclic
    std::stack<class_c> error_inh; 
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {    
        class_c current = (class_c) classes->nth(i);
        int result_code = check_cycle(current);
        if ( result_code == 1)   error_inh.push(current);
        else if (result_code == 2) return;
    }
    if (report_inheritance_error(error_inh)) return;

    // copy method and attribute from parents
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {    
        inherit_features( (class_c) classes->nth(i));
    }

    // second pass: check class attribute definition
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class_c current = (class_c) classes->nth(i);
        semant_deep(current);
    }
}

// check main validity
bool ClassTable::checkMain() {
    //check Main 
    class_c main_class = (class_c) class_tab.probe(Main);
    if ( main_class == NULL){
        semant_error() << "Class Main is not defined." << endl;
        return false;
    } else {
        method_ main_method = (method_) main_class->methodTable.probe(main_meth);
        // check if main method exists
        if ( main_method == NULL ){ 
            semant_error(main_class) << "No 'main' method in class Main." << endl;
            return false;
        } else {
            // check whether main method has formals
            if (main_method->get_formals()->len() > 0) {
                semant_error(main_class) << "'main' method in class Main should have no arguments." << endl;
                return false;
            }
        }
    }
    return true;
}

// check whey cycle exists in the path from current class to root
//      return 1 if cycle exist; 2 if parent no exist error ; 0 if all right
int ClassTable::check_cycle(class_c cur) {
    Symbol parent = cur->get_classname();
    std::set<Symbol> ancestors;
    while (parent != No_class) {
        class_c parent_class = (class_c) class_tab.lookup(parent);
        // check parent existence
        if (parent_class == NULL) {
            semant_error(cur) << "Class " << cur->get_classname() << " inherits from an undefined class " << parent << "." << endl;
            return 2;
        }
        if (ancestors.find(parent) == ancestors.end())  ancestors.insert(parent);
        else return 1;
        parent = parent_class->get_parentname();
    }
    return 0;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object, 
               No_class,
               append_Features(
                               append_Features(
                                               single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                               single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                               single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
               filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
        class_(Int, 
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
               filename);
    
    // Add these local variable into global scope table
    class_tab.addid(Object, Object_class);
    class_tab.addid(IO, IO_class);
    class_tab.addid(Int, Int_class);
    class_tab.addid(Bool, Bool_class);  
    class_tab.addid(Str, Str_class);  
}

// add base class methods and attributes into symbol table
void ClassTable::semant_basic_classes() {
    // object class
    class_c Object_class = (class_c) class_tab.lookup(Object);
    Object_class->methodTable.enterscope();
    Object_class->objTable.enterscope();
    inherit_features(Object_class);
    // IO class
    class_c IO_class = (class_c) class_tab.lookup(IO);
    IO_class->methodTable.enterscope();
    IO_class->objTable.enterscope();
    inherit_features(IO_class);
    // Int class
    class_c Int_class = (class_c) class_tab.lookup(Int);
    Int_class->methodTable.enterscope();
    Int_class->objTable.enterscope();
    inherit_features(Int_class);
    // Bool class
    class_c Bool_class = (class_c) class_tab.lookup(Bool);
    Bool_class->methodTable.enterscope();
    Bool_class->objTable.enterscope();
    inherit_features(Bool_class);
    // Str class
    class_c Str_class = (class_c) class_tab.lookup(Str);
    Str_class->methodTable.enterscope();
    Str_class->objTable.enterscope();
    inherit_features(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
//    All versions return an output stream to which you should write
//    an appropriate human-readable error message.
//
///////////////////////////////////////////////////////////////////
// alias for semant error
ostream& ClassTable::semant_error(Class_ c, tree_node *t)
{                                                             
    return semant_error(c->get_filename(),t);
}    

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

bool ClassTable::report_inheritance_error(std::stack<class_c>& reverse_errors) {
    bool has_error = !reverse_errors.empty();
    while (!reverse_errors.empty()) {
        class_c cur = reverse_errors.top();
        Symbol name = cur->get_classname();
        semant_error(cur) << "Class " << name << ", or an ancestor of " << name << ", is involved in an inheritance cycle." << endl;
        reverse_errors.pop();
    }
    return has_error;
}

////////////////////////////////////////////////////////////////////
///         SELF_DEFINED NODE ANALYSER                           ///
////////////////////////////////////////////////////////////////////

/// FIRST PASS COLLECTS CLASS NAME ///

// class first pass
bool ClassTable::semant(class_c cur) {
    cur->objTable.enterscope();
    cur->methodTable.enterscope();
    Symbol name = cur->get_classname();
    Symbol parent_name = cur->get_parentname();
    if (name == SELF_TYPE) {
        semant_error(cur) << "Redefinition of basic class SELF_TYPE." << endl;
        return false;
    }
    if (parent_name == SELF_TYPE) {
        semant_error(cur) << "Class " << cur->get_classname() << " cannot inherit class SELF_TYPE." << endl; 
        return false;
    } else  if (is_non_inherit_builtin_class(parent_name)) {
        semant_error(cur) << "Class " << cur->get_classname() << " cannot inherit class " << parent_name << "." << endl;
        return false;
    }
    Features feas = cur->get_features();
    for (int i = feas->first(); feas->more(i); i = feas->next(i)) {
        Feature fea = feas->nth(i);
        if (fea->is_method()) {
            semant(cur, (method_) fea);
        } else {
            semant(cur, (attr_) fea);
        }
    }
    // variableTable.exitscope();
    return true;
}


// method first pass: type check
void ClassTable::semant(class_c cur, method_ method) {
    Symbol name = method->get_methodname();
    Symbol return_type = method->get_returntype();
    symtab methodTable = cur->methodTable;
    if (class_tab.lookup(return_type) == NULL && return_type != SELF_TYPE) {
        ostream& os = semant_error(cur, method);
        os << "Undefined return type "<<return_type<<" in method "<<name<<"." << endl;
    }
    if (methodTable.lookup(name)) {
        ostream& os = semant_error(cur, method);
        os << "Method " << name << " is multiply defined." << endl;
    }
    methodTable.addid(name, method);
}

// attribute first pass: type check
void ClassTable::semant(class_c cur, attr_ attri) {
    Symbol name = attri->get_attrname();
    Symbol attrtype = attri->get_typedecl();
    vartab variableTable = cur->objTable;
    if (name == self) {
        ostream& os = semant_error(cur, attri);
        os << "'self' cannot be the name of an attribute." << endl;
        return;
    }
    if (class_tab.lookup(attrtype) == NULL && attrtype != SELF_TYPE) {
        ostream& os = semant_error(cur, attri);
        os << "Attribute " << name << " declared with undefined type '" << attrtype << "'." << endl;
        return;
    }
    if (variableTable.probe(name)) {
        ostream& os = semant_error(cur, attri);
        os << "Attribute " << name << " is multiply defined." << endl;
        return;
    }
    variableTable.addid(name, attrtype);
}



/// SECOND PASS DO EXPRESSION CHECKING
void ClassTable::semant_deep(class_c cur) {
    // semant features
    Features feas = cur->get_features();
    vartab varaibleTable = cur->objTable;
    for (int i = feas->first(); feas->more(i); i=feas->next(i)) {
        Feature fea = feas->nth(i);
        if (fea->is_method()) {
            semant_deep(cur, (method_) fea);
        } else {
            semant_deep(cur, (attr_) fea);
        }
    }

}

// inherits method names and attributes from ancestors
//      return true if cycle inhertance exists
void ClassTable::inherit_features(class_c cur) {
    vartab curVarTable = cur->objTable;
    symtab curMethTable = cur->methodTable;
    Symbol parent = cur->get_classname();   // set parent to classname because basic class need to append the features
    while (parent != No_class) {
        // get parent class
        class_c parent_class = (class_c) class_tab.lookup(parent);
        // inherit features
        Features feas = parent_class->get_features();
        for (int i = feas->first(); feas->more(i); i=feas->next(i)) {
            Feature fea = feas->nth(i);
            if (fea->is_method()) {
                Symbol methodname = ((method_) fea)->get_methodname();
                if (curMethTable.lookup(methodname) == NULL) curMethTable.addid(methodname, (method_) fea);
                // check overriden validity
                // else if (is_builtin_method(parent_class, methodname)) 
                    // semant_error(cur, (method_ (fea))) << "Builtin method " << methodname << " overwritten by " << cur->get_classname() << "!" << endl;
                else {
                    method_ overriden = (method_) fea;                     method_ new_method = (method_) curMethTable.lookup(methodname);
                    Formals old_formals = overriden->get_formals();        Formals new_formals = new_method->get_formals();
                    Symbol old_returntype = overriden->get_returntype();   Symbol new_returntype = new_method->get_returntype();
                    if (overriden->get_formals()->len() != new_method->get_formals()->len()) {
                        curMethTable.addid(methodname, overriden);
                        if (inherit_error_count == 0) {
                            inherit_error_count += 1;
                            semant_error(cur,  new_method) << "Incompatible number of formal parameters in redefined method " << methodname << "." << endl;
                        }
                    } else if (old_returntype != new_returntype) {
                        semant_error(cur,  new_method) << "In redefined method " << methodname << ", return type " << new_returntype << " is different from original return type " << old_returntype << "." << endl;
                    } else {
                        for( int i = old_formals->first(); old_formals->more(i); i = old_formals->next(i)) {
                            Symbol old_type = ((formal_) old_formals->nth(i))->get_type_decl();
                            Symbol new_type =  ((formal_) new_formals->nth(i))->get_type_decl();
                            if (  old_type != new_type ) {
                                semant_error(cur,  (method_ (fea))) << "In redefined method " << methodname << ", parameter type " << new_type << " is different from original type " << old_type << "." << endl;
                            }
                        }
                    }
                }
                // check attribute overriden and inherit
            } else {
                Symbol attrname = ((attr_) fea)->get_attrname();
                if (curVarTable.lookup(attrname) == NULL)
                    curVarTable.addid(attrname, ((attr_) fea)->get_typedecl());
                else if (cur->get_classname() != parent)
                    semant_error(cur, fetch_attr(cur, attrname)) << "Attribute " << attrname << " is an attribute of an inherited class." << endl;
            }
        }
        parent = parent_class->get_parentname();
    }
}


// method second pass: check formal and expression
void ClassTable::semant_deep(class_c cur, method_ method) {
    Formals formals = method->get_formals();
    vartab variableTable = cur->objTable;
    variableTable.enterscope();
    for( int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal formal = formals->nth(i);
        if (!semant_fml(cur, (formal_) formal))  return;
    }
    // check expression
    Symbol body_type = semant_exp(cur, method->get_expr());
    Symbol self_body_type = body_type;
    if (body_type == SELF_TYPE) body_type = cur->get_classname();
    // exit scope
    variableTable.exitscope();
    // check return type class 
    Symbol return_type = method->get_returntype();
    Symbol self_return_type = return_type;
    if (return_type == SELF_TYPE) return_type = cur->get_classname();
    // cout << " in method second check--- decl_type:" << return_type << " vs --real_Expr_type:" << body_type << endl;;
    // check if error happened in semant analyse
    if (errors() > 0)  return;
    if (!is_subclass(body_type, return_type) || (self_return_type == SELF_TYPE && self_body_type != SELF_TYPE)) {
        ostream& os = semant_error(cur, method);
        os << "Inferred return type " << body_type << " of method " << method->get_methodname()->get_string() << " does not conform to declared return type " << self_return_type << "." << endl;
    }
}

// attribute second pass: check declaration and expression
void ClassTable::semant_deep(class_c cur, attr_ attr) {
    Symbol name = attr->get_attrname();
    // check expression
    Expression exp = attr->get_initexpr();
    Symbol return_type = semant_exp(cur, exp);
    // check decalre type validity
    Symbol decl_type = attr->get_typedecl();
    if (!is_subclass(return_type, decl_type) && return_type != No_type ) {
        ostream& os = semant_error(cur, attr);
        os << "Inferred type " << return_type << " of initialization of attribute " << name << " does not conform to declared type " << decl_type << "." << endl;
    }
}

// formal second pass: type check
bool ClassTable::semant_fml(class_c cur, formal_ formal) {
    Symbol name = formal->get_formal_name();
    Symbol decl_type = formal->get_type_decl();
    vartab variableTable = cur->objTable;
    if (name == self) {
        ostream& os = semant_error(cur, formal);
        os << "'self' cannot be the name of a formal parameter." << endl;   
        return false;
    } 
     if (variableTable.probe(name)) {
        ostream& os = semant_error(cur, formal);
        os << "Formal parameter " << name << " is multiply defined." << endl; 
        return false;
    }
    if (decl_type == SELF_TYPE) {
        ostream& os = semant_error(cur, formal);
        os << "Formal parameter " << name << " cannot have type SELF_TYPE." << endl;   
        return false;
    }
    if (class_tab.lookup(decl_type) == NULL) {
        ostream& os = semant_error(cur, formal);
        os << "Formal " << name << " type '" << decl_type << "' is undefined." << endl;     
        return false;
    }
    variableTable.addid(name, decl_type);
    return true;
}

///////////////////////////////////////////////////////////////////////
// Some helping functions about inheritance graph
///////////////////////////////////////////////////////////////////////

// check subclass using scope table lookup
bool is_subclass(Symbol child, Symbol ancestor) {
    if (ancestor == Object || ancestor == child || child == No_type ) return true;
    if (ancestor == SELF_TYPE) return child == SELF_TYPE;
    if (child == SELF_TYPE) child = current_class->get_classname();
    // cout << " now child name :" << child << endl;
    for (class_c cls; child != No_class; child=cls->get_parentname()) {
        cls = (class_c) class_tab.lookup(child);
        // cout <<" comparing: " << ancestor << " and asding:" << child << endl;
        if (child == ancestor) return true;
    }
    return false;
}

// find least common ancestor (least upper bound)
Symbol lca(Symbol a, Symbol b) {
    if (a == SELF_TYPE && b == SELF_TYPE) return SELF_TYPE;
    else if (a == SELF_TYPE) a = current_class->get_classname();
    else if (b == SELF_TYPE) b = current_class->get_classname();
    
    if (is_subclass(a, b) || b == No_type) {
        return b;
    } else if (is_subclass(b, a) || a == No_type) {
        return a;
    } else {
        Symbol parent = a;
        while (parent != No_class ) {
            parent = ((class_c) class_tab.lookup(parent))->get_parentname();
            if (is_subclass(b, parent)) {
                return parent;
            }
        }
        return Object;
    }
}

// check whether a method is builtin method given referenced class
bool is_builtin_method(class_c builtin_class, Symbol method) {
    if (method == cool_abort || method == type_name || method == copy) {
        return true;
    } else if (builtin_class->get_classname() == IO) {
        return method == out_string || method == out_int || method == in_string || method == in_int;
    } else if (builtin_class->get_classname() == Str) {
        return method == length || method == concat || method == substr;
    } else 
        return false;
}

// check whether given class is a builtin class that cannot be inherited
bool is_non_inherit_builtin_class(Symbol builtin) {
    if (builtin == Bool || builtin == Int || builtin == Str) return true;
    else return false;
}

// check whether a classname is name of basic class
bool is_builtin_class(Symbol builtin) {
    return builtin == Bool || builtin == Int || builtin == Str || builtin == IO || builtin == Object;
}

// get ancient builtin
Symbol get_ancient_builtin(class_c cur) {
    Symbol parent = cur->get_classname();
    while (!is_builtin_class(parent)) {
        class_c parent_class = (class_c) class_tab.lookup(parent);
        parent = parent_class->get_parentname();
    }
    return parent;
}

// fetch attribute from given class and given attribute name
attr_ fetch_attr(class_c cur, Symbol attrname) {
    Features features = cur->get_features();
    for (int i = features->first(); features->more(i); i=features->next(i)) {
        Feature fea = features->nth(i);
        if (!fea->is_method()){
            attr_ attribute = (attr_) fea;
            if (attribute->get_attrname() == attrname) return attribute;
        }
    }
    return NULL;
}


///////////////////////////////////////////////////////////////////////
// check expression validity
///////////////////////////////////////////////////////////////////////

Symbol semant_expr_error(const char* msg, tree_node *node) {
    sc->semant_error(current_class->get_filename(), node) << msg << endl;
    return Object;
}

Symbol ClassTable::semant_exp(class_c cur, Expression exp) {
    current_class = cur;
    return exp->semant_expression();    
}

// assign
Symbol assign_class::semant_expression() {
    vartab variableTable = current_class->objTable;
    Symbol decl_type = (Symbol) variableTable.lookup(name);
    if (name == self) {
       return semant_expr_error("Cannot assign to 'self'.", this);
    } else if (decl_type == NULL) {
        std::string msg = "Assignment to undeclared variable "+std::string(name->get_string())+".";
       return semant_expr_error(msg.c_str(), this);
    } else {
        Symbol expr_type = expr->semant_expression();
        if (is_subclass(expr_type, decl_type)) {
            type = expr_type;
            return expr_type;
        } else {
            std::string msg = "Type "+std::string(expr_type->get_string())+" of assigned expression does not conform to declared type "+std::string(decl_type->get_string())+" of identifier "+std::string(name->get_string())+".";
            semant_expr_error(msg.c_str(), this);
            return decl_type; 
        }
    }
    return Str;
}

// plus
Symbol plus_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    if (e1_type == Int && e2_type == Int) {
        type = Int;
        return Int;
    } else {
        std::string msg = "non-Int arguments: "+std::string(e1_type->get_string())+" + "+std::string(e2_type->get_string());
        semant_expr_error(msg.c_str(), this);
        return Int;
    }
}

// sub
Symbol sub_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    if (e1_type == Int && e2_type == Int) {
        type = Int;
        return Int;
    } else {
        semant_expr_error( "Non-Int arguments in sub_class.", this);
        return Int;
    }
}

// mul
Symbol mul_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    if (e1_type == Int && e2_type == Int) {
        type = Int;
        return Int;
    } else {
        semant_expr_error("Non-Int arguments: in mul_class.", this);
        return Int;
    }
}

// divide
Symbol divide_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    if (e1_type == Int && e2_type == Int) {
        type = Int;
        return Int;
    } else {
        semant_expr_error("Non-Int arguments in divide_class.", this);
        return Int;
    }
}

// neg
Symbol neg_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    if (e1_type == Int) {
        type = Int;
        return Int;
    } else {
        semant_expr_error("Argument of '~' has type Bool instead of Int.", this);
        return Int;
    }
}

// less than
Symbol lt_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    if (e1_type == Int && e2_type == Int) {
        type = Bool;
        return Bool;
    } else {
        std::string msg = "non-Int arguments: "+std::string(e1_type->get_string())+" < "+std::string(e2_type->get_string());
        semant_expr_error(msg.c_str(), this);
        return Bool;
    }
}

// equal
Symbol eq_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    e1_type = (e1_type == SELF_TYPE)? current_class->get_classname(): e1_type;
    e2_type = (e2_type == SELF_TYPE)? current_class->get_classname(): e2_type;
    if (e1_type == e2_type) {
        type = Bool;
        return Bool;
    } else {
        semant_expr_error("Illegal comparison with a basic type.", this);
        return Bool;
    }
}

// less than or equal 
Symbol leq_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    Symbol e2_type = e2->semant_expression();
    if (e1_type == Int && e2_type == Int) {
        type = Bool;
        return Bool;
    } else {
        std::string msg = "non-Int arguments: "+std::string(e1_type->get_string())+" <= "+std::string(e2_type->get_string());
        semant_expr_error(msg.c_str(), this);
        return Bool;
    }
}

// not
Symbol comp_class::semant_expression() {
    Symbol e1_type = e1->semant_expression();
    if (e1_type == Bool) {
        type = Bool;
        return Bool;
    } else {
        semant_expr_error("Argument of 'not' has type Int instead of Bool.", this);
        return Bool;
    }
}

// const int
Symbol int_const_class::semant_expression() {
    type = Int;
    return Int;
}

// const bool
Symbol bool_const_class::semant_expression() {
    type = Bool;
    return Bool;
}

// const string
Symbol string_const_class::semant_expression() {
    type = Str;
    return Str;
}

// new
Symbol new__class::semant_expression() {
    type = type_name;
    return type;
}

// isvoid
Symbol isvoid_class::semant_expression() {
    e1->semant_expression();
    type = Bool;
    return Bool;
}

// no_expr
Symbol no_expr_class::semant_expression() {
    type = No_type;
    return No_type;
}

// object
Symbol object_class::semant_expression() {
    if (name == self) {
        type = SELF_TYPE;
        return type;
    }
    vartab variableTable = current_class->objTable;
    Symbol obj_type = variableTable.lookup(name);
    if (obj_type == NULL) {
        std::string msg = "Undeclared identifier " + std::string(name->get_string()) + ".";
        return semant_expr_error(msg.c_str(), this);
    } else  {
        type = obj_type;
        return obj_type;
    }
}

// let
Symbol let_class::semant_expression() {
    vartab variableTable = current_class->objTable;
    if (identifier == self) semant_expr_error("'self' cannot be bound in a 'let' expression.", this);
    Symbol init_type = init->semant_expression();
    variableTable.enterscope();
    if (class_tab.lookup(type_decl) == NULL && type_decl != SELF_TYPE){
        std::string msg = "Class "+std::string(type_decl->get_string())+" of let-bound identifier "+std::string(identifier->get_string())+" is undefined.";
        return semant_expr_error(msg.c_str(), this);
    }
    if (!is_subclass(init_type, type_decl)) {
        // cout << " let init type:" << init_type << " -- let type_decl:" << type_decl << endl;
        std::string msg = "Inferred type "+std::string(init_type->get_string())+" of initialization of "+std::string(identifier->get_string())+" does not conform to identifier's declared type "+std::string(type_decl->get_string())+".";
        return semant_expr_error(msg.c_str(), this);
    }
    
    variableTable.addid(identifier, type_decl);
    Symbol body_type = body->semant_expression();
    variableTable.exitscope();
    type = body_type;
    return body_type;
}

// loop
Symbol loop_class::semant_expression() {
    Symbol cond = pred->semant_expression();
    if (cond != Bool) {
        return semant_expr_error("Loop condition does not have type Bool.", this);
    }
    body->semant_expression();
    type = Object;
    return Object;
}

// if then else
Symbol cond_class::semant_expression() {
    Symbol cond_type = pred->semant_expression();
    if (cond_type != Bool) {
        return semant_expr_error("Predicate of 'if' does not have type Bool.", this);
    }
    Symbol then_type = then_exp->semant_expression();
    Symbol else_type = else_exp->semant_expression();
    Symbol lub = lca(then_type, else_type);
    type = lub;
    return lub;
}

// cases
Symbol typcase_class::semant_expression() {
    Symbol expr_type = expr->semant_expression();
    vartab variableTable = current_class->objTable;
    Symbol lub = NULL;
    caseTable.enterscope();
    for( int i = cases->first(); cases->more(i); i = cases->next(i)) {
       variableTable.enterscope(); 
       branch_ branch = (branch_) cases->nth(i);
       Symbol case_decl_type = branch->get_type_decl();
       if (case_decl_type == SELF_TYPE) {
           std::string msg = "Identifier "+std::string(branch->get_name()->get_string())+" declared with type SELF_TYPE in case branch.";
           return semant_expr_error(msg.c_str(), branch);
       } else if (class_tab.lookup(case_decl_type) == NULL) {
           std::string msg = "Class "+std::string(case_decl_type->get_string())+" of case branch is undefined.";
           return semant_expr_error(msg.c_str(), branch);  
       }
       Symbol case_expr_type = branch->semant_expression();
       if (caseTable.lookup(branch->get_type_decl())) {
           std::string msg = "Duplicate branch "+std::string(branch->get_type_decl()->get_string())+" in case statement.";
           return semant_expr_error(msg.c_str(), branch);
       }
       caseTable.addid(case_decl_type, branch->get_name());
       // consider branch return type is irrelative to case declared type
    //    if (!is_subclass(case_expr_type, case_decl_type)) {
    //        cout << "-----case expr type:" << case_expr_type << " case decl:" << case_decl_type << " in " << branch->get_name() << " try current:" << current_class->get_classname() << endl;
    //        sc->semant_error(current_class) << "Case expression type does not conform to declared type.";
    //    }
       lub = (lub == NULL) ? case_expr_type : lca(lub, case_expr_type);
       variableTable.exitscope();
    }
    caseTable.exitscope();
    type = lub;
    return lub;
}

// case
Symbol branch_class::semant_expression() {
    vartab variableTable = current_class->objTable;
    variableTable.addid(name, type_decl);
    return expr->semant_expression();
}

// block
Symbol block_class::semant_expression() {
    Symbol last = NULL;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        Expression clause = body->nth(i);
        last = clause->semant_expression();
    }
    type = (last == NULL) ? No_type : last;
    return type;
}

// dispatch
Symbol dispatch_class::semant_expression() {
    Symbol expr_class_name = expr->semant_expression();
    Symbol not_self_class = expr_class_name;
    expr_class_name = (expr_class_name == SELF_TYPE)? current_class->get_classname() : expr_class_name;
    class_c expr_class = (class_c) class_tab.lookup(expr_class_name);
    if (expr_class == NULL) return semant_expr_error("Class doen not have this method.", this);
    // check method validity
    symtab methodTable = expr_class->methodTable;
    method_ method = (method_) methodTable.lookup(name);
    if (method == NULL) {
        // cout << " ----------method :" << name << " expr:" << expr_class_name << endl;
        std::string msg = "Dispatch to undefined method "+std::string(name->get_string())+".";
        return semant_expr_error(msg.c_str(), this);
    }
    // check formal validity
    Formals formals = method->get_formals();
    Symbol method_return_type = method->get_returntype();
    if (method_return_type == SELF_TYPE && not_self_class != SELF_TYPE) {
        method_return_type = expr_class_name;
    }
    if (actual->len() != formals->len()) {
        std::string msg = "Method "+std::string(name->get_string())+" called with wrong number of arguments.";
        return semant_expr_error(msg.c_str(), this);
    }
    for( int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol act_para = actual->nth(i)->semant_expression();
        Symbol decl_para = ((formal_) formals->nth(i))->get_type_decl();
        if (!is_subclass(act_para, decl_para)) {
            std::string error_msg = "In call of method " + std::string(name->get_string()) + ", type "+std::string(act_para->get_string())+" of parameter "+std::string(((formal_) formals->nth(i))->get_formal_name()->get_string())+" does not conform to declared type "+std::string(decl_para->get_string())+".";
            return semant_expr_error(error_msg.c_str(), this);
        }
    }
    type = method_return_type;
    return method_return_type;
}


// static dispatch
Symbol static_dispatch_class::semant_expression() {
    Symbol expr_class_name = expr->semant_expression();
    Symbol not_self_class = expr_class_name;
    expr_class_name = (expr_class_name == SELF_TYPE)? current_class->get_classname() : expr_class_name;
    if (!is_subclass(expr_class_name, type_name)) {
        std::string msg = "Expression type "+std::string(not_self_class->get_string())+" does not conform to declared static dispatch type "+std::string(type_name->get_string())+".";
        return semant_expr_error(msg.c_str(), this);
    }
    class_c expr_class = (class_c) class_tab.lookup(expr_class_name);
    if (expr_class == NULL) return semant_expr_error("Class doen not have this method.", this);
    symtab methodTable = expr_class->methodTable;
    method_ method = (method_) methodTable.lookup(name);
    if (method == NULL){
        std::string msg = "Dispatch to undefined method "+std::string(name->get_string())+".";
        return semant_expr_error(msg.c_str(), this);
    } 
    Formals formals = method->get_formals();
    Symbol method_return_type = method->get_returntype();
    if (method_return_type == SELF_TYPE && not_self_class != SELF_TYPE) {
        method_return_type = expr_class_name;
    }
    if (actual->len() != formals->len()) {
        std::string msg = "Method "+std::string(name->get_string())+" called with wrong number of arguments.";
        return semant_expr_error(msg.c_str(), this);
    }
    for( int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol act_para = actual->nth(i)->semant_expression();
        Symbol decl_para = ((formal_) formals->nth(i))->get_type_decl();
        if (!is_subclass(act_para, decl_para)) {
            std::string error_msg = "In call of method " + std::string(name->get_string()) + ", type "+std::string(act_para->get_string())+" of parameter "+std::string(((formal_) formals->nth(i))->get_formal_name()->get_string())+" does not conform to declared type "+std::string(decl_para->get_string())+".";
            return semant_expr_error(error_msg.c_str(), this);
        }
    }
    type = method_return_type;
    return method_return_type;
}


///////////////////////////////////////////////////////////////////////

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();
    class_tab.enterscope();
    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    sc = classtable;

    /* some semantic analysis code may go here */
    classtable->semant_analyse(classes);
    class_tab.exitscope();
    // raise errors if sematic analysis encounter errors
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}


