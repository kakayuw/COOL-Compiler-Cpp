
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
class Test {
   otherclass: C <- (new C).initc(1);
   aa : IO;
   str: String <- newstr();
   k: Int;
   newstr() : String {"another string"};
};

class C inherits IO {
   k: Int;
   oo: Int <- 0;
   c: Int <- 2-4;
   initc(i: Int) : SELF_TYPE {self};
   getint() : Int {3};
};

class D inherits C {
   d: Int <- 12 + c * c;
   e: Bool <- false;
   getd(a:Int, b:Bool, c:Int):Int {d};
   d_static_dispatch():Object{
   	self@C.initc(1)
   };
   getint() : Int {4};
};

class E {};

class Learn inherits IO {
   b: Bool;
   o: Int <- ~666;
   learn_new() : Object{
	new Main
   };
   learn_return_self()  : SELF_TYPE {
      	self
   };
   learn_parmeter(i: Int, bl: Bool, f: IO): Object {
   	{i; bl; f;}
   };
   learn_neg(): Int {~12};
   learn_isvoid(): Bool {isvoid 123};
   learn_cond(): Object {
	if (b) then 12
	else 21 fi
   };
   learn_comapre() : Bool {
   	1 < 2
   };
   learn_not() : Bool {
   	not b
   };
   learn_assign() : Object {{
	out_string("o before assign:");
	out_int(o);
	out_string("\n");
	o <- 321;
	out_string("o after assign:");
	out_int(o);
	out_string("\n");
   }};
   learn_loop() : Object {
   	while (not b)
	loop	{b <- not b; out_string("loop here!\n");}
	pool
   };
   learn_let(o: Int): Object{{
(*	out_string("o before let:");
	out_int(o);
	out_string("\n");
	let o: Int <- o * o in {
	   out_string("o in let:");
	   out_int(o);
	   out_string("\n");
	   let o: Int <- ~o in {
		out_string("o in nested let:");
		out_int(o);
		out_string("\n");
	   };
	   out_string("o after nexted let:");
	   out_int(o);
	   out_string("\n");
	};
	out_string("o after let");
	out_int(o);*)
	out_string("\n");
   }};
   learn_let_simple():Object {
	let o :Int <- 144 in o/2
   };
   learn_success(x: Int):Object {{
   	out_string("learn success: ");
	out_int(x);
	out_string("\n");
   }};
   learn_case(o:Int):Object {{
      	(*case (new Test) of 
	f : Learn => out_string("(new D) is Learn type.\n");
	f : C => out_string("(new D) is C type.\n");
	f : D => out_string("(new D) is D type.\n");
	f : Main => out_string("(new D) is Main type.\n");
	esac;*)
      	case (new E) of 
	f : Learn => f;
	f : C => f;
	f : D => f;
	f : Main => f;
	esac;
   }};
   learn_static_dispatch() : Object{
   	(new D)@C.initc(1)
   };
};

class Main  inherits D{
  learn: Learn;
  main(): Object {
   	if ( (new D).getd(1, false, 1) < (new D).getd(1, true, 2) - 2) then out_string("no wrong!\n")
	else {
		out_int(12312); 
		out_string("\n yes tru! \n");
		learn <- (new Learn);
		learn.learn_assign();
		learn.learn_loop();
		learn.learn_let(19);
		learn.learn_case(d);
		out_int((new D)@C.getint());
	} fi
  };
};

