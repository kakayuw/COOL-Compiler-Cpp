class C inherits D {
	self : Int;
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- "wrong type";
		b <- y;
		self;
           }
	};
	e() : C {self};
};

class D inherits C {
	a : Int <- 6;
	badassign : Int <- false;
	a(x : Int) : D {
	   self
	};
	invalid_case_type() : SELF_TYPE {
	   case self of 
	   	d : C => out_string("type C");
		d : D => out_string("type D");
		undefined : X => out_string("und type");
	   esac
	};
	(* override method *)
	init(x : Int) : C {
	   self
	};
	strcompare() : Bool {
	   "abc" < "cde"
	};
	(* change return type of overriden method *)
	e() : D {self};
};

class E inherits IO {
	let_selftype() : SELF_TYPE {
	   (let c : SELF_TYPE <- out_string("let_selftype") in
		self
	   )
	};
	(* change return type of overriden method *)
};

class SubMain inherits Main {
	a : Int <- 7;
	test_case() : SELF_TYPE {
		case a of 
			c : C => out_string("c");
			d : D => out_string("d");
			dup_d : D => out_string("d duplicate");
			not_defined: Z => out_string("case not defined");
		esac
	};
};

class SuperMain {
	main() : SELF_TYPE {self};
};

Class Main {
	maain():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
    	  out_int(1);
	 }
	};
};
