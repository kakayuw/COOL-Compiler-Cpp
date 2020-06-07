class E inherits J {
	f : Int;
};

class D inherits C {
	g(x: C) : SELF_TYPE {
	{
		x <- (new C);
		self;
	}	
	};
	c(x : Int, y : Bool) : SELF_TYPE {
		self
	};
	d() : SELF_TYPE {
		(new D).c(1, false)
	};
};
class C inherits E{
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
	c : Bool;
	c(x : Int, y : Bool) : SELF_TYPE {
	   self
 	};
	
};



class Testformal inherits IO {
	formal_dup(x: Int, y: Bool) : SELF_TYPE {
		self
	};
	method_return_type_no_exist() : SELF_TYPE {self};
	self_in_formal(x: Int) : SELF_TYPE {self};
	return_type_not_conform(): SELF_TYPE {
		self
	};
	selif_in_formal(x: Int, b: Bool): SELF_TYPE {self};
};

class SuperMain  inherits IO {
	var: Int;
	ori_main() : C {
	{
	  (new C).init(1,true);
	  (new C).c(false, true);
	}
	};
	out_string(outs: String) : SELF_TYPE {
		self
	};
	main() : SELF_TYPE {
		case var of
		   c : C => out_string("c");
		   d : D => out_string("d");
		   e : E => out_string("e");
		esac
	};
};

class SubSuperMain inherits SuperMain {
	test_main(): SELF_TYPE {
		out_string("parent error")
	};
};

class Let_test inherits IO {
	let_invalid_type() : Int {
		(let x: Int in 0)
	};
	let_bad_init() : Int {
		(let x: Int <- 1 in 0)
	};
};

class Test_attribute inherits IO {
	a : Int;
};

class Test_expr {
	test_assign(a: Int) : Int {
	{
		a <- 1;
		a;
	}
	};
};

Class Main inherits SuperMain{
	x : SELF_TYPE <- self;
	main() : SELF_TYPE {self};
};
