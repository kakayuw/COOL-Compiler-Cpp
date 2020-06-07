
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;

(* error: missing semicolon in method definition *)
Class F inherits A {
    (* error: missing semicolon in method definition *)
    method1(para1: String): SELF_TYPE {self}
    (* no error *)
    method2(para1: String): SELF_TYPE {self};
    (* no error *)
    method3(para1: Int): Int {1};
    (* no error *)
    attri : String;
    (* error: no type named 'string' *)
    attri2 : string;
    (* no error *)
    attri3: Int;
}

(* correct class *)
Class G inherits A {
    method1(para1: String): SELF_TYPE {"TEST"};
};

Class H inherits A {
    (* error: typo in parameter type *)
    method1(para1: String, para2: int): SELF_TYPE {"TEST"};
    (* correct attribute *)
    correct1 : Int <- 1;
    (* error: typo in parameter type *)
    method1(para1: String, para2: Int): SELF_TYPE {
        (* error: z is not an object id *)
        while not isvoid z loop {
            case self of
                    n : A => (new A);
                    (* error: BB_ is not an type id *)
                    n : BB_ => (new BB_);
                    (* error: definition of Class C contains error *)
                    n : C  => (new C);
            esac;
        } pool
    };
};

Class G inherits A {
    (* correct attributes *)
    correct1 : Int <- 1;
    correct2 : Int <- 2;
    correct3 : Int <- 3;
    method1(para1: String, para2: Int): SELF_TYPE {
        {
            correct1 <- correct2 + correct3;
            (* error: correct4 is not an object id, check the next error could be detected in this block *)
            correct2 <- correct4 - correct1;
            correct3 <- neg correct1 / (correct1 + correct2);
            (* error: typo of 'self' *)
            correct1 <- Self;
            (* error: missing semicolon *)
            correct2 <- self;
        }
    };
    method2(para1: String): SELF_TYPE {
        (* errors in underlying nested let statement *)
        let x : Int <- let y : Int <- ley z: Int <- let zz : Int <- 0 in z+error in 123 in z/x in y + 1 in out_int(x + 1)
    };
    (* error: invalid syntax *)
    method3() : A{
        correct1 ++
    };
    -- method with no expression in it
    method4() : A{};
};
