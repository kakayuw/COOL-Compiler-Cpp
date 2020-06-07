
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

(* correct class *)
Class G inherits A {
    method1(para1: String): SELF_TYPE {"TEST"};
};

Class H inherits A {
    a2i_aux(s : String) : Int {
    (let int : Int <- 0 in  
            
            (let j : Int <- s.length() in
                (let i : Int <- 0 in
                while i < j loop
                    {
                        int <- int * 10 + c2i(s.substr(i,1));
                        i <- i + 1;
                    }
                pool
                )
            )
        
    )
    };
};
