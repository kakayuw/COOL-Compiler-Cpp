class A {
    initA(aa : String) : String {
        aa
    };
    stringConst(ab: Int): String {
        "string"
    };
    intConst(ac: Int): Int {
        ac
    };
    boolConst(ad: Bool): Bool {
        ad
    };
    -- test multiple paramters
    mulPara(ae: String, af: Int, ag: Boolean): String {
        "multiple parameters"
    };
    -- test self_type
    self(ah: String): SELF_TYPE {
        self
    };
};

class C inherits A {
    x : Int;
    method1(num : Int) : A {
        -- test block
        {
            -- test assign and neg
            x <- ~num;
            -- test + - * /
            x <- x + (x - x) * x / x;
        }
    };
};

--  test class finition
Class BB__ inherits A {
    -- test attributes
    attri : String;
    attri2 : String;
    -- test featrue list (method)
    main() : Object {
        -- test let statment
        {
        (let z : BB__ <- new A in
            while true loop  
                -- test if then else
                if s = "stop" then 
                    -- test while loop & not & isvoid
                    while not isvoid z loop {
                        -- test Case branches
                        case self of
                                n : A => (new A);
                                n : BB_ => (new BB_);
                                n : C  => (new C);
                        esac;
                    } pool
                else
                    -- test parenthethis and dynamic dispatch
                    let testa : A <- (new A).initA("a") in (
                        testb <- (new C)@A.mulPara(testa.self().self().self().stringConst(testa.intConst()), testa.intConst("123"), true)
                    )
                fi
            pool
        );
        -- test nested let statement
        let x : Int <- let y : Int <- 5 in y + 1 in out_int(x + 1);
        }
    };
};
