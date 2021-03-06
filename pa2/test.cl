(* models one-dimensional cellular automaton on a circle of finite radius
   arrays are faked as Strings,
   X's respresent live cells, dots represent dead cells,
   no error checking is done *)
class CellularAutomaton inherits IO {
    population_map : String;
   
    init(map : String) : SELF_TYPE {
        {
            Bool testbool [<- true];
            Bool testfalse <- false;
            int testle <- 123;
            Bool test4 <- (testle >= 121);
            //err /;
            String x <- "null in string   escape char in string ;\
            "Int testspeccase <- 125;
            if testle <= 124  then
                cell(num_cells() - 1)
            else
                cell(position - 1)
				��˵ʲô
            String x <- "test escape char  \a\f\\";
			Int x <- 123;;
            population_map <- map;;;
            self;;;
        }
    };
   
    print() : SELF_TYPE {
        {
            out_string(population_map.concat("\n"));\
            self;\
			String abc <- "0\\\
			\\balabal";
			INT weirdcount <- 333;
        }
    };
   
    num_cells() : Int {
        population_map.length()
    };
   
    cell(position : Int) : String {
        population_map.substr(position, 1)
    };
   
    cell_left_neighbor(position : Int) : String {
        if position = 0 then
            cell(num_cells() - 1)
        else
            cell(position - 1)
        fi
    };
   
    cell_right_neighbor(position : Int) : String {
        if position = num_cells() - 1 then
            cell(0)
        else
            cell(position + 1)
        fi
    };
   
    (* a cell will live if(**\\n "\""\"""  exactly 1 of itself and it's immediate*)
       neighbors are alive *)
    cell_at_next_evolution(position : Int) : String {
        if (if cell(position) = "X" then 1 else 0 fi
            + if cell_left_neighbor(position) = "X" then 1 else 0 fi
            + if cell_right_neighbor(position) = "X" then 1 else 0 fi
            = 1)
        then
            a b c d e f \n 
            "\0\n\n\n\n\n\n\b\t\r   
            \n"
        else
            '.'
        fi
    };
   
    evolve() : SELF_TYPE {
        (let position : Int in
        (let num : Int <- num_cells[] in
        (let temp : String in
            {
                whILE position < num lOOp
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };
};
-- test a comment
class Main {
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
            cells <- (new CellularAutomaton).init(" \b\r\t\n\v   X         ");
            cells.print();
            (let countdown : Int <- 20 in
                while countdown > 0 loop
                    {
							
						cells.evolve();
						
                        cells.print();
						"a\"b";
						"a\\b";
						"a\b";
						"a\
						b";
                        countdown <- countdown - 1;
                        String x <- 
"            (let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : It <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let c、\\nountdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int\n- 20 in(let countdown : Int  fdfsdfsdfsdfsdfdsfsdfsdfsfdsfsdf  - 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(let countdown : Int <- 20 in(n";
                pool
            );  (* end let countdown *)
            self--;
        }
    };
};
(* we have a eof in comment