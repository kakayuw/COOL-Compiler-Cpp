(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

(*
 *  StackCommand consists of operator and operand, each implemented by its subclass.
 *   Value of a StackCommand is its String representation.
 *)
Class StackCommand inherits IO {
   val : String;

   eval(stack: Stack) : Stack { stack };

   -- the value of stack command is String type by default
   getVal() : String { val };

   -- getIntVal should never be called by StackCommand type
   getIntVal() : Int { {abort(); 0;} };

   init(v: String) : SELF_TYPE { { val <- v; self; } }; 
};

-- Command definition for integer operand
Class StackOpnd inherits StackCommand {
   -- only StackOpnd type could call getIntVal()
   -- get integer value of operand
   getIntVal() : Int { 
      let strtool : A2I <- new A2I in strtool.a2i(val)
   };
};

-- Command definition for '+' operator
Class StackAdd inherits StackCommand {
   eval(stack: Stack) : Stack {
      let strtool : A2I <- new A2I in
         let a: StackCommand <- stack.pop(), b: StackCommand <- stack.pop() in 
            stack.push(    -- pop top 2 elements and push their sum
                  (new StackOpnd).init(strtool.i2a( a.getIntVal() + b.getIntVal()))
               )
   };
};

-- Command definition for 's' operator
Class StackSwap inherits StackCommand {
   eval(stack: Stack) : Stack {
      let strtool : A2I <- new A2I in
         let a: StackCommand <- stack.pop(), b: StackCommand <- stack.pop() in {
            stack.push( a );  -- pop top 2 elements and push them reversely
            stack.push( b );
         }
   };
};

-- Command definition for 'd' operator
Class StackDisplay inherits StackCommand {
   eval(stack: Stack) : Stack {
      {
         stack.printState();
         stack;     -- need to return stack 
      }
   };
};

-- Command definition for 'e' operator
Class StackEval inherits StackCommand {
   eval(stack: Stack) : Stack {
      if stack.isEmpty() then stack
      else case stack.getTop() of 
         add   :  StackAdd => {stack.pop(); add.eval(stack);};  -- first pop the oprt from stack
         swap  :  StackSwap => {stack.pop(); swap.eval(stack);};  
         int   :  StackOpnd => stack;
      esac fi
   };
};


(*
 *  Node serves as the base element of the Linked list
 *)
class Node inherits IO{
   cmd: StackCommand;   -- value of the Node
   next: Node;    -- indicate the next node address

   init(value : StackCommand, rest : Node) : Node {
      {
         cmd <- value;
         next <- rest;
         self;
      }
   };

   getCmd() : StackCommand { cmd };

   printVal() : Node {
      out_string(cmd.getVal().concat("\n"))
   };

   next() : Node {
      next
   };
};


(*
 *  Stack is implemented by linked list, supporting push, pop, getLen 
 *)
class Stack inherits IO {
   len: Int <- 0;
   top: Node;

   -- Return whether the stack is empty
   isEmpty() : Bool {
      (len = 0)
   };

   -- Return the top element of the Stack
   getTop() : StackCommand {
      if isvoid top then let void: StackCommand in void  -- return void if stack is empty
      else top.getCmd()
      fi
   };

   -- Push an element to the top of the Stack
   push(val : StackCommand) : Stack {
      {
         top <- (new Node).init(val, top);
         len <- len + 1;
         self;
      }
   };

   -- Pop an element from the top of the Stack
   pop() : StackCommand {
      if isvoid top then let void: StackCommand in void  -- return void if stack is empty
      else  {
         len <- len - 1;
         let poped_element : Node <- top in {
            top <- top.next();
            poped_element.getCmd();
         };    
      } fi
   };

   -- Print the stack state
   printState() : Object {
      let cur : Node <- top in {
         while (not isvoid cur) loop
         {
            cur.printVal();
            cur <- cur.next();
         } pool;
      }
   };
};


(*
 * The main class serves as a driver for testing.
 *  While maintaining a command stack, Main method repeatedly read input
 *  command in String form and then convert to its corresponding command 
 *  class. If command '+', 's' or integer encountered, they will be push
 *  into the stack; command 'e', 'd' will operate on the stack; command 
 *  'x' will break the loop and stop the program gracefully.
 *)
class Main inherits IO {
   s : Stack <- new Stack;

   prompt() : String {{ out_string(">"); in_string(); }};

   main() : Object {
      let notEnd : Bool <- true in
         while notEnd loop
            let input : String <- prompt() in {
               -- out_string(input.concat("\n"));
               if input = "x" then notEnd <- false
               else if input = "d" then (new StackDisplay).eval(s)
               else if input = "e" then (new StackEval).eval(s)
               else if input = "s" then s.push((new StackSwap).init(input))
               else if input = "+" then s.push((new StackAdd).init(input))
               else  s.push((new StackOpnd).init(input))
               fi fi fi fi fi;
            }
         pool
   };

};
