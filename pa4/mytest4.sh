#!/bin/bash

# arg1: coolfiles
# arg2: reimpl lexer name
# arg3: reimpl parser name

# get all filename
coolfiles=$(ls $1)
for f in $coolfiles
do
  echo "checking "$f
  diff <(../refimpl/lexer $1$f | ../refimpl/parser | ../refimpl/semant) <(../refimpl/lexer  $1$f | ../refimpl/parser | ./semant)
done

echo "checking good.cl"
diff <(../refimpl/lexer  "./good.cl" | ../refimpl/parser | ../refimpl/semant) <(../refimpl/lexer "./good.cl" | ../refimpl/parser | ./semant)
echo "checking bad.cl"
diff <(../refimpl/lexer  "./bad.cl" | ../refimpl/parser | ../refimpl/semant) <(../refimpl/lexer "./bad.cl" | ../refimpl/parser | ./semant)
