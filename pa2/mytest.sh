#!/bin/bash

# arg1: coolfiles
# arg2: reimpl lexer name

# get all filename
coolfiles=$(ls $1)
for f in $coolfiles
do
  echo "checking "$f
  diff <(./lexer $1$f) <($2 $1$f)
done

echo "checking test.cl"
diff <(./lexer "./test.cl") <($2 "./test.cl")
