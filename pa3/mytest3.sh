#!/bin/bash

# arg1: coolfiles
# arg2: reimpl lexer name
# arg3: reimpl parser name

# get all filename
coolfiles=$(ls $1)
for f in $coolfiles
do
  echo "checking "$f
  diff <($2 $1$f | $3) <($2 $1$f | ./parser)
done

echo "checking good.cl"
diff <($2 "./good.cl" | $3) <($2 "./good.cl" | ./parser)
echo "checking bad.cl"
diff <($2 "./bad.cl" | $3) <($2 "./bad.cl" | ./parser)
