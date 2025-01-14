#!/usr/bin/env bash

# Takes $FILE and replaces K foo-calls in it with $N calls to fooi,
# fooi are functions defined in separate files fooi.c (10<=i<N+10).
# Compiles the result into a.out.
#
# Example run:
#   ./split
#
# Depends on GHC being installed

FILE=worms20_10NN.c
N=64 # number of chunks; 64 can be decreased but can't go higher than 89
N1=$(($N + 9)) # start from 10 rather than from 1

echo "1. Put the original foo-calls in 'body.ins'"
grep -E '^(\s*foo.*)' $FILE > body.ins

echo "2. Split 'body.ins' into N"
split -n r/$N body.ins foo --numeric-suffixes=10 --additional-suffix=".c"

echo "3. Patch the foo\$i.c: add funciton header and footer"
for ((i=10; i<=$N1; i++)); do
    sed -i "1i void foo$i(float *x, float *y, float *val) {" "foo$i.c"
    echo "}" >> "foo$i.c"
done

echo "4. Replace foo-calls with (fewer) foo\$i-calls (10<=i<N+9)"
runhaskell ./replace_foo.hs $N $FILE

echo "5. Compiling and linking the result"
gcc -c -O3 -Wno-implicit-function-declaration $FILE ./foo*.c
gcc ${FILE%.c}.o ./foo*.o
