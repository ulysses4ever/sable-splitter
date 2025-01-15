#!/usr/bin/env bash

# Takes $FILE and replaces K foo-calls in it with $N calls to fooi,
# fooi are functions defined in separate files fooi.c (10<=i<N+10).
# Compiles the result into a.out.
#
# Example run:
#   ./split 64 filename.c
#
# The arguments are $FILE and $N, resp.
#
# Depends on GHC being installed
#

N=$1 # number of chunks; 64 can be decreased but can't go higher than 89
N1=$(($N + 9)) # start from 10 rather than from 1
FILE=$2

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

echo '0. Cleanup old foo$i''s and restore original C file'
rm -f foo*
cp $FILE.orig $FILE 2>/dev/null || echo "WARN: No original.
NOTE: If you create a backup copy of '$FILE' called '$FILE.orig' before running the script,
we will use it to restore the original on every run."

echo '1. Extract the original foo-calls in `body.ins` and put foo$i-calls instead'
runhaskell "$SCRIPT_DIR/replace_foo.hs" $N $FILE

echo "2. Split 'body.ins' into N"
split -n r/$N body.ins foo --numeric-suffixes=10 --additional-suffix=".c"

echo "3. Patch the foo\$i.c: add funciton header and footer"
for ((i=10; i<=$N1; i++)); do
#    sed -i "1i void foo$i(float *x, float *y, float *val) {" "foo$i.c"
    sed -i "1i #include \"foo.h\"\nvoid foo$i(float *x, float *y, float *val) {" "foo$i.c"
    echo "}" >> "foo$i.c"
done

echo "4. Create foo.h with the SpMV kernel (function foo)"
echo "
inline
void foo(float *y, const float* x, const float* val, int i_start, int i_end, int j_start, int j_end, int val_offset) {
    for (int j = j_start; j < j_end; j++) {
  for (int i = i_start; i < i_end; i++) {
      y[i] += ((&val[val_offset])[(((j-j_start)*(i_end-i_start)) + (i-i_start))] * x[j]);
    }
  }
}
" > foo.h

echo "5. Compiling and linking the result"
for f in ./foo*.c; do
    gcc -c -O3 -Winline $f &
done
wait
gcc -c -O3 -Wno-implicit-function-declaration $FILE
gcc ${FILE%.c}.o ./foo*.o
