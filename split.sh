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

if [ $# -ne 2 ]; then
  echo "Incorrect number of arguments. Expected 2 arguments."
  exit 1
fi

N=$1 # number of chunks; 64 can be decreased but can't go higher than 89
N1=$(($N + 9)) # start from 10 rather than from 1
FILE=$2
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
C_OPTS="-O3 -mavx -mprefer-vector-width=512 -funroll-all-loops -Wno-implicit-function-declaration"

echo '0. Initialize the working directory -- $SAVE_DIR -- and CD there'
# $SAVE_DIR is the input file name without .c and the directory part
FILE_NODIR="${FILE##*/}"  # Remove directory path -> "g7jac020.c"
BASENAME="${FILE_NODIR%.c}"   # Remove ".c" extension -> "g7jac020"
SAVE_DIR="./split-and-binaries/$BASENAME"
rm -rf $SAVE_DIR
mkdir -p $SAVE_DIR
cp "$FILE" "$SAVE_DIR/$FILE_NODIR"
echo "NOTE: Save dir is $SAVE_DIR"
pushd "$SAVE_DIR" > /dev/null

if [ "$N" -eq 0 ]; then
    echo "Fast path: split into 0 pieces means just compile the input"
    gcc $C_OPTS -o "./$BASENAME" "$BASENAME.c"
    popd > /dev/null
    exit
fi

echo '1. Extract the original foo-calls into `body.ins` and put foo$i-calls instead'
runhaskell "$SCRIPT_DIR/replace_foo.hs" $N "$FILE"

echo "2. Split 'body.ins' into N"
split -n r/$N ./body.ins foo --numeric-suffixes=10 --additional-suffix=".c"

echo "3. Patch the foo\$i.c: add funciton header and footer"
for ((i=10; i<=$N1; i++)); do
    f="./foo$i.c"
    { echo -e "#include \"foo.h\"\nvoid foo$i(float *x, float *y, float *val) {" ; cat $f; } > $f.tmp
    mv $f.tmp $f
    echo "}" >> $f
done

echo "4. Create foo.h with the SpMV kernel (function foo)"
echo "inline __attribute__((always_inline))
void foo(float *y, const float* x, const float* val, int i_start, int i_end, int j_start, int j_end, int val_offset) {
    for (int j = j_start; j < j_end; j++) {
  for (int i = i_start; i < i_end; i++) {
      y[i] += ((&val[val_offset])[(((j-j_start)*(i_end-i_start)) + (i-i_start))] * x[j]);
    }
  }
}
" > "./foo.h"

echo "5. Compiling and linking the result"
for f in ./foo*.c; do
    gcc -c -O3 -Winline $f &
done
wait
gcc -c $C_OPTS -Wno-implicit-function-declaration $FILE
gcc -o "./$BASENAME" "$BASENAME.o" ./foo*.o
popd > /dev/null
