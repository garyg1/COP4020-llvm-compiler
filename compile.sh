stack build

echo "running"
stack exec main test/asttest.txt > bin/main.ll

echo "compiling to .o"
llc-9 -filetype=obj -o bin/main.o bin/main.ll

echo "compiling external libs"
gcc src/foo.c -c -o bin/foo.o

echo "compiling to binary"
gcc bin/main.o bin/foo.o -o bin/main