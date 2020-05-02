echo "building"
stack build

echo "running"
stack exec main test/$1.txt > bin/main.ll

echo "compiling to .o"
llc-9 -filetype=obj -o bin/main.o bin/main.ll

echo "compiling external libs"
gcc src/defs.c -c -o bin/defs.o

echo "compiling to binary"
gcc bin/main.o bin/defs.o -o bin/main