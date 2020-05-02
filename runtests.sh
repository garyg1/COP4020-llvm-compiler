echo "building"
stack build

mkdir -p bin

echo "compiling external libs"
gcc src/defs.c -c -o bin/defs.o

for filename in test/*.txt; do
    prefix=${filename:5:-4}
    echo "compiling $prefix"
    stack exec main test/$prefix.txt > bin/$prefix.ll
    llc-9 -filetype=obj -o bin/$prefix.o bin/$prefix.ll
    gcc bin/$prefix.o bin/defs.o -o bin/$prefix.bin
done

for filename in bin/*.bin; do
    prefix=${filename:4:-4}
    ./$filename > bin/$prefix.actual
    if diff -q bin/$prefix.actual test/$prefix.expected; then
        echo "[✓] passed $prefix"
    else
        echo "[✗] failed $prefix"
    fi
done