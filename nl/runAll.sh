./luttu.native -l test.mc > a.ll
gcc -c printbig.c
llc-3.8 a.ll -filetype=obj 
gcc a.o printbig.o
./a.out; echo $?
