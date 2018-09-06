./luttu.native -l test.mc > a.ll
llc-3.8 a.ll -filetype=obj 
gcc a.o
./a.out; echo $?
