  $ clang-18 -c runtime.c -o runtime.o
  clang-18: command not found
  [127]
  $ ./main.exe
  $ ls
  main.exe
  runtime.c
  $ cat out.ll | grep -E 'source_filename|target datalayout|ModuleID' --invert-match
  cat: out.ll: No such file or directory
  [1]
  $ clang-18 out.ll runtime.o -o demo1.exe
  clang-18: command not found
  [127]
  $ echo "Press $(./demo1.exe) to pay respect"
  ./demo1.exe: No such file or directory
  Press  to pay respect
