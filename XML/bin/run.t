  $ clang-18 -c runtime.c -o runtime.o
  /tmp/dune_cram_a69a99_.cram.sh/main.sh: 1: /tmp/dune_cram_a69a99_.cram.sh/1.sh: clang-18: not found
  [127]
  $ ./main.exe
  Fatal error: exception Llvm_executionengine.Error("No available targets are compatible with triple \"x86_64-pc-linux-gnu\"")
  [2]
  $ ls
  main.exe
  runtime.c
  $ cat out.ll | grep -E 'source_filename|target datalayout|ModuleID' --invert-match
  cat: out.ll: No such file or directory
  [1]
  $ clang-18 out.ll runtime.o -o demo1.exe
  /tmp/dune_cram_a69a99_.cram.sh/main.sh: 1: /tmp/dune_cram_a69a99_.cram.sh/5.sh: clang-18: not found
  [127]
  $ echo "Press $(./demo1.exe) to pay respect"
  /tmp/dune_cram_a69a99_.cram.sh/main.sh: 1: /tmp/dune_cram_a69a99_.cram.sh/6.sh: ./demo1.exe: not found
  Press  to pay respect
