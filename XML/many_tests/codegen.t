  $ ../bin/XML.exe -o factorial.s <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)
  ../bin/XML.exe: not found
  [127]

  $ cat factorial.s
  cat: factorial.s: No such file or directory
  [1]
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  Assembler messages:
  Error: can't open factorial.s for reading: No such file or directory
  [1]
  $ riscv64-linux-gnu-gcc -c bin/runtime.c -o runtime.o
  cc1: fatal error: bin/runtime.c: No such file or directory
  compilation terminated.
  [1]
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  /usr/lib/gcc-cross/riscv64-linux-gnu/13/../../../../riscv64-linux-gnu/bin/ld: cannot find temp.o: No such file or directory
  /usr/lib/gcc-cross/riscv64-linux-gnu/13/../../../../riscv64-linux-gnu/bin/ld: cannot find runtime.o: No such file or directory
  collect2: error: ld returned 1 exit status
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  [1]

====================== Fibonacci ======================
  $ ../bin/XML.exe -o fibonacci.s <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)
  ../bin/XML.exe: not found
  [127]

  $ cat fibonacci.s
  cat: fibonacci.s: No such file or directory
  [1]
  $ riscv64-linux-gnu-as -march=rv64gc fibonacci.s -o temp.o
  Assembler messages:
  Error: can't open fibonacci.s for reading: No such file or directory
  [1]
  $ riscv64-linux-gnu-gcc -c bin/runtime.c -o runtime.o
  cc1: fatal error: bin/runtime.c: No such file or directory
  compilation terminated.
  [1]
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  /usr/lib/gcc-cross/riscv64-linux-gnu/13/../../../../riscv64-linux-gnu/bin/ld: cannot find temp.o: No such file or directory
  /usr/lib/gcc-cross/riscv64-linux-gnu/13/../../../../riscv64-linux-gnu/bin/ld: cannot find runtime.o: No such file or directory
  collect2: error: ld returned 1 exit status
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  [1]

====================== Ififif ======================
  $ ../bin/XML.exe -o ififif.s <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 else 1) = 1
  >           then 0 else 1 in
  >   large x
  ../bin/XML.exe: not found
  [127]

  $ cat ififif.s
  cat: ififif.s: No such file or directory
  [1]

  $ riscv64-linux-gnu-as -march=rv64gc ififif.s -o temp.o
  Assembler messages:
  Error: can't open ififif.s for reading: No such file or directory
  [1]
  $ riscv64-linux-gnu-gcc -c bin/runtime.c -o runtime.o
  cc1: fatal error: bin/runtime.c: No such file or directory
  compilation terminated.
  [1]
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  /usr/lib/gcc-cross/riscv64-linux-gnu/13/../../../../riscv64-linux-gnu/bin/ld: cannot find temp.o: No such file or directory
  /usr/lib/gcc-cross/riscv64-linux-gnu/13/../../../../riscv64-linux-gnu/bin/ld: cannot find runtime.o: No such file or directory
  collect2: error: ld returned 1 exit status
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  [1]
