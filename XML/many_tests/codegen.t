  $ dune exec ./../bin/XML.exe -- -o factorial.s <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.s
  .section .text
  .global main
  .type main, @function
  lambda_0:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    mv t0, a0
    ld t0, 8(t0)
    sd t0, -8(s0)
    mv t0, a1
    li t1, 0
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -16(s0)
    ld t0, -16(s0)
    beq t0, zero, else_0
    li t0, 1
    j endif_1
  else_0:
    mv t0, a1
    li t1, 1
    sub t0, t0, t1
    sd t0, -24(s0)
    ld t0, -8(s0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    mv t0, a1
    ld t1, -48(s0)
    mul t0, t0, t1
    sd t0, -56(s0)
    ld t0, -56(s0)
  endif_1:
    sd t0, -64(s0)
    ld a0, -64(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fac:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 16
    call malloc
    la a0, lambda_0
    li a1, 2
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    call fac
    mv t1, a0
    sd t1, 8(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    call fac
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    ld t0, 0(t0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 4
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  Segmentation fault (core dumped)
  [139]

====================== Fibonacci ======================
  $ ../bin/XML.exe -o fibonacci.s <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)

  $ cat fibonacci.s
  .section .text
  .global main
  .type main, @function
  lambda_0:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    mv t0, a0
    ld t0, 8(t0)
    sd t0, -8(s0)
    mv t0, a1
    li t1, 1
    slt t0, t1, t0
    xori t0, t0, 1
    sd t0, -16(s0)
    ld t0, -16(s0)
    beq t0, zero, else_0
    mv t0, a1
    j endif_1
  else_0:
    mv t0, a1
    li t1, 1
    sub t0, t0, t1
    sd t0, -24(s0)
    ld t0, -8(s0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    mv t0, a1
    li t1, 2
    sub t0, t0, t1
    sd t0, -56(s0)
    ld t0, -8(s0)
    sd t0, -64(s0)
    ld t0, -64(s0)
    ld t0, 0(t0)
    sd t0, -72(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -72(s0)
    ld t1, -64(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -56(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -80(s0)
    ld t0, -48(s0)
    ld t1, -80(s0)
    add t0, t0, t1
    sd t0, -88(s0)
    ld t0, -88(s0)
  endif_1:
    sd t0, -96(s0)
    ld a0, -96(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fib:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 16
    call malloc
    la a0, lambda_0
    li a1, 2
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    call fib
    mv t1, a0
    sd t1, 8(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    call fib
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    ld t0, 0(t0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 6
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc fibonacci.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  Segmentation fault (core dumped)
  [139]

====================== Ififif ======================
  $ ../bin/XML.exe -o ififif.s <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 else 1) = 1
  >           then 0 else 1 in
  >   large x

  $ cat ififif.s
  .section .text
  .global main
  .type main, @function
  lambda_0:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 40
    mv t0, a0
    ld t0, 8(t0)
    sd t0, -8(s0)
    li t0, 0
    mv t1, a1
    xor t2, t0, t1
    snez t0, t2
    sd t0, -16(s0)
    ld t0, -16(s0)
    beq t0, zero, else_0
    ld t0, -8(s0)
    sd t0, -24(s0)
    ld t0, -24(s0)
    ld t0, 0(t0)
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -32(s0)
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld t0, -40(s0)
    j endif_1
  else_0:
    ld t0, -8(s0)
    sd t0, -48(s0)
    ld t0, -48(s0)
    ld t0, 0(t0)
    sd t0, -56(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -56(s0)
    ld t1, -48(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -64(s0)
    ld t0, -64(s0)
  endif_1:
    sd t0, -72(s0)
    ld a0, -72(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  large:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 16
    call malloc
    la a0, lambda_0
    li a1, 2
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t1, a0
    sd t1, 8(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -80
    sd ra, 72(sp)
    sd s0, 64(sp)
    addi s0, sp, 64
    li t0, 0
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_2
    li t0, 0
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_3
  else_2:
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t0, a0
    sd t0, -24(s0)
    ld t0, -24(s0)
    ld t0, 0(t0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 42
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -40(s0)
    li t0, 1
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -48(s0)
    ld t0, -48(s0)
  endif_3:
    sd t0, -56(s0)
    ld t0, -56(s0)
    beq t0, zero, else_4
    li t0, 0
    j endif_5
  else_4:
    li t0, 1
  endif_5:
    sd t0, -64(s0)
    ld t0, -64(s0)
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -72(s0)
    ld t0, -72(s0)
    beq t0, zero, else_6
    li t0, 0
    j endif_7
  else_6:
    li t0, 1
  endif_7:
    sd t0, -80(s0)
    call large
    mv t0, a0
    sd t0, -88(s0)
    ld t0, -88(s0)
    ld t0, 0(t0)
    sd t0, -96(s0)
    ld t0, -96(s0)
    ld t1, -88(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -80(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -104(s0)
    ld a0, -104(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc ififif.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  Segmentation fault (core dumped)
  [139]


====================== Simple Closure ======================
  $ ../bin/XML.exe -o closure.s <<EOF
  > let simplesum x y = x + y
  > let partialapp_sum = simplesum 5
  > let main = print_int (partialapp_sum 5)
  $ cat closure.s
  .section .text
  .global main
  .type main, @function
  lambda_0:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    mv t0, a1
    mv t1, a2
    add t0, t0, t1
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  simplesum:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 8
    call malloc
    la a0, lambda_0
    li a1, 3
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  partialapp_sum:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd s0, 24(sp)
    addi s0, sp, 24
    call simplesum
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    ld t0, 0(t0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 5
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    ld a0, -24(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    call partialapp_sum
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    ld t0, 0(t0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 5
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc closure.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  copy_closure: OOM
  Aborted (core dumped)
  [134]



====================== CPS Factorial ======================
  $ ../bin/XML.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s

  $ cat 010faccps_ll.s
  .section .text
  .global main
  .type main, @function
  lambda_2:
    addi sp, sp, -96
    sd ra, 88(sp)
    sd s0, 80(sp)
    addi s0, sp, 80
    mv t0, a0
    ld t0, 8(t0)
    sd t0, -8(s0)
    mv t0, a0
    ld t0, 16(t0)
    sd t0, -16(s0)
    mv t0, a1
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -24(s0)
    ld t0, -24(s0)
    beq t0, zero, else_0
    mv t0, a2
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    ld t0, -48(s0)
    j endif_1
  else_0:
    mv t0, a1
    li t1, 1
    sub t0, t0, t1
    sd t0, -56(s0)
    ld t0, -16(s0)
    sd t0, -64(s0)
    ld t0, -64(s0)
    ld t0, 0(t0)
    sd t0, -72(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -72(s0)
    ld t1, -64(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a2
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -80(s0)
    ld t0, -8(s0)
    sd t0, -88(s0)
    ld t0, -88(s0)
    ld t0, 0(t0)
    sd t0, -96(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -96(s0)
    ld t1, -88(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -56(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -80(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -104(s0)
    ld t0, -104(s0)
  endif_1:
    sd t0, -112(s0)
    ld a0, -112(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  lambda_1:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 32
    mv t0, a3
    mv t1, a1
    mul t0, t0, t1
    sd t0, -8(s0)
    mv t0, a2
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t0, 0(t0)
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -24(s0)
    ld t1, -16(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    ld a0, -32(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  lambda_0:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 0
    mv a0, a1
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  id:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 8
    call malloc
    la a0, lambda_0
    li a1, 2
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fresh_1:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 8
    call malloc
    la a0, lambda_1
    li a1, 4
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fac_cps:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 24
    call malloc
    la a0, lambda_2
    li a1, 3
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    call fac_cps
    mv t1, a0
    sd t1, 8(a0)
    call fresh_1
    mv t1, a0
    sd t1, 16(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    call fac_cps
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    ld t0, 0(t0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 4
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    call id
    mv t1, a0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -48(s0)
    li a0, 0
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc 010faccps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  Segmentation fault (core dumped)
  [139]

====================== CPS Fibbo ======================
  $ ../bin/XML.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s

  $ cat 010fibcps_ll.s
  .section .text
  .global main
  .type main, @function
  lambda_3:
    addi sp, sp, -96
    sd ra, 88(sp)
    sd s0, 80(sp)
    addi s0, sp, 80
    mv t0, a0
    ld t0, 8(t0)
    sd t0, -8(s0)
    mv t0, a0
    ld t0, 16(t0)
    sd t0, -16(s0)
    mv t0, a1
    li t1, 2
    slt t0, t0, t1
    sd t0, -24(s0)
    ld t0, -24(s0)
    beq t0, zero, else_0
    mv t0, a2
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    ld t0, -48(s0)
    j endif_1
  else_0:
    mv t0, a1
    li t1, 1
    sub t0, t0, t1
    sd t0, -56(s0)
    ld t0, -16(s0)
    sd t0, -64(s0)
    ld t0, -64(s0)
    ld t0, 0(t0)
    sd t0, -72(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -72(s0)
    ld t1, -64(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a2
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -80(s0)
    ld t0, -8(s0)
    sd t0, -88(s0)
    ld t0, -88(s0)
    ld t0, 0(t0)
    sd t0, -96(s0)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -96(s0)
    ld t1, -88(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -56(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -80(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    sd t0, -104(s0)
    ld t0, -104(s0)
  endif_1:
    sd t0, -112(s0)
    ld a0, -112(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  lambda_2:
    addi sp, sp, -80
    sd ra, 72(sp)
    sd s0, 64(sp)
    addi s0, sp, 64
    mv t0, a0
    ld t0, 8(t0)
    sd t0, -8(s0)
    mv t0, a1
    li t1, 2
    sub t0, t0, t1
    sd t0, -16(s0)
    ld t0, -8(s0)
    sd t0, -24(s0)
    ld t0, -24(s0)
    ld t0, 0(t0)
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a4, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -32(s0)
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a4
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    mv t1, a2
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a4, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    mv t0, a3
    sd t0, -48(s0)
    ld t0, -48(s0)
    ld t0, 0(t0)
    sd t0, -56(s0)
    addi sp, sp, -8
    sd a4, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -56(s0)
    ld t1, -48(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -16(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -40(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a4, 0(sp)
    addi sp, sp, 8
    sd t0, -64(s0)
    ld a0, -64(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  lambda_1:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 32
    mv t0, a1
    mv t1, a3
    add t0, t0, t1
    sd t0, -8(s0)
    mv t0, a2
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t0, 0(t0)
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld t0, -24(s0)
    ld t1, -16(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    ld a0, -32(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  lambda_0:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 0
    mv a0, a1
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  id:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 8
    call malloc
    la a0, lambda_0
    li a1, 2
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fresh_2:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 8
    call malloc
    la a0, lambda_1
    li a1, 4
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fresh_1:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 16
    call malloc
    la a0, lambda_2
    li a1, 5
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    call fresh_2
    mv t1, a0
    sd t1, 8(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fib:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li a0, 24
    call malloc
    la a0, lambda_3
    li a1, 3
    call alloc_closure
    mv t1, a0
    sd t1, 0(a0)
    call fib
    mv t1, a0
    sd t1, 8(a0)
    call fresh_1
    mv t1, a0
    sd t1, 16(a0)
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    call fib
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    ld t0, 0(t0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    li t1, 6
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    call id
    mv t1, a0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    la a0, print_int
    li a1, 1
    call alloc_closure
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    ld t0, 0(t0)
    sd t0, -40(s0)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld t1, -24(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -48(s0)
    li a0, 0
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc 010fibcps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  Segmentation fault (core dumped)
  [139]
