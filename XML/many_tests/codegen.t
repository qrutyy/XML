  $ ls .
  $  ./../bin/XML.exe -o factorial.s <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.s
  .section .text
  .global main
  .type main, @function
  fac:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 32
    mv t0, a0
    sd t0, -8(s0) # binop: spill LHS
    li t1, 0
    ld t2, -8(s0)
    xor t0, t2, t1
    seqz t0, t0
    beq t0, zero, else_0
    li a0, 1
    j end_1
  else_0:
    mv t0, a0
    sd t0, -16(s0) # binop: spill LHS
    mv t0, a0
    sd t0, -24(s0) # binop: spill LHS
    li t1, 1
    ld t2, -24(s0)
    sub a0, t2, t1
    addi sp, sp, -8 # Saving 'live' regs
    sd a0, -32(s0)
    call fac
    mv t1, a0
    ld t2, -16(s0)
    mul a0, t2, t1
  end_1:
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16
    li a0, 4
    call fac
    call print_int
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  24
