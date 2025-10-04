.global _start
    .section .text
    .globl main
    .globl fac
    
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd fp, 32(sp)
      addi fp, sp, 32
      mv t0, a0
      mv t2, t0
      li t1, 1
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, x0, else_0
      li a0, 1
      j end_1
    else_0:
      mv t0, a0
      mv t2, t0
      mv t0, a0
      mv t2, t0
      li t1, 1
      sub t0, t2, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t2, t1
    end_1:
      ld ra, 8(fp)
      ld fp, 0(fp)
      addi sp, sp, 48
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd fp, 16(sp)
      addi fp, sp, 16
      li t0, 3
      mv a0, t0
      call fac
      mv t0, a0
      mv a0, t0
      call print_int
      li a0, 0
      ld ra, 8(fp)
      ld fp, 0(fp)
      addi sp, sp, 64
      ret
