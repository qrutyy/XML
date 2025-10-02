.global _start
_start: li a0, 6 # function arg 
	call fac # result is in a0. N.B. maybe BUG?
# Программа завершает работу системным вызовом exit(a0).
# Если бы мы вернули 720, то это число не влезает в стандартный диапазон кодов возврата (они обычно трактуются как 8-битные).
# Поэтому берут 720 mod 256 = 208.
# exit value in unix systems is limited to 8 bits.

	li a5, 256
	rem a0, a0, a5 # 6! − 256∗2 = 208
	li a7, 93 # System call number 93 terminates
	ecall # Ask Linux to perform system call
fac: li a1, 1 # 1
	ble a0, a1, .fac_exit # branch if less or equal
	mv a5, a0 # a0 is input
	li a0, 1 # product, return value
.fac_loop: mv a4, a5
	addi a5, a5, −1
	mul a0, a4, a0
	bne a5, a1, .fac_loop # branch if not equal
.fac_exit: ret


li— load immediate
la— load address
ecall— environment call
Секции .text и .data
rem— reminder (остаток от
деления)
ble— branch if less or equal
ret— return


fun -> create new_label
if we have an op -> we add both values to registers
then we compare

