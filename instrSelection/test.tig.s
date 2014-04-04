L14:
li t153, 0
move t143, t153
L11:
li t154, 1
beq t143, t154, L10
bne t143, t154, L12
L12:
addi t155, t143, 1

L10:
li t156, 0
move t101, t156
j L13
L13:
