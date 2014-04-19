L23:
li $s0, 5
li $a0, 7
jal L21
move $s2, $v0
add $s0, $s0, $s2
move $v0, $s0
j L22
L22:
L33:
addi $s0, $s0, 6
move $v0, $s0
j L32
L32:
