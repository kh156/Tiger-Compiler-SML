L20:
li $t211, 5
jal L17
move $v0, $v0
j L19
L19:
L22:
li $t212, 6
jal L18
move $v0, $v0
j L21
L21:
L24:
lw $t215, ~4($fp)
lw $t214, ~8($t215)
add $t213, $t183, $t214
move $v0, $t213
j L23
L23:
