L37:
li $s3, 8
li $s0, 4
mult $s0, $s3, $s0
move $a0, $s0
jal malloc
move $s0, $v0
move $a0, $s3
li $a1, 0
jal initArray
li $s0, 4
mult $s0, $s3, $s0
move $a0, $s0
jal malloc
move $s0, $v0
move $a0, $s3
li $a1, 0
jal initArray
add $s0, $s3, $s3
addi $s2, $s0, ~1
li $s0, 4
mult $s0, $s2, $s0
move $a0, $s0
jal malloc
move $s0, $v0
add $s2, $s3, $s3
addi $s2, $s2, ~1
move $a0, $s2
li $a1, 0
jal initArray
add $s0, $s3, $s3
addi $s2, $s0, ~1
li $s0, 4
mult $s0, $s2, $s0
move $a0, $s0
jal malloc
move $s0, $v0
add $s2, $s3, $s3
addi $s2, $s2, ~1
move $a0, $s2
li $a1, 0
jal initArray
li $a0, 0
jal L3
j L36
L36:
L88:
beq $s3, $t0, L33
bne $s3, $t0, L34
L34:
li $s2, 0
L31:
addi $t1, $s2, ~1
li $s0, 4
mult $s0, $t1, $s0
add $s0, $s7, $s0
lw $t1, 0($s0)
li $s0, 0
beq $t1, $s0, L19
bne $t1, $s0, L20
L20:
li $s0, 0
L21:
li $s0, 0
bne $s0, $s0, L24
beq $s0, $s0, L25
L25:
li $s0, 0
L26:
li $s0, 0
bne $s0, $s0, L29
beq $s0, $s0, L30
L30:
addi $s0, $t0, ~1
beq $s2, $s0, L18
bne $s2, $s0, L32
L32:
addi $s0, $s2, 1
j L31
L33:
jal L4
move $s0, $v0
L35:
move $v0, $s0
j L87
L19:
li $s0, 1
add $t1, $s2, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s5, $t1
lw $t2, 0($t1)
li $t1, 0
beq $t2, $t1, L22
bne $t2, $t1, L23
L23:
li $s0, 0
L22:
j L21
L24:
li $s0, 1
addi $t1, $s2, 7
sub $t1, $t1, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s4, $t1
lw $t2, 0($t1)
li $t1, 0
beq $t2, $t1, L27
bne $t2, $t1, L28
L28:
li $s0, 0
L27:
j L26
L29:
li $s0, 1
addi $t2, $s2, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s7, $t1
lw $s0, 0($t1)
li $s0, 1
add $t1, $s2, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s5, $t1
lw $s0, 0($t1)
li $s0, 1
addi $t1, $s2, 7
sub $t1, $t1, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s4, $t1
lw $s0, 0($t1)
addi $t1, $s3, ~1
li $s0, 4
mult $s0, $t1, $s0
add $s0, $s6, $s0
lw $s2, 0($s0)
addi $s0, $s3, 1
move $a0, $s0
jal L3
li $s0, 0
addi $t2, $s2, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s7, $t1
lw $s0, 0($t1)
li $s0, 0
add $t1, $s2, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s5, $t1
lw $s0, 0($t1)
li $s0, 0
addi $t1, $s2, 7
sub $t1, $t1, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s4, $t1
lw $s0, 0($t1)
j L30
L18:
li $s0, 0
j L35
L87:
L198:
li $s4, 0
L15:
li $s3, 0
L12:
addi $s2, $s4, ~1
li $s0, 4
mult $s0, $s2, $s0
add $s0, $s5, $s0
lw $s0, 0($s0)
beq $s0, $s3, L9
bne $s0, $s3, L10
L10:
la $s0, L8
L11:
move $a0, $s0
jal print
addi $s0, $s6, ~1
beq $s3, $s0, L6
bne $s3, $s0, L13
L13:
addi $s0, $s3, 1
j L12
L9:
la $s0, L7
j L11
L6:
la $s0, L14
move $a0, $s0
jal print
addi $s0, $s6, ~1
beq $s4, $s0, L5
bne $s4, $s0, L16
L16:
addi $s0, $s4, 1
j L15
L5:
la $s0, L17
move $a0, $s0
jal print
j L197
L197:
L17: 

L14: 

L8:  .
L7:  O
