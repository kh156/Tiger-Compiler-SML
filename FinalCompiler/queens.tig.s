L73:
li $s2, 8
li $s0, 4
mult $s0, $s2, $s0
move $a0, $s0
jal malloc
move $s0, $v0
move $a0, $s2
li $a1, 0
jal initArray
li $s0, 4
mult $s0, $s2, $s0
move $a0, $s0
jal malloc
move $s0, $v0
move $a0, $s2
li $a1, 0
jal initArray
add $s0, $s2, $s2
addi $s3, $s0, ~1
li $s0, 4
mult $s0, $s3, $s0
move $a0, $s0
jal malloc
move $s0, $v0
add $s3, $s2, $s2
addi $s3, $s3, ~1
move $a0, $s3
li $a1, 0
jal initArray
add $s0, $s2, $s2
addi $s3, $s0, ~1
li $s0, 4
mult $s0, $s3, $s0
move $a0, $s0
jal malloc
move $s0, $v0
add $s2, $s2, $s2
addi $s2, $s2, ~1
move $a0, $s2
li $a1, 0
jal initArray
li $a0, 0
jal L39
j L72
L72:
L124:
beq $s3, $t0, L69
bne $s3, $t0, L70
L70:
li $s2, 0
L67:
addi $t1, $s2, ~1
li $s0, 4
mult $s0, $t1, $s0
add $s0, $s7, $s0
lw $t1, 0($s0)
li $s0, 0
beq $t1, $s0, L55
bne $t1, $s0, L56
L56:
li $s0, 0
L57:
li $t1, 0
bne $s0, $t1, L60
beq $s0, $t1, L61
L61:
li $s0, 0
L62:
li $t1, 0
bne $s0, $t1, L65
beq $s0, $t1, L66
L66:
addi $s0, $t0, ~1
beq $s2, $s0, L54
bne $s2, $s0, L68
L68:
addi $s0, $s2, 1
j L67
L69:
jal L40
move $s0, $v0
L71:
move $v0, $s0
j L123
L55:
li $s0, 1
add $t1, $s2, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s5, $t1
lw $t2, 0($t1)
li $t1, 0
beq $t2, $t1, L58
bne $t2, $t1, L59
L59:
li $s0, 0
L58:
j L57
L60:
li $s0, 1
addi $t1, $s2, 7
sub $t1, $t1, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s4, $t1
lw $t2, 0($t1)
li $t1, 0
beq $t2, $t1, L63
bne $t2, $t1, L64
L64:
li $s0, 0
L63:
j L62
L65:
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
jal L39
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
j L66
L54:
li $s0, 0
j L71
L123:
L234:
li $s4, 0
L51:
li $s3, 0
L48:
addi $s2, $s4, ~1
li $s0, 4
mult $s0, $s2, $s0
add $s0, $s5, $s0
lw $s0, 0($s0)
beq $s0, $s3, L45
bne $s0, $s3, L46
L46:
la $s0, L44
L47:
move $a0, $s0
jal print
addi $s0, $s6, ~1
beq $s3, $s0, L42
bne $s3, $s0, L49
L49:
addi $s0, $s3, 1
j L48
L45:
la $s0, L43
j L47
L42:
la $s0, L50
move $a0, $s0
jal print
addi $s0, $s6, ~1
beq $s4, $s0, L41
bne $s4, $s0, L52
L52:
addi $s0, $s4, 1
j L51
L41:
la $s0, L53
move $a0, $s0
jal print
j L233
L233:
L53: 

L50: 

L44:  .
L43:  O
