L81:
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
jal L47
j L80
L80:
L132:
beq $s3, $t0, L77
bne $s3, $t0, L78
L78:
li $s2, 0
L75:
addi $t1, $s2, ~1
li $s0, 4
mult $s0, $t1, $s0
add $s0, $s7, $s0
lw $t1, 0($s0)
li $s0, 0
beq $t1, $s0, L63
bne $t1, $s0, L64
L64:
li $s0, 0
L65:
li $s0, 0
bne $s0, $s0, L68
beq $s0, $s0, L69
L69:
li $s0, 0
L70:
li $s0, 0
bne $s0, $s0, L73
beq $s0, $s0, L74
L74:
addi $s0, $t0, ~1
beq $s2, $s0, L62
bne $s2, $s0, L76
L76:
addi $s0, $s2, 1
j L75
L77:
jal L48
move $s0, $v0
L79:
move $v0, $s0
j L131
L63:
li $s0, 1
add $t1, $s2, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s5, $t1
lw $t2, 0($t1)
li $t1, 0
beq $t2, $t1, L66
bne $t2, $t1, L67
L67:
li $s0, 0
L66:
j L65
L68:
li $s0, 1
addi $t1, $s2, 7
sub $t1, $t1, $s3
addi $t2, $t1, ~1
li $t1, 4
mult $t1, $t2, $t1
add $t1, $s4, $t1
lw $t2, 0($t1)
li $t1, 0
beq $t2, $t1, L71
bne $t2, $t1, L72
L72:
li $s0, 0
L71:
j L70
L73:
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
jal L47
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
j L74
L62:
li $s0, 0
j L79
L131:
L242:
li $s4, 0
L59:
li $s3, 0
L56:
addi $s2, $s4, ~1
li $s0, 4
mult $s0, $s2, $s0
add $s0, $s5, $s0
lw $s0, 0($s0)
beq $s0, $s3, L53
bne $s0, $s3, L54
L54:
la $s0, L52
L55:
move $a0, $s0
jal print
addi $s0, $s6, ~1
beq $s3, $s0, L50
bne $s3, $s0, L57
L57:
addi $s0, $s3, 1
j L56
L53:
la $s0, L51
j L55
L50:
la $s0, L58
move $a0, $s0
jal print
addi $s0, $s6, ~1
beq $s4, $s0, L49
bne $s4, $s0, L60
L60:
addi $s0, $s4, 1
j L59
L49:
la $s0, L61
move $a0, $s0
jal print
j L241
L241:
L61: 

L58: 

L52:  .
L51:  O
