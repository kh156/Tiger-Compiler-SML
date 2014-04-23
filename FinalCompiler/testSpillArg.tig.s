tig_main:
	sw $fp, -12($sp)
	addi $fp, $sp, -4
	sw $ra, -4($fp)
	sw $s7, -40($fp)
	sw $s6, -36($fp)
	sw $s5, -32($fp)
	sw $s4, -28($fp)
	sw $s3, -24($fp)
	sw $s2, -20($fp)
	sw $s1, -16($fp)
	sw $s0, -12($fp)
	addi $s0, $sp, -52
	move $sp, $s0
	sw $a0, 0($fp)
	li $s0, 5
	move $a0, $fp
	li $a1, 7
	li $a2, 8
	li $a3, 9
	li $s2, 0
	addi $s2, $s2, -10
	sw $s2, 0($sp)
	li $s2, 0
	addi $s2, $s2, -11
	sw $s2, 4($sp)
	jal L168
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	addi $s0, $sp, 52
	move $sp, $s0
	lw $s0, -12($fp)
	lw $s1, -16($fp)
	lw $s2, -20($fp)
	lw $s3, -24($fp)
	lw $s4, -28($fp)
	lw $s5, -32($fp)
	lw $s6, -36($fp)
	lw $s7, -40($fp)
	lw $ra, -4($fp)
	lw $fp, -12($sp)
	jr $ra
L168:
	sw $fp, -12($sp)
	addi $fp, $sp, -4
	sw $ra, -4($fp)
	sw $s7, -40($fp)
	sw $s6, -36($fp)
	sw $s5, -32($fp)
	sw $s4, -28($fp)
	sw $s3, -24($fp)
	sw $s2, -20($fp)
	sw $s1, -16($fp)
	sw $s0, -12($fp)
	addi $s0, $sp, -44
	move $sp, $s0
	sw $a0, 0($fp)
	move $s5, $a1
	move $s3, $a2
	move $s4, $a3
	lw $s2, 4($fp)
	lw $s0, 8($fp)
	mult $s3, $s5, $s3
	mult $s2, $s4, $s2
	sub $s2, $s3, $s2
	sub $s0, $s2, $s0
	move $v0, $s0
	addi $s0, $sp, 44
	move $sp, $s0
	lw $s0, -12($fp)
	lw $s1, -16($fp)
	lw $s2, -20($fp)
	lw $s3, -24($fp)
	lw $s4, -28($fp)
	lw $s5, -32($fp)
	lw $s6, -36($fp)
	lw $s7, -40($fp)
	lw $ra, -4($fp)
	lw $fp, -12($sp)
	jr $ra
