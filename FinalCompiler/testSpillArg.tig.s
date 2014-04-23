tig_main:
	addi $sp, $sp, -32
	sw $s7, 28($sp)
	sw $s6, 24($sp)
	sw $s5, 20($sp)
	sw $s4, 16($sp)
	sw $s3, 12($sp)
	sw $s2, 8($sp)
	sw $s1, 4($sp)
	sw $s0, 0($sp)
	sw $a0, 0($fp)
	li $s0, 5
	move $a0, $fp
	li $a1, 7
	li $a2, 8
	li $a3, 9
	li $s2, 0
	addi $s2, $s2, -10
	sw $s2, 36($sp)
	li $s2, 0
	addi $s2, $s2, -11
	sw $s2, 40($sp)
	sw $fp, 32($sp)
	addi $fp, $sp, -4
	addi $sp, $sp, -4
	jal L765
	addi $sp, $sp, 4
	lw $fp, 32($sp)
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	lw $s0, 0($sp)
	lw $s1, 4($sp)
	lw $s2, 8($sp)
	lw $s3, 12($sp)
	lw $s4, 16($sp)
	lw $s5, 20($sp)
	lw $s6, 24($sp)
	lw $s7, 28($sp)
	addi $sp, $sp, 32
	jr $ra
L765:
	addi $sp, $sp, -32
	sw $s7, 28($sp)
	sw $s6, 24($sp)
	sw $s5, 20($sp)
	sw $s4, 16($sp)
	sw $s3, 12($sp)
	sw $s2, 8($sp)
	sw $s1, 4($sp)
	sw $s0, 0($sp)
	sw $a0, 0($fp)
	move $s5, $a1
	move $s3, $a2
	move $s4, $a3
	lw $s2, 40($fp)
	lw $s0, 44($fp)
	mult $s3, $s5, $s3
	mult $s2, $s4, $s2
	sub $s2, $s3, $s2
	sub $s0, $s2, $s0
	move $v0, $s0
	lw $s0, 0($sp)
	lw $s1, 4($sp)
	lw $s2, 8($sp)
	lw $s3, 12($sp)
	lw $s4, 16($sp)
	lw $s5, 20($sp)
	lw $s6, 24($sp)
	lw $s7, 28($sp)
	addi $sp, $sp, 32
	jr $ra
