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
	li $s2, 20
	li $s0, 4
	mult $s0, $s2, $s0
	move $a0, $s0
	sw $fp, 32($sp)
	addi $fp, $sp, -4
	addi $sp, $sp, -4
	jal malloc
	addi $sp, $sp, 4
	lw $fp, 32($sp)
	move $s0, $v0
	li $a0, 20
	li $a1, 0
	sw $fp, 32($sp)
	addi $fp, $sp, -4
	addi $sp, $sp, -4
	jal initArray
	addi $sp, $sp, 4
	lw $fp, 32($sp)
	li $s2, 5
	addi $s3, $s2, -1
	li $s2, 4
	mult $s2, $s3, $s2
	add $s0, $s0, $s2
	lw $s0, 0($s0)
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
