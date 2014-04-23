tig_main:
	sw $ra, -4($fp)
	sw $s7, -36($fp)
	sw $s6, -32($fp)
	sw $s5, -28($fp)
	sw $s4, -24($fp)
	sw $s3, -20($fp)
	sw $s2, -16($fp)
	sw $s1, -12($fp)
	sw $s0, -8($fp)
	sw $a0, 0($fp)
	li $s0, 5
	move $a0, $fp
	li $a1, 7
	li $a2, 8
	li $a3, 9
	li $s2, 0
	addi $s2, $s2, -10
	sw $s2, 4($sp)
	li $s2, 0
	addi $s2, $s2, -11
	sw $s2, 8($sp)
	sw $fp, 0($sp)
	addi $fp, $sp, -4
	addi $sp, $sp, -40
	jal L298
	addi $sp, $sp, 40
	lw $fp, 0($sp)
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	lw $s0, -8($fp)
	lw $s1, -12($fp)
	lw $s2, -16($fp)
	lw $s3, -20($fp)
	lw $s4, -24($fp)
	lw $s5, -28($fp)
	lw $s6, -32($fp)
	lw $s7, -36($fp)
	lw $ra, -4($fp)
	jr $ra
L298:
	sw $ra, -4($fp)
	sw $s7, -36($fp)
	sw $s6, -32($fp)
	sw $s5, -28($fp)
	sw $s4, -24($fp)
	sw $s3, -20($fp)
	sw $s2, -16($fp)
	sw $s1, -12($fp)
	sw $s0, -8($fp)
	sw $a0, 0($fp)
	move $s5, $a1
	move $s3, $a2
	move $s4, $a3
	lw $s2, 8($fp)
	lw $s0, 12($fp)
	mult $s3, $s5, $s3
	mult $s2, $s4, $s2
	sub $s2, $s3, $s2
	sub $s0, $s2, $s0
	move $v0, $s0
	lw $s0, -8($fp)
	lw $s1, -12($fp)
	lw $s2, -16($fp)
	lw $s3, -20($fp)
	lw $s4, -24($fp)
	lw $s5, -28($fp)
	lw $s6, -32($fp)
	lw $s7, -36($fp)
	lw $ra, -4($fp)
	jr $ra
