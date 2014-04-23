	addi $sp, $sp, -4
	sw $s7, 0($sp)
	addi $sp, $sp, -4
	sw $s6, 0($sp)
	addi $sp, $sp, -4
	sw $s5, 0($sp)
	addi $sp, $sp, -4
	sw $s4, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
	addi $sp, $sp, -4
	sw $s0, 0($sp)
tig_main:
	sw $a0, 0($fp)
	li $s0, 5
	move $a0, $fp
	li $a1, 7
	li $a2, 8
	sw $fp, 0($sp)
	addi $fp, $sp, -4
	addi $sp, $sp, ~16
	jal L554
	addi $sp, $sp, 16
	lw $fp, 0($sp)
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	jr $ra
	lw $s0, 0($p)
	addi $sp, $sp, -4
	sw $s7, 0($sp)
	addi $sp, $sp, -4
	sw $s6, 0($sp)
	addi $sp, $sp, -4
	sw $s5, 0($sp)
	addi $sp, $sp, -4
	sw $s4, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
	addi $sp, $sp, -4
	sw $s7, 0($sp)
	addi $sp, $sp, -4
	sw $s6, 0($sp)
	addi $sp, $sp, -4
	sw $s5, 0($sp)
	addi $sp, $sp, -4
	sw $s4, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
	addi $sp, $sp, -4
	sw $s0, 0($sp)
L554:
	sw $a0, 0($fp)
	sw $a1, ~4($fp)
	sw $a2, ~8($fp)
	move $a0, $fp
	li $a1, 6
	sw $fp, 0($sp)
	addi $fp, $sp, -4
	addi $sp, $sp, ~4
	jal L555
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
	lw $s0, 0($p)
	addi $sp, $sp, -4
	sw $s7, 0($sp)
	addi $sp, $sp, -4
	sw $s6, 0($sp)
	addi $sp, $sp, -4
	sw $s5, 0($sp)
	addi $sp, $sp, -4
	sw $s4, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
	addi $sp, $sp, -4
	sw $s7, 0($sp)
	addi $sp, $sp, -4
	sw $s6, 0($sp)
	addi $sp, $sp, -4
	sw $s5, 0($sp)
	addi $sp, $sp, -4
	sw $s4, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
	addi $sp, $sp, -4
	sw $s0, 0($sp)
L555:
	sw $a0, 0($fp)
	move $s2, $a1
	lw $s0, 0($fp)
	lw $s0, ~4($s0)
	sub $s2, $s0, $s2
	lw $s0, 0($fp)
	lw $s0, ~8($s0)
	sub $s0, $s2, $s0
	move $v0, $s0
	jr $ra
	lw $s0, 0($p)
	addi $sp, $sp, -4
	sw $s7, 0($sp)
	addi $sp, $sp, -4
	sw $s6, 0($sp)
	addi $sp, $sp, -4
	sw $s5, 0($sp)
	addi $sp, $sp, -4
	sw $s4, 0($sp)
	addi $sp, $sp, -4
	sw $s3, 0($sp)
	addi $sp, $sp, -4
	sw $s2, 0($sp)
	addi $sp, $sp, -4
	sw $s1, 0($sp)
