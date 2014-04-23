L1:
	sw $a0, 0($fp)
	li $s0, 5
	move $a0, $fp
	li $a1, 7
	li $a2, 8
	jal L3
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	jr $ra
L3:
	sw $a0, 0($fp)
	sw $a1, ~4($fp)
	sw $a2, ~8($fp)
	move $a0, $fp
	li $a1, 6
	jal L4
	jr $ra
L4:
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
