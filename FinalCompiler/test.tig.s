L25:
	li $s0, 5
	li $a0, 7
	jal L27
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	jr $ra
L27:
	li $a0, 6
	jal L28
	jr $ra
L28:
	lw $s0, 0($fp)
	lw $s0, ~4($s0)
	add $s0, $s0, $s2
	move $v0, $s0
	jr $ra
