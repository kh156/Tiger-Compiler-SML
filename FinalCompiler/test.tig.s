L131:
	li $s0, 5
	li $a0, 7
	jal L133
	move $s2, $v0
	add $s0, $s0, $s2
	move $v0, $s0
	jr $ra
L133:
	addi $s0, $s0, 6
	move $v0, $s0
	jr $ra
