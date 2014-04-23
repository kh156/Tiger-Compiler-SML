L455:
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
	jal L457
	jr $ra
L457:
	beq $s3, $t0, L487
	bne $s3, $t0, L488
L488:
	li $s2, 0
L485:
	addi $t1, $s2, ~1
	li $s0, 4
	mult $s0, $t1, $s0
	add $s0, $s7, $s0
	lw $t1, 0($s0)
	li $s0, 0
	beq $t1, $s0, L473
	bne $t1, $s0, L474
L474:
	li $s0, 0
L475:
	li $t1, 0
	bne $s0, $t1, L478
	beq $s0, $t1, L479
L479:
	li $s0, 0
L480:
	li $t1, 0
	bne $s0, $t1, L483
	beq $s0, $t1, L484
L484:
	addi $s0, $t0, ~1
	beq $s2, $s0, L472
	bne $s2, $s0, L486
L486:
	addi $s0, $s2, 1
	j L485
L487:
	jal L458
	move $s0, $v0
L489:
	move $v0, $s0
	jr $ra
L473:
	li $s0, 1
	add $t1, $s2, $s3
	addi $t2, $t1, ~1
	li $t1, 4
	mult $t1, $t2, $t1
	add $t1, $s5, $t1
	lw $t2, 0($t1)
	li $t1, 0
	beq $t2, $t1, L476
	bne $t2, $t1, L477
L477:
	li $s0, 0
L476:
	j L475
L478:
	li $s0, 1
	addi $t1, $s2, 7
	sub $t1, $t1, $s3
	addi $t2, $t1, ~1
	li $t1, 4
	mult $t1, $t2, $t1
	add $t1, $s4, $t1
	lw $t2, 0($t1)
	li $t1, 0
	beq $t2, $t1, L481
	bne $t2, $t1, L482
L482:
	li $s0, 0
L481:
	j L480
L483:
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
	jal L457
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
	j L484
L472:
	li $s0, 0
	j L489
L458:
	li $s4, 0
L469:
	li $s3, 0
L466:
	addi $s2, $s4, ~1
	li $s0, 4
	mult $s0, $s2, $s0
	add $s0, $s5, $s0
	lw $s0, 0($s0)
	beq $s0, $s3, L463
	bne $s0, $s3, L464
L464:
	la $s0, L462
L465:
	move $a0, $s0
	jal print
	addi $s0, $s6, ~1
	beq $s3, $s0, L460
	bne $s3, $s0, L467
L467:
	addi $s0, $s3, 1
	j L466
L463:
	la $s0, L461
	j L465
L460:
	la $s0, L468
	move $a0, $s0
	jal print
	addi $s0, $s6, ~1
	beq $s4, $s0, L459
	bne $s4, $s0, L470
L470:
	addi $s0, $s4, 1
	j L469
L459:
	la $s0, L471
	move $a0, $s0
	jal print
	jr $ra
L471:
	.word 1
	.ascii "\n"
L468:
	.word 1
	.ascii "\n"
L462:
	.word 2
	.ascii " ."
L461:
	.word 2
	.ascii " O"
