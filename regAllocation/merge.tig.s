L322:
jal getchar
move $s0, $v0
jal L291
move $s2, $v0
jal getchar
move $s0, $v0
jal L291
move $s0, $v0
move $a0, $s2
move $a1, $s0
jal L290
move $s0, $v0
move $a0, $s0
jal L288
j L321
L321:
L340:
li $s0, 0
beq $s2, $s0, L318
bne $s2, $s0, L319
L319:
lw $s0, 0($s2)
move $a0, $s0
jal L289
la $s0, L317
move $a0, $s0
jal print
lw $s0, 4($s2)
move $a0, $s0
jal L288
move $s0, $v0
L320:
move $v0, $s0
j L339
L318:
la $s0, L316
move $a0, $s0
jal print
move $s0, $v0
j L320
L339:
L317:  
L316: 

L361:
li $s0, 0
blt $s2, $s0, L313
bge $s2, $s0, L314
L314:
li $s0, 0
bgt $s2, $s0, L310
ble $s2, $s0, L311
L311:
la $s0, L309
move $a0, $s0
jal print
move $s0, $v0
L312:
L315:
move $v0, $s0
j L360
L313:
la $s0, L308
move $a0, $s0
jal print
li $s0, 0
sub $s0, $s0, $s2
move $a0, $s0
jal L304
move $s0, $v0
j L315
L310:
move $a0, $s2
jal L304
move $s0, $v0
j L312
L360:
L309: 0
L308: -
L387:
li $s0, 0
bgt $s3, $s0, L306
ble $s3, $s0, L307
L307:
li $v0, 0
j L386
L306:
li $s0, 10
div $s0, $s3, $s0
move $a0, $s0
jal L304
li $s0, 10
div $s2, $s3, $s0
li $s0, 10
mult $s0, $s2, $s0
sub $s0, $s3, $s0
la $s2, L305
move $a0, $s2
jal ord
move $s2, $v0
add $s0, $s0, $s2
move $a0, $s0
jal chr
move $s0, $v0
move $a0, $s0
jal print
j L307
L386:
L305: 0
L414:
li $s0, 0
beq $s4, $s0, L301
bne $s4, $s0, L302
L302:
li $s0, 0
beq $s5, $s0, L298
bne $s5, $s0, L299
L299:
lw $s2, 0($s4)
lw $s0, 0($s5)
blt $s2, $s0, L295
bge $s2, $s0, L296
L296:
li $a0, 8
jal malloc
move $s3, $v0
lw $s0, 0($s5)
sw $s0, 0($s3)
addi $s0, $s3, 4
move $a0, $s4
lw $s2, 4($s5)
move $a1, $s2
jal L290
move $s2, $v0
lw $s2, 0($s0)
L297:
move $s0, $s3
L300:
L303:
move $v0, $s0
j L413
L301:
move $s0, $s5
j L303
L298:
move $s0, $s4
j L300
L295:
li $a0, 8
jal malloc
move $s3, $v0
lw $s0, 0($s4)
sw $s0, 0($s3)
addi $s0, $s3, 4
lw $s2, 4($s4)
move $a0, $s2
move $a1, $s5
jal L290
move $s2, $v0
lw $s2, 0($s0)
j L297
L413:
L462:
li $a0, 4
jal malloc
move $s0, $v0
li $s2, 0
sw $s2, 0($s0)
move $a0, $s0
jal L268
move $s4, $v0
lw $s2, 0($s0)
li $s0, 0
bne $s2, $s0, L292
beq $s2, $s0, L293
L293:
li $s3, 0
L294:
move $v0, $s3
j L461
L292:
li $a0, 8
jal malloc
move $s3, $v0
sw $s4, 0($s3)
addi $s0, $s3, 4
jal L291
move $s2, $v0
lw $s2, 0($s0)
j L294
L461:
L491:
li $s0, 0
jal L269
addi $s2, $s2, 0
move $a0, $s4
jal L270
move $s3, $v0
lw $s3, 0($s2)
move $a0, $s4
jal L270
move $s3, $v0
li $s2, 0
bne $s3, $s2, L287
beq $s3, $s2, L285
L285:
move $v0, $s0
j L490
L287:
li $s2, 10
mult $s0, $s0, $s2
move $a0, $s4
jal ord
move $s2, $v0
add $s0, $s0, $s2
la $s2, L286
move $a0, $s2
jal ord
move $s2, $v0
sub $s0, $s0, $s2
jal getchar
move $s4, $v0
move $a0, $s4
jal L270
move $s3, $v0
li $s2, 0
bne $s3, $s2, L287
beq $s3, $s2, L492
L492:
j L285
L490:
L286: 0
L532:
move $a0, $s3
la $s0, L279
move $a1, $s0
jal stringEqual
move $s2, $v0
li $s0, 0
bne $s2, $s0, L281
beq $s2, $s0, L282
L282:
move $a0, $s3
la $s0, L280
move $a1, $s0
jal stringEqual
move $s2, $v0
L283:
li $s0, 0
bne $s2, $s0, L284
beq $s2, $s0, L278
L278:
li $v0, 0
j L531
L281:
li $s2, 1
j L283
L284:
jal getchar
move $s3, $v0
move $a0, $s3
la $s0, L279
move $a1, $s0
jal stringEqual
move $s2, $v0
li $s0, 0
bne $s2, $s0, L281
beq $s2, $s0, L533
L533:
j L282
L531:
L280: 

L279:  
L563:
move $a0, $s4
jal ord
move $s0, $v0
la $s2, L271
move $a0, $s2
jal ord
move $s2, $v0
bge $s0, $s2, L273
blt $s0, $s2, L274
L274:
li $s2, 0
L275:
move $v0, $s2
j L562
L273:
li $s2, 1
move $a0, $s4
jal ord
move $s0, $v0
la $s3, L272
move $a0, $s3
jal ord
move $s3, $v0
ble $s0, $s3, L276
bgt $s0, $s3, L277
L277:
li $s2, 0
L276:
j L275
L562:
L272: 9
L271: 0
