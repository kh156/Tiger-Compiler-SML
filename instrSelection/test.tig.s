L261:
li t972, 5
jal L256
move t101, t101
j L260
L260:
L263:
li t974, 0
bgt t952, t974, L257
ble t952, t974, L258
L258:
li t953, 99
L259:
move t101, t953
j L262
L257:
addi t976, t952, ~1
move t975, t976
jal L256
move t973, t101
addi t977, t973, 1
move t953, t977
j L259
L262:
