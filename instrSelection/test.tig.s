L241:
li t919, 5
move t918, t919
jal L236
move t101, t101
j L240
L240:
L243:
li t921, 0
bgt t898, t921, L237
ble t898, t921, L238
L238:
li t922, 99
move t899, t922
L239:
move t101, t899
j L242
L237:
addi t924, t898, ~1
move t923, t924
jal L236
move t920, t101
addi t925, t920, 1
move t899, t925
j L239
L242:
