L160:
li t565, 10
move t555, t565
li t566, 0
bgt t555, t566, L158
ble t555, t566, L157
L157:
li t567, 0
move t101, t567
j L159
L158:
li t569, 1
addi t568, t569, 1
li t570, 0
bgt t555, t570, L158
ble t555, t570, L161
L161:
j L157
L159:
