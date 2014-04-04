L167:
li t581, 10
move t571, t581
li t582, 0
bgt t571, t582, L165
ble t571, t582, L164
L164:
li t583, 0
move t101, t583
j L166
L165:
li t585, 1
addi t584, t585, 1
li t586, 0
bgt t571, t586, L165
ble t571, t586, L168
L168:
j L164
L166:
