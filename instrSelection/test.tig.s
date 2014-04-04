L10:
li t105, 6
sw t105, ~12(t100)
li t106, 5
sw t106, ~8(t100)
lw t108, ~8(t100)
lw t109, ~12(t100)
add t107, t108, 's1
move t101, t107
j L9
L9:
