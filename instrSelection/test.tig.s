L69:
li t294, 10
li t295, 4
mult t293, t294, t295
move t292, t293
jal malloc
move t280, t291
li t298, 10
move t297, t298
li t300, 0
move t299, t300
jal initArray
move t281, t280
li t305, 2
addi t304, t305, ~1
li t306, 4
mult t303, t304, t306
add t302, t281, t303
lw t301, 0(t302)
move t101, t301
j L68
L68:
