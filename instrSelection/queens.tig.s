L135:
add t411, t357, t357
addi t410, t411, ~1
li t412, 4
mult t409, t410, t412
move t408, t409
jal malloc
move t364, t101
add t415, t357, t357
addi t414, t415, ~1
move t413, t414
li t417, 0
move t416, t417
jal initArray
move t365, t364
add t421, t357, t357
addi t420, t421, ~1
li t422, 4
mult t419, t420, t422
move t418, t419
jal malloc
move t362, t101
add t425, t357, t357
addi t424, t425, ~1
move t423, t424
li t427, 0
move t426, t427
jal initArray
move t363, t362
li t430, 4
mult t429, t357, t430
move t428, t429
jal malloc
move t360, t101
move t431, t357
li t433, 0
move t432, t433
jal initArray
move t361, t360
li t436, 4
mult t435, t357, t436
move t434, t435
jal malloc
move t358, t101
move t437, t357
li t439, 0
move t438, t439
jal initArray
move t359, t358
li t440, 8
move t357, t440
li t442, 0
move t441, t442
jal L91
move t101, t101
j L134
L134:
L137:
beq t366, t357, L131
bne t366, t357, L132
L132:
li t443, 0
move t379, t443
L129:
addi t447, t379, ~1
li t448, 4
mult t446, t447, t448
add t445, t359, t446
lw t444, 0(t445)
li t449, 0
beq t444, t449, L107
bne t444, t449, L108
L108:
li t450, 0
move t380, t450
L109:
li t451, 0
bne t380, t451, L112
beq t380, t451, L113
L113:
li t452, 0
move t382, t452
L114:
li t453, 0
bne t382, t453, L127
beq t382, t453, L128
L128:
addi t454, t357, ~1
beq t379, t454, L106
bne t379, t454, L130
L130:
addi t455, t379, 1
j L129
L131:
jal L92
move t389, t101
L133:
move t101, t389
j L136
L107:
li t456, 1
move t381, t456
add t461, t379, t366
addi t460, t461, ~1
li t462, 4
mult t459, t460, t462
add t458, t363, t459
lw t457, 0(t458)
li t463, 0
beq t457, t463, L110
bne t457, t463, L111
L111:
li t464, 0
move t381, t464
L110:
move t380, t381
j L109
L112:
li t465, 1
move t383, t465
addi t471, t379, 7
sub t470, t471, t366
addi t469, t470, ~1
li t472, 4
mult t468, t469, t472
add t467, t365, t468
lw t466, 0(t467)
li t473, 0
beq t466, t473, L115
bne t466, t473, L116
L116:
li t474, 0
move t383, t474
L115:
move t382, t383
j L114
L127:
addi t478, t379, ~1
li t479, 4
mult t477, t478, t479
add t476, t359, t477
lw t475, 0(t476)
li t480, 0
beq t475, t480, L117
bne t475, t480, L118
L118:
li t481, 0
move t384, t481
L119:
li t482, 0
bne t384, t482, L122
beq t384, t482, L123
L123:
li t483, 0
move t386, t483
L124:
j L128
L117:
li t484, 1
move t385, t484
add t489, t379, t366
addi t488, t489, ~1
li t490, 4
mult t487, t488, t490
add t486, t363, t487
lw t485, 0(t486)
li t491, 0
beq t485, t491, L120
bne t485, t491, L121
L121:
li t492, 0
move t385, t492
L120:
move t384, t385
j L119
L122:
li t493, 1
move t387, t493
addi t499, t379, 7
sub t498, t499, t366
addi t497, t498, ~1
li t500, 4
mult t496, t497, t500
add t495, t365, t496
lw t494, 0(t495)
li t501, 0
beq t494, t501, L125
bne t494, t501, L126
L126:
li t502, 0
move t387, t502
L125:
move t386, t387
j L124
L106:
li t503, 0
move t389, t503
j L133
L136:
L139:
li t504, 0
move t367, t504
L103:
li t505, 0
move t368, t505
L100:
addi t509, t367, ~1
li t510, 4
mult t508, t509, t510
add t507, t361, t508
lw t506, 0(t507)
beq t506, t368, L97
bne t506, t368, L98
L98:
la t511, L96
move t369, t511
L99:
move t512, t369
jal print
addi t513, t357, ~1
beq t368, t513, L94
bne t368, t513, L101
L101:
addi t514, t368, 1
j L100
L97:
la t515, L95
move t369, t515
j L99
L94:
la t517, L102
move t516, t517
jal print
addi t518, t357, ~1
beq t367, t518, L93
bne t367, t518, L104
L104:
addi t519, t367, 1
j L103
L93:
la t521, L105
move t520, t521
jal print
move t101, t101
j L138
L138:
L105: 

L102: 

L96:  .
L95:  O
