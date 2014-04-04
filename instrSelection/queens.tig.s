L167:
add t516, t466, t466
addi t515, t516, ~1
li t517, 4
mult t514, t515, t517
move t513, t514
jal malloc
move t473, t101
add t520, t466, t466
addi t519, t520, ~1
move t518, t519
li t522, 0
move t521, t522
jal initArray
move t474, t473
add t526, t466, t466
addi t525, t526, ~1
li t527, 4
mult t524, t525, t527
move t523, t524
jal malloc
move t471, t101
add t530, t466, t466
addi t529, t530, ~1
move t528, t529
li t532, 0
move t531, t532
jal initArray
move t472, t471
li t535, 4
mult t534, t466, t535
move t533, t534
jal malloc
move t469, t101
move t536, t466
li t538, 0
move t537, t538
jal initArray
move t470, t469
li t541, 4
mult t540, t466, t541
move t539, t540
jal malloc
move t467, t101
move t542, t466
li t544, 0
move t543, t544
jal initArray
move t468, t467
li t545, 8
move t466, t545
li t547, 0
move t546, t547
jal L133
move t101, t101
j L166
L166:
L169:
beq t475, t466, L163
bne t475, t466, L164
L164:
li t548, 0
move t488, t548
L161:
addi t552, t488, ~1
li t553, 4
mult t551, t552, t553
add t550, t468, t551
lw t549, 0(t550)
li t554, 0
beq t549, t554, L149
bne t549, t554, L150
L150:
li t555, 0
move t489, t555
L151:
li t556, 0
bne t489, t556, L154
beq t489, t556, L155
L155:
li t557, 0
move t491, t557
L156:
li t558, 0
bne t491, t558, L159
beq t491, t558, L160
L160:
addi t559, t466, ~1
beq t488, t559, L148
bne t488, t559, L162
L162:
addi t560, t488, 1
j L161
L163:
jal L134
move t494, t101
L165:
move t101, t494
j L168
L149:
li t561, 1
move t490, t561
add t566, t488, t475
addi t565, t566, ~1
li t567, 4
mult t564, t565, t567
add t563, t472, t564
lw t562, 0(t563)
li t568, 0
beq t562, t568, L152
bne t562, t568, L153
L153:
li t569, 0
move t490, t569
L152:
move t489, t490
j L151
L154:
li t570, 1
move t492, t570
addi t576, t488, 7
sub t575, t576, t475
addi t574, t575, ~1
li t577, 4
mult t573, t574, t577
add t572, t474, t573
lw t571, 0(t572)
li t578, 0
beq t571, t578, L157
bne t571, t578, L158
L158:
li t579, 0
move t492, t579
L157:
move t491, t492
j L156
L159:
li t580, 1
addi t584, t488, ~1
li t585, 4
mult t583, t584, t585
add t582, t468, t583
lw t581, 0(t582)
move t581, t580
li t586, 1
add t591, t488, t475
addi t590, t591, ~1
li t592, 4
mult t589, t590, t592
add t588, t472, t589
lw t587, 0(t588)
move t587, t586
li t593, 1
addi t599, t488, 7
sub t598, t599, t475
addi t597, t598, ~1
li t600, 4
mult t596, t597, t600
add t595, t474, t596
lw t594, 0(t595)
move t594, t593
addi t604, t475, ~1
li t605, 4
mult t603, t604, t605
add t602, t470, t603
lw t601, 0(t602)
move t601, t488
addi t607, t475, 1
move t606, t607
jal L133
li t608, 0
addi t612, t488, ~1
li t613, 4
mult t611, t612, t613
add t610, t468, t611
lw t609, 0(t610)
move t609, t608
li t614, 0
add t619, t488, t475
addi t618, t619, ~1
li t620, 4
mult t617, t618, t620
add t616, t472, t617
lw t615, 0(t616)
move t615, t614
li t621, 0
addi t627, t488, 7
sub t626, t627, t475
addi t625, t626, ~1
li t628, 4
mult t624, t625, t628
add t623, t474, t624
lw t622, 0(t623)
move t622, t621
j L160
L148:
li t629, 0
move t494, t629
j L165
L168:
L171:
li t630, 0
move t476, t630
L145:
li t631, 0
move t477, t631
L142:
addi t635, t476, ~1
li t636, 4
mult t634, t635, t636
add t633, t470, t634
lw t632, 0(t633)
beq t632, t477, L139
bne t632, t477, L140
L140:
la t637, L138
move t478, t637
L141:
move t638, t478
jal print
addi t639, t466, ~1
beq t477, t639, L136
bne t477, t639, L143
L143:
addi t640, t477, 1
j L142
L139:
la t641, L137
move t478, t641
j L141
L136:
la t643, L144
move t642, t643
jal print
addi t644, t466, ~1
beq t476, t644, L135
bne t476, t644, L146
L146:
addi t645, t476, 1
j L145
L135:
la t647, L147
move t646, t647
jal print
move t101, t101
j L170
L170:
L147: 

L144: 

L138:  .
L137:  O
