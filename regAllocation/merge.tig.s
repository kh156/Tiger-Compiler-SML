L112:
jal getchar
move t225, t101
jal L81
move t324, t101
jal L81
move t323, t101
jal getchar
move t225, t101
move t335, t323
move t336, t324
jal L80
move t334, t101
move t337, t334
jal L78
move t101, t101
j L111
L111:
L114:
li t338, 0
beq t259, t338, L108
bne t259, t338, L109
L109:
lw t340, 0(t259)
move t339, t340
jal L79
la t342, L107
move t341, t342
jal print
lw t344, 4(t259)
move t343, t344
jal L78
move t313, t101
L110:
move t101, t313
j L113
L108:
la t346, L106
move t345, t346
jal print
move t313, t101
j L110
L113:
L107:  
L106: 

L116:
li t347, 0
blt t260, t347, L103
bge t260, t347, L104
L104:
li t348, 0
bgt t260, t348, L100
ble t260, t348, L101
L101:
la t350, L99
move t349, t350
jal print
move t302, t101
L102:
move t303, t302
L105:
move t101, t303
j L115
L103:
la t352, L98
move t351, t352
jal print
li t355, 0
sub t354, t355, t260
move t353, t354
jal L94
move t303, t101
j L105
L100:
move t356, t260
jal L94
move t302, t101
j L102
L115:
L99: 0
L98: -
L118:
li t360, 0
bgt t291, t360, L96
ble t291, t360, L97
L97:
li t361, 0
move t101, t361
j L117
L96:
li t364, 10
div t363, t291, t364
move t362, t363
jal L94
li t368, 10
div t367, t291, t368
li t369, 10
mult t366, t367, t369
sub t365, t291, t366
move t359, t365
la t371, L95
move t370, t371
jal ord
move t358, t101
add t373, t359, t358
move t372, t373
jal chr
move t357, t101
move t374, t357
jal print
j L97
L117:
L95: 0
L120:
li t379, 0
beq t262, t379, L91
bne t262, t379, L92
L92:
li t380, 0
beq t261, t380, L88
bne t261, t380, L89
L89:
lw t381, 0(t262)
lw t382, 0(t261)
blt t381, t382, L85
bge t381, t382, L86
L86:
li t384, 8
move t383, t384
jal malloc
move t278, t101
lw t385, 0(t261)
sw t385, 0(t278)
addi t386, t278, 4
move t378, t386
move t387, t262
lw t389, 4(t261)
move t388, t389
jal L80
move t377, t101
lw t390, 0(t378)
move t390, t377
move t279, t278
L87:
move t280, t279
L90:
move t281, t280
L93:
move t101, t281
j L119
L91:
move t281, t261
j L93
L88:
move t280, t262
j L90
L85:
li t392, 8
move t391, t392
jal malloc
move t277, t101
lw t393, 0(t262)
sw t393, 0(t277)
addi t394, t277, 4
move t376, t394
lw t396, 4(t262)
move t395, t396
move t397, t261
jal L80
move t375, t101
lw t398, 0(t376)
move t398, t375
move t279, t277
j L87
L119:
L122:
move t401, t264
jal L58
move t265, t101
li t403, 4
move t402, t403
jal malloc
move t263, t101
li t404, 0
sw t404, 0(t263)
move t264, t263
lw t405, 0(t264)
li t406, 0
bne t405, t406, L82
beq t405, t406, L83
L83:
li t407, 0
move t267, t407
L84:
move t101, t267
j L121
L82:
li t409, 8
move t408, t409
jal malloc
move t266, t101
sw t265, 0(t266)
addi t410, t266, 4
move t400, t410
jal L81
move t399, t101
lw t411, 0(t400)
move t411, t399
move t267, t266
j L84
L121:
L124:
li t420, 0
move t227, t420
jal L59
addi t421, t226, 0
move t413, t421
move t422, t225
jal L60
move t412, t101
lw t423, 0(t413)
move t423, t412
move t424, t225
jal L60
move t414, t101
li t425, 0
bne t414, t425, L77
beq t414, t425, L75
L75:
move t101, t227
j L123
L77:
li t427, 10
mult t426, t227, t427
move t416, t426
move t428, t225
jal ord
move t415, t101
add t429, t416, t415
move t418, t429
la t431, L76
move t430, t431
jal ord
move t417, t101
sub t432, t418, t417
move t227, t432
jal getchar
move t225, t101
move t433, t225
jal L60
move t419, t101
li t434, 0
bne t419, t434, L77
beq t419, t434, L125
L125:
j L75
L123:
L76: 0
L127:
move t437, t225
la t439, L69
move t438, t439
jal stringEqual
move t435, t101
li t440, 0
bne t435, t440, L71
beq t435, t440, L72
L72:
move t441, t225
la t443, L70
move t442, t443
jal stringEqual
move t240, t101
L73:
li t444, 0
bne t240, t444, L74
beq t240, t444, L68
L68:
li t445, 0
move t101, t445
j L126
L71:
li t446, 1
move t240, t446
j L73
L74:
jal getchar
move t225, t101
move t447, t225
la t449, L69
move t448, t449
jal stringEqual
move t436, t101
li t450, 0
bne t436, t450, L71
beq t436, t450, L128
L128:
j L72
L126:
L70: 

L69:  
L130:
move t457, t225
jal ord
move t451, t101
move t453, t451
la t459, L61
move t458, t459
jal ord
move t452, t101
bge t453, t452, L63
blt t453, t452, L64
L64:
li t460, 0
move t229, t460
L65:
move t101, t229
j L129
L63:
li t461, 1
move t230, t461
move t462, t225
jal ord
move t454, t101
move t456, t454
la t464, L62
move t463, t464
jal ord
move t455, t101
ble t456, t455, L66
bgt t456, t455, L67
L67:
li t465, 0
move t230, t465
L66:
move t229, t230
j L65
L129:
L62: 9
L61: 0
