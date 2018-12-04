module Day3.Input exposing (input, testInput)


testInput =
    """#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2"""


input =
    """#1 @ 896,863: 29x19
#2 @ 367,315: 15x26
#3 @ 53,233: 21x25
#4 @ 581,238: 13x24
#5 @ 677,75: 17x17
#6 @ 130,597: 21x12
#7 @ 362,788: 29x11
#8 @ 139,784: 27x12
#9 @ 18,968: 16x25
#10 @ 690,473: 10x15
#11 @ 791,508: 26x24
#12 @ 648,35: 12x26
#13 @ 30,697: 14x17
#14 @ 726,670: 24x16
#15 @ 70,418: 11x23
#16 @ 115,456: 17x16
#17 @ 734,678: 21x22
#18 @ 457,627: 22x18
#19 @ 793,835: 10x24
#20 @ 70,440: 26x29
#21 @ 756,515: 25x21
#22 @ 151,570: 16x18
#23 @ 297,592: 24x16
#24 @ 432,339: 24x22
#25 @ 670,546: 22x21
#26 @ 60,565: 29x25
#27 @ 694,589: 22x27
#28 @ 480,479: 13x15
#29 @ 81,130: 17x12
#30 @ 614,647: 24x11
#31 @ 740,23: 10x13
#32 @ 191,587: 18x22
#33 @ 448,31: 16x16
#34 @ 684,346: 15x10
#35 @ 681,534: 24x21
#36 @ 888,137: 14x16
#37 @ 224,25: 13x27
#38 @ 359,251: 21x17
#39 @ 276,182: 18x22
#40 @ 768,188: 22x23
#41 @ 888,352: 17x12
#42 @ 131,173: 15x15
#43 @ 783,698: 17x15
#44 @ 377,583: 14x11
#45 @ 425,693: 14x10
#46 @ 794,59: 14x29
#47 @ 78,760: 12x21
#48 @ 50,425: 28x29
#49 @ 782,522: 18x23
#50 @ 112,264: 10x11
#51 @ 549,150: 15x29
#52 @ 594,375: 26x20
#53 @ 141,701: 16x19
#54 @ 295,433: 15x15
#55 @ 448,208: 19x26
#56 @ 113,183: 25x10
#57 @ 795,133: 22x29
#58 @ 376,382: 26x21
#59 @ 662,576: 20x18
#60 @ 885,342: 13x17
#61 @ 120,658: 16x14
#62 @ 657,906: 23x16
#63 @ 724,922: 22x28
#64 @ 807,75: 15x26
#65 @ 601,61: 22x19
#66 @ 851,812: 26x18
#67 @ 248,839: 5x6
#68 @ 889,105: 27x19
#69 @ 809,622: 28x27
#70 @ 521,595: 11x13
#71 @ 295,469: 23x21
#72 @ 213,7: 27x21
#73 @ 571,484: 26x19
#74 @ 327,835: 25x28
#75 @ 454,119: 20x24
#76 @ 400,792: 25x19
#77 @ 456,134: 28x14
#78 @ 191,527: 11x12
#79 @ 585,12: 27x11
#80 @ 920,483: 17x28
#81 @ 498,353: 26x20
#82 @ 680,920: 10x10
#83 @ 704,722: 14x19
#84 @ 47,510: 29x11
#85 @ 109,693: 19x23
#86 @ 556,151: 22x11
#87 @ 836,630: 13x16
#88 @ 759,565: 22x19
#89 @ 475,195: 19x15
#90 @ 894,606: 21x16
#91 @ 985,464: 12x12
#92 @ 882,103: 18x19
#93 @ 784,248: 21x15
#94 @ 374,803: 15x10
#95 @ 656,186: 11x3
#96 @ 92,819: 29x23
#97 @ 989,400: 10x18
#98 @ 133,691: 22x24
#99 @ 731,59: 17x12
#100 @ 207,430: 22x24
#101 @ 732,831: 24x10
#102 @ 326,888: 24x13
#103 @ 815,189: 18x27
#104 @ 213,285: 29x16
#105 @ 114,762: 13x27
#106 @ 267,903: 26x26
#107 @ 897,131: 27x25
#108 @ 465,821: 12x11
#109 @ 643,638: 22x28
#110 @ 560,858: 21x17
#111 @ 13,925: 29x18
#112 @ 334,780: 29x26
#113 @ 254,648: 18x10
#114 @ 71,440: 23x24
#115 @ 435,602: 23x27
#116 @ 581,279: 14x21
#117 @ 63,76: 29x13
#118 @ 193,942: 25x25
#119 @ 811,382: 13x21
#120 @ 773,696: 15x23
#121 @ 569,301: 10x13
#122 @ 334,938: 26x27
#123 @ 368,591: 14x10
#124 @ 107,872: 29x21
#125 @ 713,589: 29x27
#126 @ 628,418: 28x29
#127 @ 911,161: 12x17
#128 @ 924,357: 16x13
#129 @ 149,132: 27x17
#130 @ 624,318: 27x27
#131 @ 241,839: 14x12
#132 @ 661,691: 24x19
#133 @ 468,238: 10x14
#134 @ 440,140: 21x24
#135 @ 334,571: 19x23
#136 @ 854,299: 27x12
#137 @ 964,833: 4x11
#138 @ 696,103: 22x18
#139 @ 568,219: 14x26
#140 @ 119,63: 28x22
#141 @ 751,11: 27x11
#142 @ 629,942: 18x11
#143 @ 333,9: 24x24
#144 @ 629,178: 26x15
#145 @ 669,893: 26x16
#146 @ 817,862: 22x19
#147 @ 295,732: 15x21
#148 @ 189,400: 27x10
#149 @ 911,595: 25x26
#150 @ 694,200: 23x24
#151 @ 449,844: 28x24
#152 @ 543,202: 27x18
#153 @ 580,479: 23x21
#154 @ 326,6: 24x25
#155 @ 474,821: 26x21
#156 @ 83,639: 25x28
#157 @ 20,26: 24x14
#158 @ 440,24: 12x15
#159 @ 218,246: 23x13
#160 @ 959,159: 16x28
#161 @ 68,51: 16x14
#162 @ 978,418: 13x11
#163 @ 56,380: 12x24
#164 @ 928,315: 12x14
#165 @ 159,522: 23x16
#166 @ 96,50: 26x17
#167 @ 363,363: 23x25
#168 @ 382,798: 11x17
#169 @ 255,171: 19x22
#170 @ 72,878: 22x16
#171 @ 819,590: 13x15
#172 @ 16,889: 13x12
#173 @ 200,346: 12x14
#174 @ 679,360: 19x15
#175 @ 65,12: 10x11
#176 @ 845,265: 17x26
#177 @ 582,512: 20x28
#178 @ 226,261: 26x10
#179 @ 562,507: 21x25
#180 @ 868,208: 26x17
#181 @ 269,248: 28x22
#182 @ 848,486: 24x26
#183 @ 550,490: 21x18
#184 @ 389,760: 14x26
#185 @ 507,244: 24x15
#186 @ 707,733: 13x22
#187 @ 174,839: 24x20
#188 @ 774,571: 26x15
#189 @ 780,561: 16x21
#190 @ 695,911: 24x20
#191 @ 525,137: 14x25
#192 @ 383,800: 23x10
#193 @ 470,841: 16x29
#194 @ 703,803: 14x21
#195 @ 15,961: 21x10
#196 @ 224,940: 18x14
#197 @ 896,14: 16x12
#198 @ 843,506: 28x28
#199 @ 433,928: 26x28
#200 @ 399,107: 26x21
#201 @ 293,562: 14x25
#202 @ 870,472: 18x15
#203 @ 15,604: 22x14
#204 @ 767,422: 17x22
#205 @ 912,113: 17x15
#206 @ 284,778: 14x26
#207 @ 103,137: 17x21
#208 @ 31,118: 14x21
#209 @ 565,26: 13x14
#210 @ 571,741: 20x12
#211 @ 258,190: 17x28
#212 @ 921,242: 18x10
#213 @ 502,495: 27x26
#214 @ 215,370: 16x14
#215 @ 596,516: 19x15
#216 @ 416,617: 17x10
#217 @ 310,37: 29x20
#218 @ 530,929: 21x15
#219 @ 825,807: 28x26
#220 @ 426,27: 19x10
#221 @ 460,57: 18x27
#222 @ 672,194: 23x22
#223 @ 289,31: 22x28
#224 @ 313,185: 15x26
#225 @ 337,213: 17x22
#226 @ 583,318: 12x16
#227 @ 650,765: 22x22
#228 @ 896,74: 12x20
#229 @ 686,893: 17x15
#230 @ 759,529: 21x12
#231 @ 526,252: 15x12
#232 @ 27,267: 19x23
#233 @ 104,37: 19x19
#234 @ 938,62: 10x15
#235 @ 676,681: 24x13
#236 @ 749,212: 20x14
#237 @ 692,652: 5x7
#238 @ 130,329: 21x16
#239 @ 629,818: 16x23
#240 @ 315,66: 18x21
#241 @ 173,618: 12x10
#242 @ 578,648: 18x23
#243 @ 758,656: 16x25
#244 @ 628,956: 20x17
#245 @ 342,933: 26x29
#246 @ 312,153: 18x16
#247 @ 430,271: 21x28
#248 @ 972,791: 15x22
#249 @ 484,649: 20x11
#250 @ 255,888: 17x20
#251 @ 723,185: 25x29
#252 @ 372,395: 10x16
#253 @ 703,898: 25x16
#254 @ 678,935: 25x16
#255 @ 528,75: 19x25
#256 @ 448,871: 16x15
#257 @ 850,930: 19x23
#258 @ 183,587: 27x29
#259 @ 918,243: 20x28
#260 @ 418,358: 28x12
#261 @ 482,834: 11x27
#262 @ 441,317: 12x27
#263 @ 537,685: 22x10
#264 @ 691,522: 27x28
#265 @ 40,525: 12x26
#266 @ 558,846: 23x13
#267 @ 366,431: 5x8
#268 @ 922,241: 14x27
#269 @ 33,673: 20x14
#270 @ 669,72: 24x27
#271 @ 867,140: 13x14
#272 @ 201,547: 13x14
#273 @ 528,575: 12x21
#274 @ 676,651: 28x21
#275 @ 421,460: 15x16
#276 @ 788,528: 11x17
#277 @ 579,18: 12x15
#278 @ 798,460: 16x29
#279 @ 921,550: 21x16
#280 @ 533,37: 26x10
#281 @ 916,711: 19x10
#282 @ 435,124: 15x11
#283 @ 448,439: 16x28
#284 @ 59,566: 13x28
#285 @ 139,964: 19x16
#286 @ 759,959: 12x29
#287 @ 563,498: 10x22
#288 @ 510,453: 6x17
#289 @ 372,830: 11x14
#290 @ 289,567: 20x17
#291 @ 737,571: 17x13
#292 @ 631,382: 26x12
#293 @ 395,913: 16x28
#294 @ 569,619: 21x16
#295 @ 535,22: 10x12
#296 @ 531,547: 17x16
#297 @ 761,667: 23x17
#298 @ 554,832: 15x27
#299 @ 298,427: 27x18
#300 @ 238,573: 27x14
#301 @ 55,717: 15x29
#302 @ 559,546: 25x13
#303 @ 32,136: 12x13
#304 @ 623,474: 17x16
#305 @ 695,788: 18x22
#306 @ 349,857: 20x23
#307 @ 339,360: 26x25
#308 @ 437,514: 28x20
#309 @ 305,722: 22x27
#310 @ 67,64: 29x28
#311 @ 285,110: 25x18
#312 @ 218,468: 25x22
#313 @ 274,460: 10x14
#314 @ 494,624: 19x25
#315 @ 737,854: 19x27
#316 @ 260,207: 25x23
#317 @ 481,677: 10x19
#318 @ 771,539: 19x20
#319 @ 523,39: 23x21
#320 @ 12,701: 13x13
#321 @ 746,112: 29x14
#322 @ 755,615: 27x26
#323 @ 364,834: 18x22
#324 @ 9,305: 15x20
#325 @ 620,955: 26x15
#326 @ 33,848: 21x20
#327 @ 903,203: 22x16
#328 @ 337,765: 29x26
#329 @ 467,479: 18x18
#330 @ 491,64: 10x16
#331 @ 847,558: 26x15
#332 @ 958,28: 15x16
#333 @ 398,839: 21x20
#334 @ 210,624: 18x12
#335 @ 209,618: 14x23
#336 @ 762,627: 18x23
#337 @ 406,114: 13x6
#338 @ 508,447: 11x28
#339 @ 477,459: 18x20
#340 @ 860,874: 26x29
#341 @ 32,202: 23x29
#342 @ 185,249: 20x22
#343 @ 232,665: 19x22
#344 @ 635,533: 17x24
#345 @ 14,348: 19x25
#346 @ 872,423: 26x12
#347 @ 377,705: 27x12
#348 @ 458,239: 18x22
#349 @ 182,61: 14x15
#350 @ 156,30: 16x26
#351 @ 862,494: 26x25
#352 @ 688,890: 21x22
#353 @ 311,909: 19x22
#354 @ 345,931: 19x14
#355 @ 813,864: 10x24
#356 @ 408,41: 17x19
#357 @ 584,278: 26x13
#358 @ 860,308: 10x25
#359 @ 824,815: 22x25
#360 @ 594,290: 20x29
#361 @ 621,527: 14x28
#362 @ 910,2: 25x16
#363 @ 118,44: 17x27
#364 @ 652,183: 26x12
#365 @ 857,940: 24x27
#366 @ 762,601: 28x29
#367 @ 868,629: 21x19
#368 @ 395,521: 12x18
#369 @ 684,187: 20x18
#370 @ 783,279: 20x27
#371 @ 310,55: 27x29
#372 @ 823,579: 14x12
#373 @ 960,13: 18x20
#374 @ 706,207: 11x11
#375 @ 477,362: 24x20
#376 @ 466,334: 13x19
#377 @ 309,553: 22x29
#378 @ 200,305: 15x25
#379 @ 559,198: 15x24
#380 @ 284,469: 24x24
#381 @ 238,184: 25x25
#382 @ 351,212: 22x12
#383 @ 400,157: 17x10
#384 @ 0,485: 19x16
#385 @ 591,221: 25x25
#386 @ 755,497: 22x15
#387 @ 326,746: 29x29
#388 @ 190,841: 17x16
#389 @ 207,321: 19x27
#390 @ 777,54: 22x15
#391 @ 746,31: 29x16
#392 @ 454,211: 13x19
#393 @ 302,909: 21x13
#394 @ 421,207: 16x22
#395 @ 500,613: 13x16
#396 @ 650,820: 28x28
#397 @ 549,674: 26x19
#398 @ 68,771: 20x12
#399 @ 981,507: 18x25
#400 @ 688,531: 14x13
#401 @ 721,843: 23x25
#402 @ 890,854: 15x12
#403 @ 801,835: 15x13
#404 @ 768,635: 23x26
#405 @ 665,63: 13x18
#406 @ 384,338: 26x26
#407 @ 308,207: 22x17
#408 @ 438,208: 27x29
#409 @ 119,758: 13x10
#410 @ 425,110: 27x26
#411 @ 681,585: 19x21
#412 @ 316,918: 27x15
#413 @ 827,719: 26x20
#414 @ 771,525: 21x26
#415 @ 312,381: 26x12
#416 @ 111,262: 15x29
#417 @ 703,322: 15x16
#418 @ 51,852: 16x24
#419 @ 651,33: 17x11
#420 @ 238,965: 23x18
#421 @ 849,86: 13x13
#422 @ 926,364: 17x25
#423 @ 583,914: 27x16
#424 @ 582,48: 22x13
#425 @ 473,644: 16x28
#426 @ 485,657: 28x15
#427 @ 134,132: 16x10
#428 @ 616,810: 29x23
#429 @ 395,1: 10x20
#430 @ 24,391: 22x24
#431 @ 471,64: 15x19
#432 @ 81,252: 10x28
#433 @ 592,348: 18x17
#434 @ 765,535: 12x14
#435 @ 476,17: 17x26
#436 @ 523,129: 24x13
#437 @ 39,529: 24x29
#438 @ 814,785: 24x23
#439 @ 904,301: 26x28
#440 @ 941,247: 25x27
#441 @ 648,688: 11x21
#442 @ 312,489: 28x17
#443 @ 822,34: 10x11
#444 @ 870,408: 17x18
#445 @ 441,147: 17x24
#446 @ 132,475: 27x27
#447 @ 589,275: 17x12
#448 @ 570,716: 26x17
#449 @ 155,472: 13x27
#450 @ 60,221: 22x28
#451 @ 269,485: 11x26
#452 @ 565,555: 22x13
#453 @ 797,684: 14x21
#454 @ 161,753: 28x15
#455 @ 112,825: 14x29
#456 @ 943,887: 20x20
#457 @ 641,169: 14x18
#458 @ 142,179: 24x14
#459 @ 808,689: 16x17
#460 @ 778,207: 27x15
#461 @ 402,819: 11x27
#462 @ 946,694: 27x28
#463 @ 867,351: 25x10
#464 @ 200,262: 18x19
#465 @ 321,82: 23x28
#466 @ 896,30: 29x22
#467 @ 279,116: 15x21
#468 @ 900,867: 19x21
#469 @ 961,1: 12x21
#470 @ 811,169: 4x12
#471 @ 17,606: 16x9
#472 @ 950,898: 23x18
#473 @ 516,886: 15x28
#474 @ 418,377: 28x17
#475 @ 359,381: 26x21
#476 @ 661,709: 29x12
#477 @ 165,425: 11x24
#478 @ 680,354: 28x23
#479 @ 290,604: 28x20
#480 @ 417,368: 27x27
#481 @ 87,69: 27x26
#482 @ 912,123: 29x29
#483 @ 621,154: 22x18
#484 @ 481,558: 18x26
#485 @ 186,64: 10x12
#486 @ 46,553: 28x14
#487 @ 347,350: 18x10
#488 @ 383,685: 8x8
#489 @ 49,235: 16x20
#490 @ 617,487: 27x10
#491 @ 125,867: 29x26
#492 @ 471,301: 27x11
#493 @ 651,927: 23x27
#494 @ 380,683: 24x13
#495 @ 55,504: 14x14
#496 @ 731,2: 22x13
#497 @ 707,362: 15x12
#498 @ 61,312: 11x23
#499 @ 658,61: 22x22
#500 @ 808,392: 18x27
#501 @ 785,765: 11x19
#502 @ 205,356: 28x19
#503 @ 327,105: 16x20
#504 @ 333,753: 22x20
#505 @ 493,644: 21x28
#506 @ 798,119: 22x18
#507 @ 316,125: 16x23
#508 @ 890,351: 16x21
#509 @ 859,484: 16x27
#510 @ 311,663: 10x18
#511 @ 482,302: 23x21
#512 @ 100,552: 11x20
#513 @ 419,592: 13x22
#514 @ 976,466: 19x19
#515 @ 99,870: 25x26
#516 @ 464,349: 10x25
#517 @ 616,261: 25x20
#518 @ 729,826: 21x21
#519 @ 177,792: 26x15
#520 @ 544,755: 28x17
#521 @ 788,522: 16x16
#522 @ 228,278: 26x11
#523 @ 481,625: 21x14
#524 @ 98,170: 12x27
#525 @ 175,766: 17x10
#526 @ 899,369: 25x29
#527 @ 699,508: 11x18
#528 @ 508,887: 18x22
#529 @ 801,15: 23x24
#530 @ 581,215: 20x16
#531 @ 216,81: 19x19
#532 @ 32,940: 11x22
#533 @ 222,749: 22x24
#534 @ 761,180: 22x26
#535 @ 563,860: 20x26
#536 @ 923,252: 22x27
#537 @ 784,899: 29x16
#538 @ 577,630: 11x15
#539 @ 12,893: 24x23
#540 @ 96,169: 18x25
#541 @ 214,938: 18x23
#542 @ 37,189: 20x27
#543 @ 10,965: 24x26
#544 @ 513,12: 28x18
#545 @ 731,828: 13x19
#546 @ 431,156: 11x13
#547 @ 144,590: 29x19
#548 @ 841,678: 20x16
#549 @ 87,137: 27x15
#550 @ 372,772: 28x10
#551 @ 346,878: 25x23
#552 @ 974,831: 22x22
#553 @ 905,159: 11x16
#554 @ 50,110: 28x28
#555 @ 903,233: 20x22
#556 @ 706,119: 27x21
#557 @ 234,973: 12x10
#558 @ 365,455: 21x19
#559 @ 28,317: 28x21
#560 @ 504,434: 29x11
#561 @ 639,691: 11x16
#562 @ 434,250: 19x29
#563 @ 663,901: 11x23
#564 @ 440,434: 21x16
#565 @ 916,345: 15x21
#566 @ 731,513: 21x21
#567 @ 892,79: 12x22
#568 @ 948,824: 29x25
#569 @ 506,828: 10x17
#570 @ 563,190: 11x12
#571 @ 572,863: 26x25
#572 @ 68,495: 18x20
#573 @ 106,128: 17x16
#574 @ 922,226: 18x20
#575 @ 926,257: 18x15
#576 @ 38,722: 22x19
#577 @ 4,296: 11x16
#578 @ 839,477: 15x11
#579 @ 439,185: 22x27
#580 @ 88,726: 11x14
#581 @ 73,702: 23x23
#582 @ 798,848: 14x26
#583 @ 448,557: 5x7
#584 @ 559,778: 27x29
#585 @ 364,250: 6x13
#586 @ 107,455: 23x27
#587 @ 777,282: 25x10
#588 @ 918,207: 17x16
#589 @ 712,668: 16x13
#590 @ 532,7: 15x17
#591 @ 482,23: 6x4
#592 @ 928,597: 23x17
#593 @ 623,38: 21x11
#594 @ 426,52: 29x11
#595 @ 149,775: 19x27
#596 @ 631,24: 27x20
#597 @ 169,611: 25x20
#598 @ 142,470: 12x29
#599 @ 795,642: 19x29
#600 @ 50,77: 16x27
#601 @ 560,745: 21x25
#602 @ 652,389: 16x19
#603 @ 736,636: 17x15
#604 @ 507,972: 17x20
#605 @ 233,481: 24x18
#606 @ 280,302: 10x25
#607 @ 209,456: 10x29
#608 @ 727,708: 19x26
#609 @ 285,780: 24x11
#610 @ 31,3: 22x16
#611 @ 1,693: 14x26
#612 @ 62,10: 13x27
#613 @ 765,184: 13x16
#614 @ 443,134: 15x26
#615 @ 770,58: 13x16
#616 @ 369,28: 11x13
#617 @ 219,426: 11x22
#618 @ 318,249: 12x27
#619 @ 606,343: 16x15
#620 @ 603,262: 14x28
#621 @ 896,308: 12x23
#622 @ 560,854: 15x10
#623 @ 425,118: 21x18
#624 @ 144,966: 7x10
#625 @ 953,117: 14x21
#626 @ 360,772: 23x12
#627 @ 924,604: 17x27
#628 @ 285,670: 16x16
#629 @ 853,637: 19x20
#630 @ 28,614: 21x11
#631 @ 176,531: 16x10
#632 @ 976,845: 15x17
#633 @ 542,539: 27x11
#634 @ 413,702: 18x19
#635 @ 645,61: 14x13
#636 @ 653,933: 10x11
#637 @ 268,113: 21x13
#638 @ 669,376: 26x10
#639 @ 808,798: 13x29
#640 @ 469,750: 16x25
#641 @ 531,2: 19x14
#642 @ 176,148: 27x12
#643 @ 234,460: 29x22
#644 @ 329,304: 25x14
#645 @ 293,109: 11x26
#646 @ 211,323: 16x14
#647 @ 325,97: 11x11
#648 @ 494,525: 29x12
#649 @ 301,824: 26x18
#650 @ 734,809: 10x28
#651 @ 671,175: 23x19
#652 @ 334,233: 25x24
#653 @ 631,947: 22x27
#654 @ 461,825: 17x17
#655 @ 485,202: 15x14
#656 @ 44,673: 21x18
#657 @ 880,481: 3x6
#658 @ 313,220: 23x12
#659 @ 166,102: 18x19
#660 @ 320,70: 22x12
#661 @ 64,870: 13x25
#662 @ 609,252: 20x23
#663 @ 518,206: 15x29
#664 @ 148,715: 29x18
#665 @ 226,775: 18x21
#666 @ 277,293: 14x21
#667 @ 879,174: 17x10
#668 @ 446,57: 16x15
#669 @ 93,552: 26x18
#670 @ 384,406: 20x17
#671 @ 719,653: 29x25
#672 @ 179,263: 23x18
#673 @ 675,342: 25x20
#674 @ 540,818: 17x18
#675 @ 607,287: 24x25
#676 @ 509,598: 19x22
#677 @ 406,352: 18x21
#678 @ 194,805: 25x28
#679 @ 128,453: 16x29
#680 @ 857,516: 14x29
#681 @ 35,5: 15x17
#682 @ 85,978: 25x21
#683 @ 368,188: 24x27
#684 @ 100,676: 21x22
#685 @ 446,69: 26x15
#686 @ 408,829: 19x26
#687 @ 640,88: 13x29
#688 @ 837,969: 13x16
#689 @ 559,44: 21x25
#690 @ 893,171: 23x24
#691 @ 51,505: 21x14
#692 @ 268,881: 19x19
#693 @ 810,42: 29x24
#694 @ 307,494: 21x18
#695 @ 189,602: 10x12
#696 @ 83,806: 20x24
#697 @ 541,421: 29x18
#698 @ 244,15: 11x14
#699 @ 665,160: 29x28
#700 @ 53,681: 22x23
#701 @ 808,183: 14x21
#702 @ 363,757: 18x19
#703 @ 509,202: 19x24
#704 @ 589,316: 13x17
#705 @ 405,188: 16x28
#706 @ 97,808: 15x20
#707 @ 465,482: 14x19
#708 @ 319,584: 18x26
#709 @ 809,779: 10x28
#710 @ 217,785: 14x18
#711 @ 450,282: 28x13
#712 @ 157,90: 20x26
#713 @ 63,401: 16x28
#714 @ 173,84: 29x18
#715 @ 623,829: 26x19
#716 @ 75,596: 10x20
#717 @ 528,323: 14x17
#718 @ 20,688: 21x21
#719 @ 639,804: 18x22
#720 @ 323,166: 19x24
#721 @ 834,43: 22x22
#722 @ 518,44: 16x20
#723 @ 395,724: 18x27
#724 @ 427,303: 8x12
#725 @ 41,377: 16x17
#726 @ 713,625: 24x17
#727 @ 40,303: 22x22
#728 @ 559,307: 19x19
#729 @ 419,251: 21x17
#730 @ 887,54: 10x16
#731 @ 421,161: 13x19
#732 @ 364,428: 11x15
#733 @ 895,357: 27x20
#734 @ 425,931: 18x12
#735 @ 180,918: 9x15
#736 @ 459,819: 24x28
#737 @ 6,687: 24x25
#738 @ 401,755: 29x16
#739 @ 304,10: 26x29
#740 @ 446,555: 10x12
#741 @ 931,235: 13x10
#742 @ 611,251: 10x20
#743 @ 917,152: 28x19
#744 @ 144,559: 22x18
#745 @ 283,578: 21x21
#746 @ 361,792: 11x20
#747 @ 357,463: 29x17
#748 @ 182,597: 23x13
#749 @ 291,457: 25x10
#750 @ 672,589: 28x16
#751 @ 339,309: 26x23
#752 @ 53,863: 27x21
#753 @ 934,742: 21x22
#754 @ 852,332: 27x20
#755 @ 64,201: 22x22
#756 @ 485,823: 23x22
#757 @ 34,617: 28x13
#758 @ 702,638: 20x29
#759 @ 578,92: 12x19
#760 @ 875,126: 24x15
#761 @ 544,120: 28x23
#762 @ 576,640: 15x18
#763 @ 104,853: 23x17
#764 @ 133,574: 21x26
#765 @ 42,605: 11x10
#766 @ 777,547: 18x16
#767 @ 462,456: 24x29
#768 @ 313,557: 14x8
#769 @ 827,963: 24x26
#770 @ 100,54: 13x23
#771 @ 21,688: 11x26
#772 @ 321,184: 29x27
#773 @ 752,507: 11x18
#774 @ 328,770: 14x21
#775 @ 844,305: 14x12
#776 @ 234,4: 15x25
#777 @ 648,519: 23x15
#778 @ 147,404: 20x23
#779 @ 62,332: 25x20
#780 @ 646,26: 10x19
#781 @ 852,88: 13x18
#782 @ 693,826: 28x21
#783 @ 389,707: 18x14
#784 @ 683,881: 20x17
#785 @ 948,623: 12x21
#786 @ 187,241: 19x20
#787 @ 191,744: 20x19
#788 @ 917,472: 11x17
#789 @ 643,312: 21x19
#790 @ 452,30: 3x24
#791 @ 806,777: 26x24
#792 @ 422,622: 19x21
#793 @ 75,571: 27x16
#794 @ 898,497: 12x24
#795 @ 742,858: 21x17
#796 @ 617,77: 26x22
#797 @ 197,797: 21x29
#798 @ 240,573: 10x12
#799 @ 439,135: 10x17
#800 @ 97,410: 23x27
#801 @ 127,657: 26x26
#802 @ 651,99: 29x24
#803 @ 151,46: 14x14
#804 @ 96,722: 14x29
#805 @ 923,815: 26x22
#806 @ 426,621: 28x10
#807 @ 475,818: 27x10
#808 @ 543,543: 29x14
#809 @ 334,301: 18x15
#810 @ 484,311: 25x24
#811 @ 781,357: 22x13
#812 @ 278,560: 21x23
#813 @ 191,940: 14x10
#814 @ 749,857: 23x16
#815 @ 406,421: 29x23
#816 @ 728,934: 28x10
#817 @ 412,153: 18x27
#818 @ 188,237: 10x29
#819 @ 189,825: 11x27
#820 @ 614,372: 11x19
#821 @ 470,66: 20x11
#822 @ 932,250: 12x18
#823 @ 94,423: 14x19
#824 @ 452,768: 25x24
#825 @ 667,862: 12x22
#826 @ 890,352: 25x17
#827 @ 218,244: 19x17
#828 @ 950,165: 19x17
#829 @ 19,633: 14x12
#830 @ 68,591: 27x16
#831 @ 40,131: 24x14
#832 @ 907,847: 14x13
#833 @ 524,723: 21x28
#834 @ 834,474: 15x11
#835 @ 228,169: 27x10
#836 @ 261,479: 20x29
#837 @ 287,738: 22x10
#838 @ 677,358: 11x14
#839 @ 836,48: 10x20
#840 @ 964,689: 20x28
#841 @ 327,237: 14x29
#842 @ 944,761: 13x13
#843 @ 454,822: 23x11
#844 @ 849,732: 27x16
#845 @ 834,450: 17x24
#846 @ 743,627: 28x15
#847 @ 589,17: 28x22
#848 @ 912,123: 12x24
#849 @ 290,683: 17x16
#850 @ 696,308: 22x25
#851 @ 314,819: 23x21
#852 @ 19,118: 21x16
#853 @ 240,686: 25x23
#854 @ 272,54: 29x18
#855 @ 855,501: 17x21
#856 @ 494,361: 28x17
#857 @ 758,657: 16x18
#858 @ 351,383: 23x11
#859 @ 717,43: 23x23
#860 @ 392,520: 11x13
#861 @ 584,199: 10x24
#862 @ 391,151: 17x23
#863 @ 152,692: 22x20
#864 @ 809,165: 16x24
#865 @ 284,65: 19x28
#866 @ 316,713: 11x20
#867 @ 10,670: 14x24
#868 @ 890,25: 22x12
#869 @ 824,78: 29x22
#870 @ 787,681: 23x27
#871 @ 660,453: 29x21
#872 @ 425,835: 28x20
#873 @ 940,641: 16x14
#874 @ 953,4: 14x18
#875 @ 265,643: 16x10
#876 @ 530,11: 22x19
#877 @ 576,81: 18x13
#878 @ 739,812: 23x27
#879 @ 272,471: 22x24
#880 @ 775,532: 22x18
#881 @ 11,288: 26x22
#882 @ 908,242: 29x11
#883 @ 328,45: 28x16
#884 @ 885,735: 23x10
#885 @ 449,197: 19x28
#886 @ 115,433: 14x29
#887 @ 523,878: 22x17
#888 @ 772,412: 5x9
#889 @ 49,709: 20x17
#890 @ 407,639: 28x20
#891 @ 25,126: 25x26
#892 @ 962,416: 18x18
#893 @ 510,139: 24x29
#894 @ 614,967: 17x17
#895 @ 563,847: 27x21
#896 @ 931,713: 14x15
#897 @ 890,139: 9x10
#898 @ 822,122: 20x29
#899 @ 25,209: 15x13
#900 @ 690,650: 10x13
#901 @ 430,449: 14x16
#902 @ 826,675: 20x27
#903 @ 360,34: 12x21
#904 @ 809,3: 25x16
#905 @ 654,624: 26x25
#906 @ 115,736: 27x29
#907 @ 87,312: 12x20
#908 @ 233,156: 11x18
#909 @ 359,243: 15x24
#910 @ 779,718: 20x23
#911 @ 866,347: 25x28
#912 @ 628,39: 27x13
#913 @ 98,980: 4x13
#914 @ 756,538: 14x15
#915 @ 697,606: 11x24
#916 @ 658,883: 28x21
#917 @ 767,693: 19x15
#918 @ 509,867: 27x25
#919 @ 134,475: 17x20
#920 @ 184,801: 14x16
#921 @ 950,726: 15x27
#922 @ 421,431: 13x15
#923 @ 362,772: 23x28
#924 @ 171,595: 17x11
#925 @ 69,489: 18x24
#926 @ 48,705: 21x19
#927 @ 87,264: 15x16
#928 @ 682,479: 21x29
#929 @ 665,832: 23x25
#930 @ 803,394: 23x27
#931 @ 828,300: 29x27
#932 @ 585,190: 13x10
#933 @ 329,208: 11x22
#934 @ 903,337: 18x18
#935 @ 9,919: 19x14
#936 @ 566,925: 28x15
#937 @ 412,211: 14x26
#938 @ 87,46: 16x14
#939 @ 91,551: 15x27
#940 @ 558,499: 27x12
#941 @ 864,824: 26x10
#942 @ 760,658: 22x10
#943 @ 782,255: 18x12
#944 @ 352,235: 17x29
#945 @ 456,162: 27x21
#946 @ 662,800: 26x21
#947 @ 456,186: 19x28
#948 @ 24,627: 26x19
#949 @ 348,344: 26x16
#950 @ 542,730: 29x16
#951 @ 559,618: 15x21
#952 @ 513,857: 19x16
#953 @ 285,898: 24x16
#954 @ 634,183: 17x26
#955 @ 955,407: 16x12
#956 @ 349,789: 17x15
#957 @ 275,910: 28x24
#958 @ 818,519: 25x25
#959 @ 696,589: 12x11
#960 @ 942,126: 24x29
#961 @ 337,595: 24x14
#962 @ 298,707: 29x21
#963 @ 351,157: 14x17
#964 @ 453,829: 11x26
#965 @ 496,782: 20x24
#966 @ 345,582: 16x25
#967 @ 19,380: 16x18
#968 @ 170,667: 22x27
#969 @ 87,616: 11x29
#970 @ 766,418: 12x27
#971 @ 338,57: 10x15
#972 @ 814,818: 12x26
#973 @ 761,969: 29x25
#974 @ 161,528: 18x10
#975 @ 748,872: 10x15
#976 @ 509,872: 11x29
#977 @ 929,248: 16x10
#978 @ 143,862: 18x11
#979 @ 346,369: 8x12
#980 @ 762,652: 28x28
#981 @ 789,669: 12x21
#982 @ 881,140: 29x25
#983 @ 490,521: 24x22
#984 @ 246,834: 11x15
#985 @ 87,864: 12x20
#986 @ 638,821: 22x20
#987 @ 53,461: 29x28
#988 @ 833,296: 20x21
#989 @ 948,618: 26x23
#990 @ 710,819: 14x16
#991 @ 690,208: 27x18
#992 @ 623,208: 15x25
#993 @ 969,887: 29x24
#994 @ 167,495: 22x24
#995 @ 966,791: 20x13
#996 @ 485,772: 15x15
#997 @ 571,40: 15x19
#998 @ 312,192: 10x29
#999 @ 300,72: 28x25
#1000 @ 887,520: 24x17
#1001 @ 891,355: 14x17
#1002 @ 846,324: 19x11
#1003 @ 797,809: 25x12
#1004 @ 160,148: 21x15
#1005 @ 30,492: 21x23
#1006 @ 823,390: 24x20
#1007 @ 830,835: 18x22
#1008 @ 53,335: 21x17
#1009 @ 560,204: 25x29
#1010 @ 526,192: 21x24
#1011 @ 875,468: 13x24
#1012 @ 369,142: 27x12
#1013 @ 497,765: 26x18
#1014 @ 337,922: 18x17
#1015 @ 687,922: 11x18
#1016 @ 480,375: 17x22
#1017 @ 936,542: 20x11
#1018 @ 869,352: 25x22
#1019 @ 533,284: 16x27
#1020 @ 106,835: 23x12
#1021 @ 185,785: 27x25
#1022 @ 192,864: 15x27
#1023 @ 727,408: 19x29
#1024 @ 647,21: 29x15
#1025 @ 589,212: 27x29
#1026 @ 194,594: 29x11
#1027 @ 826,31: 24x19
#1028 @ 98,550: 24x29
#1029 @ 666,22: 29x10
#1030 @ 657,898: 10x28
#1031 @ 591,646: 17x14
#1032 @ 242,189: 29x20
#1033 @ 242,981: 12x17
#1034 @ 423,828: 29x29
#1035 @ 644,357: 16x14
#1036 @ 23,253: 13x12
#1037 @ 439,532: 17x13
#1038 @ 670,834: 24x17
#1039 @ 302,81: 29x26
#1040 @ 438,541: 23x21
#1041 @ 230,76: 16x17
#1042 @ 613,321: 12x29
#1043 @ 648,170: 29x12
#1044 @ 115,185: 20x4
#1045 @ 480,315: 26x28
#1046 @ 675,56: 11x26
#1047 @ 233,344: 22x26
#1048 @ 139,304: 13x28
#1049 @ 712,685: 26x26
#1050 @ 723,179: 14x26
#1051 @ 824,769: 13x23
#1052 @ 332,297: 20x21
#1053 @ 516,288: 18x17
#1054 @ 243,962: 14x26
#1055 @ 849,328: 20x23
#1056 @ 931,632: 16x20
#1057 @ 815,83: 12x27
#1058 @ 48,483: 14x21
#1059 @ 844,271: 11x15
#1060 @ 647,599: 29x29
#1061 @ 865,890: 15x5
#1062 @ 510,792: 24x23
#1063 @ 52,985: 28x10
#1064 @ 663,116: 19x25
#1065 @ 618,534: 25x24
#1066 @ 97,721: 27x22
#1067 @ 394,398: 29x28
#1068 @ 372,760: 19x21
#1069 @ 237,198: 15x25
#1070 @ 974,818: 11x14
#1071 @ 529,869: 23x18
#1072 @ 767,410: 15x14
#1073 @ 319,212: 27x21
#1074 @ 366,313: 28x18
#1075 @ 899,293: 17x16
#1076 @ 574,290: 10x27
#1077 @ 25,669: 22x23
#1078 @ 831,531: 17x10
#1079 @ 389,711: 22x14
#1080 @ 493,975: 19x17
#1081 @ 796,793: 18x15
#1082 @ 587,783: 28x17
#1083 @ 674,110: 28x19
#1084 @ 489,239: 18x15
#1085 @ 377,224: 12x28
#1086 @ 455,823: 26x10
#1087 @ 421,343: 19x28
#1088 @ 322,73: 16x5
#1089 @ 609,341: 13x17
#1090 @ 740,953: 26x27
#1091 @ 279,193: 12x23
#1092 @ 112,864: 10x27
#1093 @ 427,263: 23x28
#1094 @ 447,133: 10x28
#1095 @ 886,64: 11x29
#1096 @ 579,487: 21x26
#1097 @ 809,901: 20x27
#1098 @ 920,117: 29x22
#1099 @ 504,317: 11x19
#1100 @ 115,313: 25x27
#1101 @ 772,419: 13x20
#1102 @ 2,369: 13x11
#1103 @ 983,415: 13x23
#1104 @ 493,129: 24x28
#1105 @ 776,543: 14x14
#1106 @ 193,77: 20x25
#1107 @ 307,648: 21x16
#1108 @ 117,302: 17x27
#1109 @ 645,345: 11x25
#1110 @ 590,351: 23x18
#1111 @ 240,251: 14x14
#1112 @ 383,247: 16x11
#1113 @ 470,477: 20x10
#1114 @ 506,231: 21x23
#1115 @ 420,300: 28x19
#1116 @ 781,934: 26x22
#1117 @ 790,949: 11x14
#1118 @ 39,647: 22x25
#1119 @ 893,259: 21x27
#1120 @ 531,23: 25x25
#1121 @ 906,832: 29x16
#1122 @ 261,926: 21x27
#1123 @ 941,323: 16x21
#1124 @ 613,170: 17x28
#1125 @ 311,812: 19x18
#1126 @ 153,702: 18x14
#1127 @ 746,213: 16x18
#1128 @ 848,274: 25x22
#1129 @ 449,506: 14x20
#1130 @ 515,195: 12x20
#1131 @ 488,701: 25x12
#1132 @ 947,222: 25x13
#1133 @ 690,927: 21x12
#1134 @ 329,775: 12x12
#1135 @ 76,339: 14x11
#1136 @ 836,452: 11x11
#1137 @ 716,368: 15x12
#1138 @ 909,882: 28x22
#1139 @ 944,61: 24x16
#1140 @ 614,311: 28x23
#1141 @ 635,652: 24x16
#1142 @ 486,559: 22x16
#1143 @ 583,93: 12x12
#1144 @ 476,666: 11x20
#1145 @ 599,60: 17x13
#1146 @ 150,685: 14x14
#1147 @ 14,384: 26x14
#1148 @ 152,669: 13x25
#1149 @ 198,756: 22x18
#1150 @ 615,965: 14x17
#1151 @ 784,779: 15x16
#1152 @ 7,478: 20x12
#1153 @ 64,983: 10x13
#1154 @ 331,90: 29x17
#1155 @ 179,254: 19x13
#1156 @ 194,866: 9x11
#1157 @ 133,66: 13x28
#1158 @ 557,797: 13x23
#1159 @ 957,814: 19x14
#1160 @ 28,262: 20x17
#1161 @ 403,921: 10x18
#1162 @ 506,380: 18x21
#1163 @ 418,57: 12x19
#1164 @ 56,559: 11x10
#1165 @ 738,916: 18x20
#1166 @ 36,181: 25x12
#1167 @ 388,176: 20x15
#1168 @ 567,28: 3x3
#1169 @ 534,115: 22x13
#1170 @ 322,144: 14x29
#1171 @ 119,465: 27x16
#1172 @ 272,913: 14x21
#1173 @ 786,679: 19x21
#1174 @ 522,80: 28x18
#1175 @ 398,825: 23x16
#1176 @ 753,594: 17x24
#1177 @ 757,945: 28x28
#1178 @ 892,750: 26x15
#1179 @ 418,249: 25x18
#1180 @ 649,561: 26x22
#1181 @ 780,426: 26x14
#1182 @ 680,540: 16x20
#1183 @ 687,583: 23x25
#1184 @ 308,884: 20x14
#1185 @ 908,149: 16x26
#1186 @ 197,244: 16x26
#1187 @ 484,77: 17x14
#1188 @ 634,956: 16x11
#1189 @ 547,921: 22x16
#1190 @ 682,457: 12x24
#1191 @ 661,684: 24x12
#1192 @ 970,412: 20x13
#1193 @ 357,159: 14x19
#1194 @ 37,169: 11x23
#1195 @ 22,225: 14x20
#1196 @ 510,800: 23x16
#1197 @ 16,205: 15x26
#1198 @ 792,624: 11x11
#1199 @ 431,355: 26x13
#1200 @ 508,31: 29x18
#1201 @ 960,229: 24x24
#1202 @ 596,250: 25x21
#1203 @ 876,734: 17x29
#1204 @ 518,305: 16x22
#1205 @ 18,171: 28x12
#1206 @ 225,739: 18x17
#1207 @ 761,110: 22x28
#1208 @ 500,844: 18x20
#1209 @ 768,664: 21x28
#1210 @ 927,136: 23x10
#1211 @ 299,384: 21x23
#1212 @ 438,641: 25x18
#1213 @ 777,350: 15x29
#1214 @ 850,512: 17x10
#1215 @ 520,512: 14x10
#1216 @ 556,837: 10x23
#1217 @ 19,695: 14x20
#1218 @ 297,175: 25x18
#1219 @ 922,334: 26x12
#1220 @ 530,717: 23x10
#1221 @ 857,494: 27x15
#1222 @ 574,15: 27x17
#1223 @ 913,42: 10x15
#1224 @ 892,0: 27x11
#1225 @ 80,551: 24x29
#1226 @ 77,311: 19x29
#1227 @ 635,945: 10x24
#1228 @ 548,548: 13x25
#1229 @ 836,260: 18x27
#1230 @ 834,125: 22x12
#1231 @ 132,456: 13x15
#1232 @ 503,317: 12x27
#1233 @ 441,326: 18x13
#1234 @ 442,867: 26x23
#1235 @ 782,458: 19x22
#1236 @ 875,211: 4x7
#1237 @ 961,883: 11x10
#1238 @ 751,504: 20x15
#1239 @ 730,559: 29x17
#1240 @ 641,780: 16x26
#1241 @ 318,823: 11x15
#1242 @ 190,275: 17x28
#1243 @ 826,778: 10x22
#1244 @ 720,612: 28x21
#1245 @ 692,586: 22x11
#1246 @ 592,494: 21x12
#1247 @ 833,567: 26x14
#1248 @ 919,143: 28x28
#1249 @ 111,814: 18x14
#1250 @ 656,609: 17x13
#1251 @ 908,260: 22x29
#1252 @ 944,769: 12x11
#1253 @ 857,515: 21x17
#1254 @ 809,30: 16x14
#1255 @ 386,101: 25x21
#1256 @ 322,713: 19x12
#1257 @ 640,404: 23x25
#1258 @ 566,90: 24x16
#1259 @ 727,919: 14x28
#1260 @ 812,86: 5x11
#1261 @ 445,28: 17x29
#1262 @ 79,332: 22x18
#1263 @ 796,651: 28x21
#1264 @ 665,773: 19x28
#1265 @ 381,390: 21x13
#1266 @ 757,499: 29x26
#1267 @ 400,11: 11x10
#1268 @ 776,67: 15x18
#1269 @ 292,267: 26x10
#1270 @ 424,604: 22x19
#1271 @ 312,63: 15x15
#1272 @ 249,834: 16x13
#1273 @ 524,809: 17x15
#1274 @ 501,889: 27x23
#1275 @ 140,365: 24x28
#1276 @ 430,124: 23x25
#1277 @ 565,415: 23x15
#1278 @ 86,325: 21x14
#1279 @ 64,568: 25x29
#1280 @ 800,787: 26x28
#1281 @ 582,876: 11x10
#1282 @ 180,402: 24x12
#1283 @ 720,394: 18x15
#1284 @ 209,305: 10x28
#1285 @ 771,542: 26x15
#1286 @ 402,641: 21x27
#1287 @ 138,557: 18x28
#1288 @ 29,32: 14x12
#1289 @ 815,838: 18x13
#1290 @ 576,771: 22x23
#1291 @ 19,587: 25x20
#1292 @ 415,21: 24x25
#1293 @ 548,321: 18x23
#1294 @ 256,642: 27x20
#1295 @ 65,434: 12x29
#1296 @ 498,392: 13x27
#1297 @ 389,96: 14x12
#1298 @ 40,503: 20x24
#1299 @ 228,364: 10x21
#1300 @ 177,909: 17x28
#1301 @ 882,758: 19x20
#1302 @ 311,449: 21x10
#1303 @ 58,397: 13x16
#1304 @ 202,538: 23x22
#1305 @ 922,138: 15x25
#1306 @ 859,122: 15x22
#1307 @ 61,251: 11x20
#1308 @ 226,352: 12x28
#1309 @ 275,651: 21x10
#1310 @ 107,545: 16x19
#1311 @ 893,839: 19x26
#1312 @ 664,883: 29x29
#1313 @ 807,825: 16x13
#1314 @ 555,745: 11x13
#1315 @ 146,372: 28x15
#1316 @ 979,531: 11x14
#1317 @ 45,494: 22x14
#1318 @ 595,225: 25x28
#1319 @ 316,867: 25x29
#1320 @ 498,704: 16x17
#1321 @ 60,204: 26x25
#1322 @ 136,489: 25x21
#1323 @ 793,626: 19x20"""
