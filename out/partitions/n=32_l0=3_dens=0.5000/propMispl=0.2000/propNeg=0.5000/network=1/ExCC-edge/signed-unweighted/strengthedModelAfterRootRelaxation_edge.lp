\ENCODING=ISO-8859-1
\Problem name: ilog.cplex

Minimize
 obj1: - x_0_1 - x_0_2 - x_0_3 - x_0_4 + x_0_5 + x_0_6 - x_0_7 - x_0_8 + x_0_9
       - x_0_10 + x_0_12 + x_0_13 + x_0_19 + x_16_19 + x_0_21 + x_16_23
       + x_16_25 + x_0_26 + x_16_26 + x_16_27 + x_0_29 + x_16_29 + x_16_30
       + x_0_31 + x_16_31 - x_1_2 - x_1_3 - x_1_4 + x_1_5 + x_1_6 - x_1_7
       - x_1_9 - x_1_10 - x_1_12 + x_1_15 + x_1_16 + x_17_18 + x_17_19
       - x_17_20 + x_1_21 - x_17_21 + x_1_24 + x_1_27 + x_17_27 + x_17_30
       - x_2_3 - x_2_4 + x_2_5 - x_2_6 - x_2_9 + x_2_12 + x_2_13 + x_2_16
       - x_18_19 - x_18_21 - x_18_22 - x_18_24 + x_18_25 + x_18_27 + x_2_28
       - x_18_29 + x_18_30 - x_3_4 - x_3_5 - x_3_6 - x_3_7 + x_3_8 - x_3_9
       - x_3_10 + x_3_11 + x_3_17 - x_19_20 + x_3_23 - x_3_25 + x_3_26 + x_3_30
       - x_4_7 - x_4_9 - x_4_10 + x_4_13 + x_4_14 + x_4_15 - x_4_17 + x_4_19
       + x_4_22 - x_4_23 + x_20_26 + x_4_29 + x_20_29 + x_4_30 + x_20_30
       - x_4_31 - x_5_6 - x_5_8 - x_5_9 - x_5_10 + x_5_22 + x_21_23 - x_21_24
       + x_5_25 + x_5_26 + x_21_27 - x_5_31 + x_21_31 - x_6_7 - x_6_8 + x_6_9
       - x_6_10 + x_6_13 + x_6_15 - x_6_16 + x_6_17 + x_6_18 + x_6_21 - x_6_22
       + x_6_23 + x_22_23 - x_22_24 + x_22_25 - x_6_26 - x_22_26 + x_6_27
       + x_6_28 + x_22_28 + x_22_29 - x_7_10 - x_7_11 + x_7_12 - x_7_13
       - x_7_15 + x_7_17 - x_7_18 + x_7_19 + x_7_21 + x_7_23 + x_7_26 + x_23_27
       + x_7_28 - x_23_28 - x_23_29 - x_23_30 + x_7_31 - x_8_9 + x_8_10
       + x_8_12 - x_8_14 + x_8_16 - x_8_21 - x_24_26 - x_24_27 + x_8_28
       - x_24_28 + x_8_29 + x_24_29 + x_8_30 + x_8_31 - x_24_31 + x_9_11
       + x_9_12 + x_9_13 + x_9_21 - x_9_22 + x_9_23 + x_9_24 - x_25_26
       - x_25_27 - x_25_28 + x_9_30 - x_25_30 - x_25_31 + x_10_11 + x_10_14
       - x_10_16 + x_10_17 - x_10_21 - x_10_22 + x_10_27 - x_26_27 - x_26_28
       + x_10_30 + x_26_30 + x_10_31 - x_26_31 - x_11_12 - x_11_13 - x_11_15
       + x_11_16 - x_11_17 - x_11_18 - x_11_19 - x_11_21 + x_11_25 - x_27_28
       + x_27_29 - x_27_30 - x_27_31 - x_12_13 - x_12_14 + x_12_16 - x_12_17
       - x_12_18 - x_12_19 - x_12_20 - x_12_21 + x_12_22 - x_12_24 - x_12_27
       + x_28_29 + x_12_30 - x_28_31 - x_13_15 - x_13_16 - x_13_17 - x_13_18
       - x_13_19 - x_13_20 - x_13_21 + x_13_23 + x_13_24 + x_13_25 + x_13_26
       + x_13_27 + x_13_31 - x_29_31 + x_14_15 - x_14_16 - x_14_17 - x_14_19
       - x_14_20 - x_14_21 + x_14_22 + x_14_23 + x_14_24 + x_14_31 - x_30_31
       - x_15_16 - x_15_18 - x_15_20 - x_15_21 - x_15_24 - x_15_25 - x_15_27
       + x_15_28 - x_15_31 + 122
Subject To
 c1:   x_0_3 - x_0_5 + x_3_5 <= 1
 c2:   x_0_2 - x_0_6 + x_2_6 <= 1
 c3:   x_0_1 - x_0_9 + x_1_9 <= 1
 c4:   x_0_1 - x_0_12 + x_1_12 <= 1
 c5:   - x_8_12 + x_8_21 + x_12_21 <= 1
 c6:   x_6_8 + x_6_16 - x_8_16 <= 1
 c7:   x_0_1 - x_0_19 + x_1_12 + x_12_19 <= 2
 c8:   - x_16_19 + x_13_16 + x_13_19 <= 1
 c9:   x_0_4 - x_0_29 + x_4_23 + x_23_29 <= 2
 c10:  x_5_8 + x_5_31 - x_8_30 + x_30_31 <= 2
 c11:  x_0_4 - x_0_31 + x_4_31 <= 1
 c12:  x_5_8 + x_5_31 - x_8_31 <= 1
 c13:  x_1_3 - x_1_5 + x_3_5 <= 1
 c14:  x_1_2 - x_1_6 + x_2_6 <= 1
 c15:  x_1_7 - x_1_15 + x_7_15 <= 1
 c16:  x_1_10 - x_1_16 + x_10_16 <= 1
 c17:  - x_17_18 + x_11_17 + x_11_18 <= 1
 c18:  - x_17_19 + x_11_17 + x_11_19 <= 1
 c19:  x_8_9 + x_8_21 - x_9_21 <= 1
 c20:  x_4_9 + x_4_23 - x_9_23 <= 1
 c21:  - x_17_27 + x_12_17 + x_12_27 <= 1
 c22:  x_2_3 - x_2_5 + x_3_5 <= 1
 c23:  - x_10_11 + x_10_21 + x_11_21 <= 1
 c24:  x_1_2 + x_1_12 - x_2_12 <= 1
 c25:  x_2_6 - x_2_16 + x_6_16 <= 1
 c26:  x_17_21 - x_10_17 + x_10_21 <= 1
 c27:  - x_18_25 + x_15_18 + x_15_25 <= 1
 c28:  - x_18_27 + x_12_18 + x_12_27 <= 1
 c29:  x_2_3 - x_2_28 + x_3_25 + x_25_28 <= 2
 c30:  x_25_26 + x_25_30 - x_26_30 <= 1
 c31:  x_0_3 + x_0_8 - x_3_8 <= 1
 c32:  x_3_7 - x_3_11 + x_7_11 <= 1
 c33:  x_11_13 - x_11_16 + x_13_16 <= 1
 c34:  x_3_4 - x_3_17 + x_4_17 <= 1
 c35:  x_3_4 - x_3_23 + x_4_23 <= 1
 c36:  x_11_15 - x_11_25 + x_15_25 <= 1
 c37:  - x_27_29 + x_27_31 + x_29_31 <= 1
 c38:  - x_4_13 + x_4_17 + x_13_17 <= 1
 c39:  - x_4_14 + x_4_17 + x_14_17 <= 1
 c40:  x_4_7 - x_4_15 + x_7_15 <= 1
 c41:  x_12_13 - x_12_16 + x_13_16 <= 1
 c42:  x_4_17 - x_4_19 + x_11_17 + x_11_19 <= 2
 c43:  x_4_9 - x_4_22 + x_9_22 <= 1
 c44:  x_18_22 + x_12_18 - x_12_22 <= 1
 c45:  - x_20_26 + x_24_26 + x_12_20 + x_12_24 <= 2
 c46:  x_4_23 - x_4_29 + x_23_29 <= 1
 c47:  x_18_19 + x_18_29 + x_19_20 - x_20_29 <= 2
 c48:  x_23_28 + x_23_29 - x_28_29 <= 1
 c49:  x_5_6 - x_5_22 + x_6_22 <= 1
 c50:  x_18_24 + x_13_18 - x_13_24 <= 1
 c51:  x_13_15 - x_13_25 + x_15_25 <= 1
 c52:  - x_21_27 + x_12_21 + x_12_27 <= 1
 c53:  x_12_13 + x_12_27 - x_13_27 <= 1
 c54:  - x_21_31 + x_15_21 + x_15_31 <= 1
 c55:  x_13_15 - x_13_31 + x_15_31 <= 1
 c56:  x_2_6 + x_2_9 - x_6_9 <= 1
 c57:  x_21_24 + x_14_21 - x_14_24 <= 1
 c58:  - x_22_25 + x_22_26 + x_25_26 <= 1
 c59:  x_6_26 - x_6_27 + x_26_27 <= 1
 c60:  x_6_26 - x_6_28 + x_26_28 <= 1
 c61:  x_22_24 - x_22_28 + x_24_28 <= 1
 c62:  - x_7_12 + x_7_18 + x_12_18 <= 1
 c63:  x_4_7 + x_4_17 - x_7_17 <= 1
 c64:  x_18_19 + x_7_18 - x_7_19 <= 1
 c65:  x_18_21 + x_7_18 - x_7_21 <= 1
 c66:  x_4_7 + x_4_23 - x_7_23 <= 1
 c67:  - x_23_27 + x_23_28 + x_27_28 <= 1
 c68:  x_24_28 + x_15_24 - x_15_28 <= 1
 c69:  x_4_7 + x_4_31 - x_7_31 <= 1
 c70:  x_0_1 - x_0_3 + x_1_3 <= 1
 c71:  - x_0_5 + x_0_10 + x_5_10 <= 1
 c72:  - x_0_6 + x_0_7 + x_6_7 <= 1
 c73:  - x_8_12 + x_8_14 + x_12_14 <= 1
 c74:  - x_16_19 + x_14_16 + x_14_19 <= 1
 c75:  x_17_21 + x_8_14 - x_8_21 + x_14_17 <= 2
 c76:  x_24_26 - x_24_28 + x_26_28 <= 1
 c77:  x_24_27 - x_24_29 + x_27_29 <= 1
 c78:  - x_1_5 + x_1_9 + x_5_9 <= 1
 c79:  x_1_3 - x_1_6 + x_3_6 <= 1
 c80:  x_4_7 + x_4_9 + x_7_11 - x_9_11 <= 2
 c81:  x_1_7 - x_1_15 + x_7_11 + x_11_15 <= 2
 c82:  - x_17_18 + x_12_17 + x_12_18 <= 1
 c83:  - x_17_19 + x_12_17 + x_12_19 <= 1
 c84:  x_1_12 - x_1_21 + x_12_21 <= 1
 c85:  - x_25_26 + x_25_27 + x_26_27 <= 1
 c86:  - x_17_27 + x_13_15 + x_13_17 + x_15_27 <= 2
 c87:  - x_2_5 + x_2_9 + x_5_9 <= 1
 c88:  x_2_3 - x_2_6 + x_3_6 <= 1
 c89:  x_7_10 + x_7_11 - x_10_11 <= 1
 c90:  - x_18_24 + x_15_18 + x_15_24 <= 1
 c91:  - x_18_25 + x_25_27 + x_15_18 + x_15_27 <= 2
 c92:  - x_18_27 + x_15_18 + x_15_27 <= 1
 c93:  x_22_24 + x_24_27 + x_10_22 - x_10_27 <= 2
 c94:  x_26_27 - x_26_30 + x_27_30 <= 1
 c95:  - x_3_5 + x_3_6 + x_5_6 <= 1
 c96:  x_3_6 - x_3_8 + x_6_8 <= 1
 c97:  - x_11_16 + x_11_19 + x_14_16 + x_14_19 <= 2
 c98:  x_17_21 - x_11_17 + x_11_21 <= 1
 c99:  - x_19_20 + x_12_19 + x_12_20 <= 1
 c100: x_25_27 + x_11_15 - x_11_25 + x_15_27 <= 2
 c101: x_1_4 + x_1_12 - x_4_13 + x_12_13 <= 2
 c102: x_4_7 - x_4_15 + x_7_11 + x_11_15 <= 2
 c103: x_12_14 - x_12_16 + x_14_16 <= 1
 c104: x_1_4 + x_1_12 - x_4_17 + x_12_17 <= 2
 c105: x_1_4 + x_1_12 - x_4_19 + x_12_19 <= 2
 c106: x_4_10 - x_4_22 + x_10_22 <= 1
 c107: x_21_24 + x_12_21 - x_12_24 <= 1
 c108: x_12_18 - x_12_27 + x_15_18 + x_15_27 <= 2
 c109: x_18_29 - x_20_29 + x_12_18 + x_12_20 <= 2
 c110: x_27_28 + x_27_29 - x_28_29 <= 1
 c111: - x_13_16 + x_13_17 + x_14_16 + x_14_17 <= 2
 c112: x_18_21 + x_18_29 - x_21_23 + x_23_29 <= 2
 c113: x_18_29 + x_23_29 + x_13_18 - x_13_23 <= 2
 c114: x_21_24 + x_13_21 - x_13_24 <= 1
 c115: - x_5_25 + x_5_31 + x_25_31 <= 1
 c116: x_25_27 + x_13_15 - x_13_25 + x_15_27 <= 2
 c117: x_21_24 + x_24_26 + x_13_21 - x_13_26 <= 2
 c118: - x_21_27 + x_15_21 + x_15_27 <= 1
 c119: x_13_15 - x_13_27 + x_15_27 <= 1
 c120: x_21_24 - x_21_31 + x_24_31 <= 1
 c121: x_5_6 + x_5_9 - x_6_9 <= 1
 c122: - x_14_15 + x_14_20 + x_15_20 <= 1
 c123: x_17_21 + x_14_17 - x_14_21 <= 1
 c124: x_18_19 + x_18_29 + x_23_29 + x_14_19 - x_14_23 <= 3
 c125: x_17_21 + x_21_24 + x_14_17 - x_14_24 <= 2
 c126: x_22_26 - x_22_28 + x_26_28 <= 1
 c127: x_17_21 + x_21_24 + x_24_31 + x_14_17 - x_14_31 <= 3
 c128: x_1_7 + x_1_12 - x_7_12 <= 1
 c129: x_7_11 - x_7_13 + x_11_13 <= 1
 c130: x_7_11 - x_7_15 + x_11_15 <= 1
 c131: x_1_7 + x_1_12 - x_7_17 + x_12_17 <= 2
 c132: x_7_11 - x_7_18 + x_11_18 <= 1
 c133: x_7_11 - x_7_19 + x_11_19 <= 1
 c134: x_7_11 - x_7_21 + x_11_21 <= 1
 c135: x_25_27 - x_15_25 + x_15_27 <= 1
 c136: - x_23_27 + x_23_29 + x_27_29 <= 1
 c137: - x_23_28 + x_23_29 + x_27_28 + x_27_29 <= 2
 c138: x_27_28 + x_15_27 - x_15_28 <= 1
 c139: x_24_31 + x_15_24 - x_15_31 <= 1
 c140: x_0_2 - x_0_9 + x_2_9 <= 1
 c141: - x_16_19 + x_18_19 + x_15_16 + x_15_18 <= 2
 c142: x_23_29 + x_23_30 + x_24_26 - x_24_29 + x_26_30 <= 3
 c143: x_0_1 + x_0_5 - x_1_5 <= 1
 c144: x_0_1 + x_0_6 - x_1_6 <= 1
 c145: x_3_7 + x_3_9 + x_7_13 - x_9_11 + x_11_13 <= 3
 c146: x_1_7 - x_1_12 + x_7_13 + x_12_13 <= 2
 c147: x_3_7 + x_3_9 + x_7_13 - x_9_12 + x_12_13 <= 3
 c148: x_3_7 + x_3_9 + x_7_13 - x_9_13 <= 2
 c149: x_1_7 - x_1_15 + x_7_13 + x_13_15 <= 2
 c150: x_1_7 - x_1_21 + x_7_13 + x_13_21 <= 2
 c151: x_3_7 + x_3_9 + x_7_13 - x_9_21 + x_13_21 <= 3
 c152: x_17_18 - x_17_27 + x_18_24 + x_24_27 <= 2
 c153: x_7_10 + x_7_13 - x_10_11 + x_11_13 <= 2
 c154: x_7_10 + x_7_13 - x_10_14 + x_13_17 + x_14_17 <= 3
 c155: x_7_10 + x_7_13 - x_10_17 + x_13_17 <= 2
 c156: x_7_10 + x_7_13 - x_10_21 + x_13_21 <= 2
 c157: x_18_24 - x_18_25 + x_24_26 + x_25_26 <= 2
 c158: x_18_24 - x_18_27 + x_24_27 <= 1
 c159: x_3_7 - x_3_11 + x_7_13 + x_11_13 <= 2
 c160: x_11_15 - x_11_16 + x_15_16 <= 1
 c161: x_3_7 - x_3_17 + x_7_13 + x_13_17 <= 2
 c162: x_18_24 + x_24_26 + x_25_26 + x_11_18 - x_11_25 <= 3
 c163: x_23_29 + x_23_30 - x_27_29 + x_27_30 <= 2
 c164: x_4_7 - x_4_13 + x_7_13 <= 1
 c165: x_4_7 - x_4_14 + x_7_13 + x_13_17 + x_14_17 <= 3
 c166: x_4_7 - x_4_15 + x_7_13 + x_13_15 <= 2
 c167: - x_12_16 + x_12_18 + x_15_16 + x_15_18 <= 2
 c168: x_4_7 - x_4_17 + x_7_13 + x_13_17 <= 2
 c169: x_4_7 - x_4_19 + x_7_13 + x_13_19 <= 2
 c170: x_18_24 + x_12_18 - x_12_24 <= 1
 c171: x_17_18 + x_17_20 + x_18_24 - x_20_26 + x_24_26 <= 3
 c172: x_18_24 + x_24_27 + x_12_18 - x_12_27 <= 2
 c173: x_23_29 + x_23_30 + x_25_28 + x_25_30 - x_28_29 <= 3
 c174: x_13_15 - x_13_16 + x_15_16 <= 1
 c175: x_17_18 + x_13_17 - x_13_18 <= 1
 c176: x_5_9 - x_5_22 + x_9_22 <= 1
 c177: x_18_21 + x_18_24 - x_21_24 <= 1
 c178: x_17_18 + x_18_24 + x_13_17 - x_13_24 <= 2
 c179: x_17_18 + x_18_24 + x_24_26 + x_25_26 + x_13_17 - x_13_25 <= 4
 c180: x_17_18 + x_18_24 + x_24_26 + x_13_17 - x_13_26 <= 3
 c181: x_18_21 + x_18_24 - x_21_27 + x_24_27 <= 2
 c182: x_17_18 + x_18_24 + x_24_27 + x_13_17 - x_13_27 <= 3
 c183: x_18_21 + x_18_24 - x_21_31 + x_24_31 <= 2
 c184: x_17_18 + x_18_24 + x_24_31 + x_13_17 - x_13_31 <= 3
 c185: x_23_29 + x_23_30 - x_29_31 + x_30_31 <= 2
 c186: x_17_18 + x_18_24 + x_14_17 - x_14_24 <= 2
 c187: x_17_18 + x_18_24 + x_24_31 + x_14_17 - x_14_31 <= 3
 c188: - x_7_11 + x_7_13 + x_11_13 <= 1
 c189: - x_7_12 + x_7_13 + x_12_13 <= 1
 c190: x_7_13 - x_7_15 + x_13_15 <= 1
 c191: x_7_13 - x_7_17 + x_13_17 <= 1
 c192: x_17_18 + x_7_13 - x_7_18 + x_13_17 <= 2
 c193: x_7_13 - x_7_19 + x_13_19 <= 1
 c194: x_7_13 - x_7_21 + x_13_21 <= 1
 c195: x_18_24 + x_15_18 - x_15_24 <= 1
 c196: x_18_24 + x_24_26 + x_25_26 + x_15_18 - x_15_25 <= 3
 c197: - x_23_27 + x_23_30 + x_27_30 <= 1
 c198: x_18_24 + x_24_27 + x_15_18 - x_15_27 <= 2
 c199: - x_23_28 + x_23_30 + x_25_28 + x_25_30 <= 2
 c200: x_18_24 + x_24_28 + x_15_18 - x_15_28 <= 2
 c201: x_18_24 + x_24_31 + x_15_18 - x_15_31 <= 2
 c202: x_0_1 - x_0_10 + x_1_10 <= 1
 c203: x_5_8 + x_5_10 - x_8_10 <= 1
 c204: - x_24_29 + x_24_31 + x_29_31 <= 1
 c205: x_3_9 + x_3_25 + x_24_26 - x_9_24 + x_25_26 <= 3
 c206: x_3_9 + x_3_25 - x_9_30 + x_25_31 + x_30_31 <= 3
 c207: - x_25_30 + x_25_31 + x_30_31 <= 1
 c208: x_6_10 + x_6_16 - x_10_16 <= 1
 c209: x_3_10 + x_3_25 + x_25_27 - x_10_27 <= 2
 c210: x_3_10 + x_3_25 + x_25_31 - x_10_30 + x_30_31 <= 3
 c211: - x_26_30 + x_26_31 + x_30_31 <= 1
 c212: x_3_10 + x_3_25 + x_25_31 - x_10_31 <= 2
 c213: x_3_25 - x_3_26 + x_25_26 <= 1
 c214: x_24_27 + x_24_31 - x_27_29 + x_29_31 <= 2
 c215: x_3_25 - x_3_30 + x_25_31 + x_30_31 <= 2
 c216: x_24_27 + x_24_31 - x_27_30 + x_30_31 <= 2
 c217: x_24_27 + x_24_31 - x_27_31 <= 1
 c218: - x_28_29 + x_28_31 + x_29_31 <= 1
 c219: x_3_4 + x_3_25 - x_4_30 + x_25_31 + x_30_31 <= 3
 c220: x_3_4 + x_3_25 - x_4_31 + x_25_31 <= 2
 c221: - x_14_15 + x_14_21 + x_15_21 <= 1
 c222: x_13_15 + x_13_20 - x_15_20 <= 1
 c223: x_3_7 + x_3_25 - x_7_26 + x_25_26 <= 2
 c224: - x_23_27 + x_23_29 + x_24_27 + x_24_31 + x_29_31 <= 3
 c225: x_3_7 + x_3_25 - x_7_28 + x_25_28 <= 2
 c226: - x_23_28 + x_23_29 + x_28_31 + x_29_31 <= 2
 c227: x_3_7 + x_3_25 - x_7_31 + x_25_31 <= 2
 c228: x_6_8 + x_6_10 - x_8_10 <= 1
 c229: x_5_6 - x_5_10 + x_6_10 <= 1
 c230: x_0_1 - x_0_7 + x_1_7 <= 1
 c231: x_0_1 + x_0_10 - x_1_10 <= 1
 c232: x_0_1 + x_0_10 - x_1_16 + x_10_16 <= 2
 c233: x_0_2 + x_0_10 - x_2_16 + x_10_16 <= 2
 c234: - x_6_9 + x_6_22 + x_9_22 <= 1
Bounds
 0 <= x_0_1 <= 1
 0 <= x_0_2 <= 1
 0 <= x_0_3 <= 1
 0 <= x_0_4 <= 1
 0 <= x_0_5 <= 1
 0 <= x_0_6 <= 1
 0 <= x_0_7 <= 1
 0 <= x_0_8 <= 1
 0 <= x_0_9 <= 1
 0 <= x_0_10 <= 1
 0 <= x_0_12 <= 1
 0 <= x_0_13 <= 1
 0 <= x_0_19 <= 1
 0 <= x_16_19 <= 1
 0 <= x_0_21 <= 1
 0 <= x_16_23 <= 1
 0 <= x_16_25 <= 1
 0 <= x_0_26 <= 1
 0 <= x_16_26 <= 1
 0 <= x_16_27 <= 1
 0 <= x_0_29 <= 1
 0 <= x_16_29 <= 1
 0 <= x_16_30 <= 1
 0 <= x_0_31 <= 1
 0 <= x_16_31 <= 1
 0 <= x_1_2 <= 1
 0 <= x_1_3 <= 1
 0 <= x_1_4 <= 1
 0 <= x_1_5 <= 1
 0 <= x_1_6 <= 1
 0 <= x_1_7 <= 1
 0 <= x_1_9 <= 1
 0 <= x_1_10 <= 1
 0 <= x_1_12 <= 1
 0 <= x_1_15 <= 1
 0 <= x_1_16 <= 1
 0 <= x_17_18 <= 1
 0 <= x_17_19 <= 1
 0 <= x_17_20 <= 1
 0 <= x_1_21 <= 1
 0 <= x_17_21 <= 1
 0 <= x_1_24 <= 1
 0 <= x_1_27 <= 1
 0 <= x_17_27 <= 1
 0 <= x_17_30 <= 1
 0 <= x_2_3 <= 1
 0 <= x_2_4 <= 1
 0 <= x_2_5 <= 1
 0 <= x_2_6 <= 1
 0 <= x_2_9 <= 1
 0 <= x_2_12 <= 1
 0 <= x_2_13 <= 1
 0 <= x_2_16 <= 1
 0 <= x_18_19 <= 1
 0 <= x_18_21 <= 1
 0 <= x_18_22 <= 1
 0 <= x_18_24 <= 1
 0 <= x_18_25 <= 1
 0 <= x_18_27 <= 1
 0 <= x_2_28 <= 1
 0 <= x_18_29 <= 1
 0 <= x_18_30 <= 1
 0 <= x_3_4 <= 1
 0 <= x_3_5 <= 1
 0 <= x_3_6 <= 1
 0 <= x_3_7 <= 1
 0 <= x_3_8 <= 1
 0 <= x_3_9 <= 1
 0 <= x_3_10 <= 1
 0 <= x_3_11 <= 1
 0 <= x_3_17 <= 1
 0 <= x_19_20 <= 1
 0 <= x_3_23 <= 1
 0 <= x_3_25 <= 1
 0 <= x_3_26 <= 1
 0 <= x_3_30 <= 1
 0 <= x_4_7 <= 1
 0 <= x_4_9 <= 1
 0 <= x_4_10 <= 1
 0 <= x_4_13 <= 1
 0 <= x_4_14 <= 1
 0 <= x_4_15 <= 1
 0 <= x_4_17 <= 1
 0 <= x_4_19 <= 1
 0 <= x_4_22 <= 1
 0 <= x_4_23 <= 1
 0 <= x_20_26 <= 1
 0 <= x_4_29 <= 1
 0 <= x_20_29 <= 1
 0 <= x_4_30 <= 1
 0 <= x_20_30 <= 1
 0 <= x_4_31 <= 1
 0 <= x_5_6 <= 1
 0 <= x_5_8 <= 1
 0 <= x_5_9 <= 1
 0 <= x_5_10 <= 1
 0 <= x_5_22 <= 1
 0 <= x_21_23 <= 1
 0 <= x_21_24 <= 1
 0 <= x_5_25 <= 1
 0 <= x_5_26 <= 1
 0 <= x_21_27 <= 1
 0 <= x_5_31 <= 1
 0 <= x_21_31 <= 1
 0 <= x_6_7 <= 1
 0 <= x_6_8 <= 1
 0 <= x_6_9 <= 1
 0 <= x_6_10 <= 1
 0 <= x_6_13 <= 1
 0 <= x_6_15 <= 1
 0 <= x_6_16 <= 1
 0 <= x_6_17 <= 1
 0 <= x_6_18 <= 1
 0 <= x_6_21 <= 1
 0 <= x_6_22 <= 1
 0 <= x_6_23 <= 1
 0 <= x_22_23 <= 1
 0 <= x_22_24 <= 1
 0 <= x_22_25 <= 1
 0 <= x_6_26 <= 1
 0 <= x_22_26 <= 1
 0 <= x_6_27 <= 1
 0 <= x_6_28 <= 1
 0 <= x_22_28 <= 1
 0 <= x_22_29 <= 1
 0 <= x_7_10 <= 1
 0 <= x_7_11 <= 1
 0 <= x_7_12 <= 1
 0 <= x_7_13 <= 1
 0 <= x_7_15 <= 1
 0 <= x_7_17 <= 1
 0 <= x_7_18 <= 1
 0 <= x_7_19 <= 1
 0 <= x_7_21 <= 1
 0 <= x_7_23 <= 1
 0 <= x_7_26 <= 1
 0 <= x_23_27 <= 1
 0 <= x_7_28 <= 1
 0 <= x_23_28 <= 1
 0 <= x_23_29 <= 1
 0 <= x_23_30 <= 1
 0 <= x_7_31 <= 1
 0 <= x_8_9 <= 1
 0 <= x_8_10 <= 1
 0 <= x_8_12 <= 1
 0 <= x_8_14 <= 1
 0 <= x_8_16 <= 1
 0 <= x_8_21 <= 1
 0 <= x_24_26 <= 1
 0 <= x_24_27 <= 1
 0 <= x_8_28 <= 1
 0 <= x_24_28 <= 1
 0 <= x_8_29 <= 1
 0 <= x_24_29 <= 1
 0 <= x_8_30 <= 1
 0 <= x_8_31 <= 1
 0 <= x_24_31 <= 1
 0 <= x_9_11 <= 1
 0 <= x_9_12 <= 1
 0 <= x_9_13 <= 1
 0 <= x_9_21 <= 1
 0 <= x_9_22 <= 1
 0 <= x_9_23 <= 1
 0 <= x_9_24 <= 1
 0 <= x_25_26 <= 1
 0 <= x_25_27 <= 1
 0 <= x_25_28 <= 1
 0 <= x_9_30 <= 1
 0 <= x_25_30 <= 1
 0 <= x_25_31 <= 1
 0 <= x_10_11 <= 1
 0 <= x_10_14 <= 1
 0 <= x_10_16 <= 1
 0 <= x_10_17 <= 1
 0 <= x_10_21 <= 1
 0 <= x_10_22 <= 1
 0 <= x_10_27 <= 1
 0 <= x_26_27 <= 1
 0 <= x_26_28 <= 1
 0 <= x_10_30 <= 1
 0 <= x_26_30 <= 1
 0 <= x_10_31 <= 1
 0 <= x_26_31 <= 1
 0 <= x_11_12 <= 1
 0 <= x_11_13 <= 1
 0 <= x_11_15 <= 1
 0 <= x_11_16 <= 1
 0 <= x_11_17 <= 1
 0 <= x_11_18 <= 1
 0 <= x_11_19 <= 1
 0 <= x_11_21 <= 1
 0 <= x_11_25 <= 1
 0 <= x_27_28 <= 1
 0 <= x_27_29 <= 1
 0 <= x_27_30 <= 1
 0 <= x_27_31 <= 1
 0 <= x_12_13 <= 1
 0 <= x_12_14 <= 1
 0 <= x_12_16 <= 1
 0 <= x_12_17 <= 1
 0 <= x_12_18 <= 1
 0 <= x_12_19 <= 1
 0 <= x_12_20 <= 1
 0 <= x_12_21 <= 1
 0 <= x_12_22 <= 1
 0 <= x_12_24 <= 1
 0 <= x_12_27 <= 1
 0 <= x_28_29 <= 1
 0 <= x_12_30 <= 1
 0 <= x_28_31 <= 1
 0 <= x_13_15 <= 1
 0 <= x_13_16 <= 1
 0 <= x_13_17 <= 1
 0 <= x_13_18 <= 1
 0 <= x_13_19 <= 1
 0 <= x_13_20 <= 1
 0 <= x_13_21 <= 1
 0 <= x_13_23 <= 1
 0 <= x_13_24 <= 1
 0 <= x_13_25 <= 1
 0 <= x_13_26 <= 1
 0 <= x_13_27 <= 1
 0 <= x_13_31 <= 1
 0 <= x_29_31 <= 1
 0 <= x_14_15 <= 1
 0 <= x_14_16 <= 1
 0 <= x_14_17 <= 1
 0 <= x_14_19 <= 1
 0 <= x_14_20 <= 1
 0 <= x_14_21 <= 1
 0 <= x_14_22 <= 1
 0 <= x_14_23 <= 1
 0 <= x_14_24 <= 1
 0 <= x_14_31 <= 1
 0 <= x_30_31 <= 1
 0 <= x_15_16 <= 1
 0 <= x_15_18 <= 1
 0 <= x_15_20 <= 1
 0 <= x_15_21 <= 1
 0 <= x_15_24 <= 1
 0 <= x_15_25 <= 1
 0 <= x_15_27 <= 1
 0 <= x_15_28 <= 1
 0 <= x_15_31 <= 1
End
