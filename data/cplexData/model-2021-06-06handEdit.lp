\* Crop Rotation 2021 *\

minimize
Z:
 + 1000 ud_1 + 1000 ud_2 + 1000 ud_3 + 1000 ud_4 + 1000 ud_5 + 1000 ud_6 + 1000 ud_7 + 1000 ud_8
 + 500 rr_1 + 500 rr_2 + 500 rr_3 + 500 rr_4 + 500 rr_5 + 500 rr_6 + 500 rr_7 + 500 rr_8 + 500 rr_9 + 500 rr_10
 + 500 rr_11 + 500 rr_12 + 500 rr_13 + 500 rr_14 + 500 rr_15 + 500 rr_16

Subject to

 c_uf_1: + 1 uf_1  <= 1
 c_uf_2: + 1 uf_2  <= 1
 c_uf_3: + 1 uf_3 - 1 uf_89  <= 0
 c_uf_4: + 1 uf_4 - 1 uf_90  <= 0
 c_uf_5: + 1 uf_5 - 1 uf_91  <= 0
 c_uf_6: + 1 uf_6 - 1 uf_92  <= 0
 c_uf_7: + 1 uf_7 - 1 uf_93  <= 0
 c_uf_8: + 1 uf_8 - 1 uf_94  <= 0
 c_uf_9: - 1 uf_1 + 1 uf_9  <= 0
 c_uf_10: - 1 uf_2 + 1 uf_10  <= 0
 c_uf_11: - 1 uf_3 + 1 uf_11  <= 0
 c_uf_12: - 1 uf_4 + 1 uf_12  <= 0
 c_uf_13: - 1 uf_5 + 1 uf_13  <= 0
 c_uf_14: - 1 uf_6 + 1 uf_14  <= 0
 c_uf_15: - 1 uf_7 + 1 uf_15  <= 0
 c_uf_16: - 1 uf_8 + 1 uf_16  <= 0
 c_uf_17: + 1 cpmy_1 - 1 uf_9 + 1 uf_17  <= 0
 c_uf_18: + 1 cpmy_9 - 1 uf_10 + 1 uf_18  <= 0
 c_uf_19: + 1 cpmy_3 - 1 uf_11 + 1 uf_19  <= 0
 c_uf_20: + 1 cpmy_11 - 1 uf_12 + 1 uf_20  <= 0
 c_uf_21: + 1 cpmy_5 - 1 uf_13 + 1 uf_21  <= 0
 c_uf_22: + 1 cpmy_13 - 1 uf_14 + 1 uf_22  <= 0
 c_uf_23: + 1 cpmy_7 - 1 uf_15 + 1 uf_23  <= 0
 c_uf_24: + 1 cpmy_15 - 1 uf_16 + 1 uf_24  <= 0
 c_uf_25: + 1 cpmy_2 - 1 uf_17 + 1 uf_25  <= 0
 c_uf_26: + 1 cpmy_10 - 1 uf_18 + 1 uf_26  <= 0
 c_uf_27: + 1 cpmy_4 - 1 uf_19 + 1 uf_27  <= 0
 c_uf_28: + 1 cpmy_12 - 1 uf_20 + 1 uf_28  <= 0
 c_uf_29: + 1 cpmy_6 - 1 uf_21 + 1 uf_29  <= 0
 c_uf_30: + 1 cpmy_14 - 1 uf_22 + 1 uf_30  <= 0
 c_uf_31: + 1 cpmy_8 - 1 uf_23 + 1 uf_31  <= 0
 c_uf_32: + 1 cpmy_16 - 1 uf_24 + 1 uf_32  <= 0
 c_uf_33: - 1 cpmy_1 - 1 uf_25 + 1 uf_33  <= 0
 c_uf_34: - 1 cpmy_9 - 1 uf_26 + 1 uf_34  <= 0
 c_uf_35: - 1 cpmy_3 - 1 uf_27 + 1 uf_35  <= 0
 c_uf_36: - 1 cpmy_11 - 1 uf_28 + 1 uf_36  <= 0
 c_uf_37: - 1 cpmy_5 - 1 uf_29 + 1 uf_37  <= 0
 c_uf_38: - 1 cpmy_13 - 1 uf_30 + 1 uf_38  <= 0
 c_uf_39: - 1 cpmy_7 - 1 uf_31 + 1 uf_39  <= 0
 c_uf_40: - 1 cpmy_15 - 1 uf_32 + 1 uf_40  <= 0
 c_uf_41: - 1 uf_33 + 1 uf_41  <= 0
 c_uf_42: - 1 uf_34 + 1 uf_42  <= 0
 c_uf_43: - 1 uf_35 + 1 uf_43  <= 0
 c_uf_44: - 1 uf_36 + 1 uf_44  <= 0
 c_uf_45: - 1 uf_37 + 1 uf_45  <= 0
 c_uf_46: - 1 uf_38 + 1 uf_46  <= 0
 c_uf_47: - 1 uf_39 + 1 uf_47  <= 0
 c_uf_48: - 1 uf_40 + 1 uf_48  <= 0
 c_uf_49: - 1 uf_41 + 1 uf_49  <= 0
 c_uf_50: - 1 uf_42 + 1 uf_50  <= 0
 c_uf_51: - 1 uf_43 + 1 uf_51  <= 0
 c_uf_52: - 1 uf_44 + 1 uf_52  <= 0
 c_uf_53: - 1 uf_45 + 1 uf_53  <= 0
 c_uf_54: - 1 uf_46 + 1 uf_54  <= 0
 c_uf_55: - 1 uf_47 + 1 uf_55  <= 0
 c_uf_56: - 1 uf_48 + 1 uf_56  <= 0
 c_uf_57: - 1 cpmy_2 - 1 uf_49 + 1 uf_57  <= 0
 c_uf_58: - 1 cpmy_10 - 1 uf_50 + 1 uf_58  <= 0
 c_uf_59: - 1 cpmy_4 - 1 uf_51 + 1 uf_59  <= 0
 c_uf_60: - 1 cpmy_12 - 1 uf_52 + 1 uf_60  <= 0
 c_uf_61: - 1 cpmy_6 - 1 uf_53 + 1 uf_61  <= 0
 c_uf_62: - 1 cpmy_14 - 1 uf_54 + 1 uf_62  <= 0
 c_uf_63: - 1 cpmy_8 - 1 uf_55 + 1 uf_63  <= 0
 c_uf_64: - 1 cpmy_16 - 1 uf_56 + 1 uf_64  <= 0
 c_uf_65: - 1 uf_57 + 1 uf_65  <= 0
 c_uf_66: - 1 uf_58 + 1 uf_66  <= 0
 c_uf_67: - 1 uf_59 + 1 uf_67  <= 0
 c_uf_68: - 1 uf_60 + 1 uf_68  <= 0
 c_uf_69: - 1 uf_61 + 1 uf_69  <= 0
 c_uf_70: - 1 uf_62 + 1 uf_70  <= 0
 c_uf_71: - 1 uf_63 + 1 uf_71  <= 0
 c_uf_72: - 1 uf_64 + 1 uf_72  <= 0
 c_uf_73: - 1 uf_65 + 1 uf_73  <= 0
 c_uf_74: - 1 uf_66 + 1 uf_74  <= 0
 c_uf_75: - 1 uf_67 + 1 uf_75  <= 0
 c_uf_76: - 1 uf_68 + 1 uf_76  <= 0
 c_uf_77: - 1 uf_69 + 1 uf_77  <= 0
 c_uf_78: - 1 uf_70 + 1 uf_78  <= 0
 c_uf_79: - 1 uf_71 + 1 uf_79  <= 0
 c_uf_80: - 1 uf_72 + 1 uf_80  <= 0
 c_uf_81: - 1 uf_73 + 1 uf_81  <= 0
 c_uf_82: - 1 uf_74 + 1 uf_82  <= 0
 c_uf_83: - 1 uf_75 + 1 uf_83  <= 0
 c_uf_84: - 1 uf_76 + 1 uf_84  <= 0
 c_uf_85: - 1 uf_77 + 1 uf_85  <= 0
 c_uf_86: - 1 uf_78 + 1 uf_86  <= 0
 c_uf_87: - 1 uf_79 + 1 uf_87  <= 0
 c_uf_88: - 1 uf_80 + 1 uf_88  <= 0
 c_uf_89: - 1 uf_81 + 1 uf_89  <= 0
 c_uf_90: - 1 uf_82 + 1 uf_90  <= 0
 c_uf_91: - 1 uf_83 + 1 uf_91  <= 0
 c_uf_92: - 1 uf_84 + 1 uf_92  <= 0
 c_uf_93: - 1 uf_85 + 1 uf_93  <= 0
 c_uf_94: - 1 uf_86 + 1 uf_94  <= 0
 c_uf_95: - 1 uf_87 + 1 uf_95  <= 0
 c_uf_96: - 1 uf_88 + 1 uf_96  <= 0

 c_ud_1: + 30 cpmy_2 + 30 cpmy_10 + 1 ud_1  >= 30
 c_ud_2: + 50 cpmy_1 + 50 cpmy_9 + 1 ud_2  >= 250
 c_ud_3: + 30 cpmy_4 + 30 cpmy_12 + 1 ud_3  >= 30
 c_ud_4: + 50 cpmy_3 + 50 cpmy_11 + 1 ud_4  >= 250
 c_ud_5: + 30 cpmy_6 + 30 cpmy_14 + 1 ud_5  >= 30
 c_ud_6: + 50 cpmy_5 + 50 cpmy_13 + 1 ud_6  >= 250
 c_ud_7: + 30 cpmy_8 + 30 cpmy_16 + 1 ud_7  >= 30
 c_ud_8: + 50 cpmy_7 + 50 cpmy_15 + 1 ud_8  >= 250

 c_cpmy_1_1_Mar_1:  +1 cpmy_1  -1 dr_1  +1 rr_1  <= 0
 c_cpmy_1_1_Mar_2:  +1 cpmy_2  +1 dr_1  +1 rr_1  <= 1
 c_cpmy_1_1_Mar_13:  +1 cpmy_3  +1 dr_1  +1 rr_1  <= 1
 c_cpmy_1_1_Mar_14:  +1 cpmy_4  +1 dr_1  +1 rr_1  <= 1
 
 c_cpmy_2_1_Apr_1:  +1 cpmy_2  -1 dr_2  +1 rr_2  <= 0
 c_cpmy_2_1_Apr_12:  +1 cpmy_3  +1 dr_2  +1 rr_2  <= 0
 c_cpmy_2_1_Apr_13:  +1 cpmy_4  +1 dr_2  +1 rr_2  <= 0

 c_cpmy_3_2_Mar_1:  +1 cpmy_3  -1 dr_3  +1 rr_3  <= 0
 c_cpmy_3_2_Mar_2:  +1 cpmy_4  +1 dr_3  +1 rr_3  <= 1
 c_cpmy_3_2_Mar_13:  +1 cpmy_5  +1 dr_3  +1 rr_3  <= 1
 c_cpmy_3_2_Mar_14:  +1 cpmy_6  +1 dr_3  +1 rr_3  <= 1

 c_cpmy_4_2_Apr_1:  +1 cpmy_4  -1 dr_4  +1 rr_4  <= 0
 c_cpmy_4_2_Apr_12:  +1 cpmy_5  +1 dr_4  +1 rr_4  <= 1
 c_cpmy_4_2_Apr_13:  +1 cpmy_6  +1 dr_4  +1 rr_4  <= 1
 
 c_cpmy_5_3_Mar_1:  +1 cpmy_5  -1 dr_5  +1 rr_5  <= 0
 c_cpmy_5_3_Mar_2:  +1 cpmy_6  +1 dr_5  +1 rr_5  <= 1
 c_cpmy_5_3_Mar_13:  +1 cpmy_7  +1 dr_5  +1 rr_5  <= 1
 c_cpmy_5_3_Mar_14:  +1 cpmy_8  +1 dr_5  +1 rr_5  <= 1
 
 c_cpmy_6_3_Apr_1:  +1 cpmy_6  -1 dr_5  +1 rr_5  <= 0
 c_cpmy_6_3_Apr_12:  +1 cpmy_7  +1 dr_5  +1 rr_5  <= 1
 c_cpmy_6_3_Apr_13:  +1 cpmy_8  +1 dr_5  +1 rr_5  <= 1

 c_cpmy_7_4_Mar_1:  +1 cpmy_7  -1 dr_7  +1 rr_7  <= 0
 c_cpmy_7_4_Mar_2:  +1 cpmy_8  +1 dr_7  +1 rr_7  <= 1

 c_cpmy_8_4_Apr_1:  +1 cpmy_8  -1 dr_8  +1 rr_8  <= 0
 
 c_cpmy_9_1_Mar_1:  +1 cpmy_9  -1 dr_9  +1 rr_9  <= 0
 c_cpmy_9_1_Mar_2:  +1 cpmy_10  +1 dr_9  +1 rr_9  <= 1
 c_cpmy_9_1_Mar_13:  +1 cpmy_11  +1 dr_9  +1 rr_9  <= 1
 c_cpmy_9_1_Mar_14:  +1 cpmy_12  +1 dr_9  +1 rr_9  <= 1

 c_cpmy_10_1_Apr_1:  +1 cpmy_10  -1 dr_10  +1 rr_10  <= 0
 c_cpmy_10_1_Apr_12:  +1 cpmy_11  +1 dr_10  +1 rr_10  <= 0
 c_cpmy_10_1_Apr_13:  +1 cpmy_12  +1 dr_10  +1 rr_10  <= 0

 c_cpmy_11_2_Mar_1:  +1 cpmy_11  -1 dr_11  +1 rr_11  <= 0
 c_cpmy_11_2_Mar_2:  +1 cpmy_12  +1 dr_11  +1 rr_11  <= 1
 c_cpmy_11_2_Mar_13:  +1 cpmy_13  +1 dr_11  +1 rr_11  <= 1
 c_cpmy_11_2_Mar_14:  +1 cpmy_14  +1 dr_11  +1 rr_11  <= 1

 c_cpmy_12_2_Apr_1:  +1 cpmy_12  -1 dr_12  +1 rr_12  <= 0
 c_cpmy_12_2_Apr_12:  +1 cpmy_13  +1 dr_12  +1 rr_12  <= 1
 c_cpmy_12_2_Apr_13:  +1 cpmy_14  +1 dr_12  +1 rr_12  <= 1

 c_cpmy_13_3_Mar_1:  +1 cpmy_13  -1 dr_13  +1 rr_13  <= 0
 c_cpmy_13_3_Mar_2:  +1 cpmy_14  +1 dr_13  +1 rr_13  <= 1
 c_cpmy_13_3_Mar_13:  +1 cpmy_15  +1 dr_13  +1 rr_13  <= 1
 c_cpmy_13_3_Mar_14:  +1 cpmy_16  +1 dr_13  +1 rr_13  <= 1

 c_cpmy_14_3_Apr_1:  +1 cpmy_14  -1 dr_14  +1 rr_14  <= 0
 c_cpmy_14_3_Apr_12:  +1 cpmy_15  +1 dr_14  +1 rr_14  <= 1
 c_cpmy_14_3_Apr_13:  +1 cpmy_16  +1 dr_14  +1 rr_14  <= 1

 c_cpmy_15_4_Mar_1:  +1 cpmy_15  -1 dr_15  +1 rr_15  <= 0
 c_cpmy_15_4_Mar_2:  +1 cpmy_16  +1 dr_15  +1 rr_15  <= 1

 c_cpmy_16_4_Apr_1:  +1 cpmy_16  -1 dr_16  +1 rr_16  <= 0

binary
 cpmy_1
 cpmy_2
 cpmy_3
 cpmy_4
 cpmy_5
 cpmy_6
 cpmy_7
 cpmy_8
 cpmy_9
 cpmy_10
 cpmy_11
 cpmy_12
 cpmy_13
 cpmy_14
 cpmy_15
 cpmy_16
 uf_1
 uf_2
 uf_3
 uf_4
 uf_5
 uf_6
 uf_7
 uf_8
 uf_9
 uf_10
 uf_11
 uf_12
 uf_13
 uf_14
 uf_15
 uf_16
 uf_17
 uf_18
 uf_19
 uf_20
 uf_21
 uf_22
 uf_23
 uf_24
 uf_25
 uf_26
 uf_27
 uf_28
 uf_29
 uf_30
 uf_31
 uf_32
 uf_33
 uf_34
 uf_35
 uf_36
 uf_37
 uf_38
 uf_39
 uf_40
 uf_41
 uf_42
 uf_43
 uf_44
 uf_45
 uf_46
 uf_47
 uf_48
 uf_49
 uf_50
 uf_51
 uf_52
 uf_53
 uf_54
 uf_55
 uf_56
 uf_57
 uf_58
 uf_59
 uf_60
 uf_61
 uf_62
 uf_63
 uf_64
 uf_65
 uf_66
 uf_67
 uf_68
 uf_69
 uf_70
 uf_71
 uf_72
 uf_73
 uf_74
 uf_75
 uf_76
 uf_77
 uf_78
 uf_79
 uf_80
 uf_81
 uf_82
 uf_83
 uf_84
 uf_85
 uf_86
 uf_87
 uf_88
 uf_89
 uf_90
 uf_91
 uf_92
 uf_93
 uf_94
 uf_95
 uf_96
 dr_1
 dr_2
 dr_3
 dr_4
 dr_5
 dr_6
 dr_7
 dr_8
 dr_9
 dr_10
 dr_11
 dr_12
 dr_13
 dr_14
 dr_15
 dr_16

general
 ud_1
 ud_2
 ud_3
 ud_4
 ud_5
 ud_6
 ud_7
 ud_8
 rr_1
 rr_2
 rr_3
 rr_4
 rr_5
 rr_6
 rr_7
 rr_8
 rr_9
 rr_10
 rr_11
 rr_12
 rr_13
 rr_14
 rr_15
 rr_16

end

