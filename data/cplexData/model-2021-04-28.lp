\* Crop Rotation 2021 *\

minimize
Z:
 + 1000 ud_1 + 1000 ud_2 + 1000 ud_3 + 1000 ud_4 + 1000 ud_5 + 1000 ud_6 + 1000 ud_7 + 1000 ud_8

Subject to

 c_uf_1: + 1 uf_1  <= 1
 c_uf_2: + 1 uf_2  <= 1
 c_uf_3: - 1 uf_3 + 1 uf_89  = 0
 c_uf_4: - 1 uf_4 + 1 uf_90  = 0
 c_uf_5: - 1 uf_5 + 1 uf_91  = 0
 c_uf_6: - 1 uf_6 + 1 uf_92  = 0
 c_uf_7: - 1 uf_7 + 1 uf_93  = 0
 c_uf_8: - 1 uf_8 + 1 uf_94  = 0
 c_uf_9: + 1 uf_1 - 1 uf_9  = 0
 c_uf_10: + 1 uf_2 - 1 uf_10  = 0
 c_uf_11: + 1 uf_3 - 1 uf_11  = 0
 c_uf_12: + 1 uf_4 - 1 uf_12  = 0
 c_uf_13: + 1 uf_5 - 1 uf_13  = 0
 c_uf_14: + 1 uf_6 - 1 uf_14  = 0
 c_uf_15: + 1 uf_7 - 1 uf_15  = 0
 c_uf_16: + 1 uf_8 - 1 uf_16  = 0
 c_uf_17: + 1 uf_9 - 1 uf_17  = 0
 c_uf_18: + 1 uf_10 - 1 uf_18  = 0
 c_uf_19: + 1 uf_11 - 1 uf_19  = 0
 c_uf_20: + 1 uf_12 - 1 uf_20  = 0
 c_uf_21: + 1 uf_13 - 1 uf_21  = 0
 c_uf_22: + 1 uf_14 - 1 uf_22  = 0
 c_uf_23: + 1 uf_15 - 1 uf_23  = 0
 c_uf_24: + 1 uf_16 - 1 uf_24  = 0
 c_uf_25: + 1 cpmy_1 + 1 cpmy_2 + 1 uf_17 - 1 uf_25  = 0
 c_uf_26: + 1 cpmy_25 + 1 cpmy_26 + 1 uf_18 - 1 uf_26  = 0
 c_uf_27: + 1 cpmy_7 + 1 cpmy_8 + 1 uf_19 - 1 uf_27  = 0
 c_uf_28: + 1 cpmy_31 + 1 cpmy_32 + 1 uf_20 - 1 uf_28  = 0
 c_uf_29: + 1 cpmy_13 + 1 cpmy_14 + 1 uf_21 - 1 uf_29  = 0
 c_uf_30: + 1 cpmy_37 + 1 cpmy_38 + 1 uf_22 - 1 uf_30  = 0
 c_uf_31: + 1 cpmy_19 + 1 cpmy_20 + 1 uf_23 - 1 uf_31  = 0
 c_uf_32: + 1 cpmy_43 + 1 cpmy_44 + 1 uf_24 - 1 uf_32  = 0
 c_uf_33: + 1 cpmy_3 + 1 cpmy_4 + 1 uf_25 - 1 uf_33  = 0
 c_uf_34: + 1 cpmy_27 + 1 cpmy_28 + 1 uf_26 - 1 uf_34  = 0
 c_uf_35: + 1 cpmy_9 + 1 cpmy_10 + 1 uf_27 - 1 uf_35  = 0
 c_uf_36: + 1 cpmy_33 + 1 cpmy_34 + 1 uf_28 - 1 uf_36  = 0
 c_uf_37: + 1 cpmy_15 + 1 cpmy_16 + 1 uf_29 - 1 uf_37  = 0
 c_uf_38: + 1 cpmy_39 + 1 cpmy_40 + 1 uf_30 - 1 uf_38  = 0
 c_uf_39: + 1 cpmy_21 + 1 cpmy_22 + 1 uf_31 - 1 uf_39  = 0
 c_uf_40: + 1 cpmy_45 + 1 cpmy_46 + 1 uf_32 - 1 uf_40  = 0
 c_uf_41: - 1 cpmy_1 + 1 cpmy_5 + 1 uf_33 - 1 uf_41  = 0
 c_uf_42: - 1 cpmy_25 + 1 cpmy_29 + 1 uf_34 - 1 uf_42  = 0
 c_uf_43: - 1 cpmy_7 + 1 cpmy_11 + 1 uf_35 - 1 uf_43  = 0
 c_uf_44: - 1 cpmy_31 + 1 cpmy_35 + 1 uf_36 - 1 uf_44  = 0
 c_uf_45: - 1 cpmy_13 + 1 cpmy_17 + 1 uf_37 - 1 uf_45  = 0
 c_uf_46: - 1 cpmy_37 + 1 cpmy_41 + 1 uf_38 - 1 uf_46  = 0
 c_uf_47: - 1 cpmy_19 + 1 cpmy_23 + 1 uf_39 - 1 uf_47  = 0
 c_uf_48: - 1 cpmy_43 + 1 cpmy_47 + 1 uf_40 - 1 uf_48  = 0
 c_uf_49: - 1 cpmy_3 + 1 cpmy_6 + 1 uf_41 - 1 uf_49  = 0
 c_uf_50: - 1 cpmy_27 + 1 cpmy_30 + 1 uf_42 - 1 uf_50  = 0
 c_uf_51: - 1 cpmy_9 + 1 cpmy_12 + 1 uf_43 - 1 uf_51  = 0
 c_uf_52: - 1 cpmy_33 + 1 cpmy_36 + 1 uf_44 - 1 uf_52  = 0
 c_uf_53: - 1 cpmy_15 + 1 cpmy_18 + 1 uf_45 - 1 uf_53  = 0
 c_uf_54: - 1 cpmy_39 + 1 cpmy_42 + 1 uf_46 - 1 uf_54  = 0
 c_uf_55: - 1 cpmy_21 + 1 cpmy_24 + 1 uf_47 - 1 uf_55  = 0
 c_uf_56: - 1 cpmy_45 + 1 cpmy_48 + 1 uf_48 - 1 uf_56  = 0
 c_uf_57: - 1 cpmy_2 + 1 uf_49 - 1 uf_57  = 0
 c_uf_58: - 1 cpmy_26 + 1 uf_50 - 1 uf_58  = 0
 c_uf_59: - 1 cpmy_8 + 1 uf_51 - 1 uf_59  = 0
 c_uf_60: - 1 cpmy_32 + 1 uf_52 - 1 uf_60  = 0
 c_uf_61: - 1 cpmy_14 + 1 uf_53 - 1 uf_61  = 0
 c_uf_62: - 1 cpmy_38 + 1 uf_54 - 1 uf_62  = 0
 c_uf_63: - 1 cpmy_20 + 1 uf_55 - 1 uf_63  = 0
 c_uf_64: - 1 cpmy_44 + 1 uf_56 - 1 uf_64  = 0
 c_uf_65: - 1 cpmy_4 + 1 uf_57 - 1 uf_65  = 0
 c_uf_66: - 1 cpmy_28 + 1 uf_58 - 1 uf_66  = 0
 c_uf_67: - 1 cpmy_10 + 1 uf_59 - 1 uf_67  = 0
 c_uf_68: - 1 cpmy_34 + 1 uf_60 - 1 uf_68  = 0
 c_uf_69: - 1 cpmy_16 + 1 uf_61 - 1 uf_69  = 0
 c_uf_70: - 1 cpmy_40 + 1 uf_62 - 1 uf_70  = 0
 c_uf_71: - 1 cpmy_22 + 1 uf_63 - 1 uf_71  = 0
 c_uf_72: - 1 cpmy_46 + 1 uf_64 - 1 uf_72  = 0
 c_uf_73: - 1 cpmy_5 + 1 uf_65 - 1 uf_73  = 0
 c_uf_74: - 1 cpmy_29 + 1 uf_66 - 1 uf_74  = 0
 c_uf_75: - 1 cpmy_11 + 1 uf_67 - 1 uf_75  = 0
 c_uf_76: - 1 cpmy_35 + 1 uf_68 - 1 uf_76  = 0
 c_uf_77: - 1 cpmy_17 + 1 uf_69 - 1 uf_77  = 0
 c_uf_78: - 1 cpmy_41 + 1 uf_70 - 1 uf_78  = 0
 c_uf_79: - 1 cpmy_23 + 1 uf_71 - 1 uf_79  = 0
 c_uf_80: - 1 cpmy_47 + 1 uf_72 - 1 uf_80  = 0
 c_uf_81: - 1 cpmy_6 + 1 uf_73 - 1 uf_81  = 0
 c_uf_82: - 1 cpmy_30 + 1 uf_74 - 1 uf_82  = 0
 c_uf_83: - 1 cpmy_12 + 1 uf_75 - 1 uf_83  = 0
 c_uf_84: - 1 cpmy_36 + 1 uf_76 - 1 uf_84  = 0
 c_uf_85: - 1 cpmy_18 + 1 uf_77 - 1 uf_85  = 0
 c_uf_86: - 1 cpmy_42 + 1 uf_78 - 1 uf_86  = 0
 c_uf_87: - 1 cpmy_24 + 1 uf_79 - 1 uf_87  = 0
 c_uf_88: - 1 cpmy_48 + 1 uf_80 - 1 uf_88  = 0
 c_uf_89: + 1 uf_81 - 1 uf_89  = 0
 c_uf_90: + 1 uf_82 - 1 uf_90  = 0
 c_uf_91: + 1 uf_83 - 1 uf_91  = 0
 c_uf_92: + 1 uf_84 - 1 uf_92  = 0
 c_uf_93: + 1 uf_85 - 1 uf_93  = 0
 c_uf_94: + 1 uf_86 - 1 uf_94  = 0
 c_uf_95: + 1 uf_87 - 1 uf_95  = 0
 c_uf_96: + 1 uf_88 - 1 uf_96  = 0

 c_ud_1: + 18.75 cpmy_1 + 18.75 cpmy_3 + 18.75 cpmy_25 + 18.75 cpmy_27 -1 ud_1  = 30
 c_ud_2: + 62.5 cpmy_2 + 62.5 cpmy_4 + 62.5 cpmy_5 + 62.5 cpmy_6 + 62.5 cpmy_26 + 62.5 cpmy_28 + 62.5 cpmy_29 + 62.5 cpmy_30 -1 ud_2  = 20
 c_ud_3: + 18.75 cpmy_7 + 18.75 cpmy_9 + 18.75 cpmy_31 + 18.75 cpmy_33 -1 ud_3  = 30
 c_ud_4: + 62.5 cpmy_8 + 62.5 cpmy_10 + 62.5 cpmy_11 + 62.5 cpmy_12 + 62.5 cpmy_32 + 62.5 cpmy_34 + 62.5 cpmy_35 + 62.5 cpmy_36 -1 ud_4  = 20
 c_ud_5: + 18.75 cpmy_13 + 18.75 cpmy_15 + 18.75 cpmy_37 + 18.75 cpmy_39 -1 ud_5  = 30
 c_ud_6: + 62.5 cpmy_14 + 62.5 cpmy_16 + 62.5 cpmy_17 + 62.5 cpmy_18 + 62.5 cpmy_38 + 62.5 cpmy_40 + 62.5 cpmy_41 + 62.5 cpmy_42 -1 ud_6  = 20
 c_ud_7: + 18.75 cpmy_19 + 18.75 cpmy_21 + 18.75 cpmy_43 + 18.75 cpmy_45 -1 ud_7  = 30
 c_ud_8: + 62.5 cpmy_20 + 62.5 cpmy_22 + 62.5 cpmy_23 + 62.5 cpmy_24 + 62.5 cpmy_44 + 62.5 cpmy_46 + 62.5 cpmy_47 + 62.5 cpmy_48 -1 ud_8  = 20

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
 cpmy_17
 cpmy_18
 cpmy_19
 cpmy_20
 cpmy_21
 cpmy_22
 cpmy_23
 cpmy_24
 cpmy_25
 cpmy_26
 cpmy_27
 cpmy_28
 cpmy_29
 cpmy_30
 cpmy_31
 cpmy_32
 cpmy_33
 cpmy_34
 cpmy_35
 cpmy_36
 cpmy_37
 cpmy_38
 cpmy_39
 cpmy_40
 cpmy_41
 cpmy_42
 cpmy_43
 cpmy_44
 cpmy_45
 cpmy_46
 cpmy_47
 cpmy_48
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

general
 ud_1
 ud_2
 ud_3
 ud_4
 ud_5
 ud_6
 ud_7
 ud_8
