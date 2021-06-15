\* Crop Rotation: 15 Crops in 20 Fields - includes cover crops 2021 *\

minimize
Z:
 + 1000 ud_1 + 1000 ud_2 + 1000 ud_3 + 1000 ud_4 + 1000 ud_5 + 1000 ud_6 + 1000 ud_7 + 1000 ud_8 + 1000 ud_9 + 1000 ud_10 + 1000 ud_11 + 1000 ud_12 + 1000 ud_13 + 1000 ud_14 + 1000 ud_15 + 1000 ud_16

Subject to

 c_uf_1: + 1 uf_1  <= 1
 c_uf_2: + 1 uf_2  <= 1
 c_uf_3: + 1 uf_3  <= 1
 c_uf_4: + 1 uf_4  <= 1
 c_uf_5: + 1 uf_5  <= 1
 c_uf_6: + 1 uf_6 - 1 uf_221  <= 0
 c_uf_7: + 1 uf_7 - 1 uf_222  <= 0
 c_uf_8: + 1 uf_8 - 1 uf_223  <= 0
 c_uf_9: + 1 uf_9 - 1 uf_224  <= 0
 c_uf_10: + 1 uf_10 - 1 uf_225  <= 0
 c_uf_11: + 1 uf_11 - 1 uf_226  <= 0
 c_uf_12: + 1 uf_12 - 1 uf_227  <= 0
 c_uf_13: + 1 uf_13 - 1 uf_228  <= 0
 c_uf_14: + 1 uf_14 - 1 uf_229  <= 0
 c_uf_15: + 1 uf_15 - 1 uf_230  <= 0
 c_uf_16: + 1 uf_16 - 1 uf_231  <= 0
 c_uf_17: + 1 uf_17 - 1 uf_232  <= 0
 c_uf_18: + 1 uf_18 - 1 uf_233  <= 0
 c_uf_19: + 1 uf_19 - 1 uf_234  <= 0
 c_uf_20: + 1 uf_20 - 1 uf_235  <= 0
 c_uf_21: - 1 uf_1 + 1 uf_21  <= 0
 c_uf_22: - 1 uf_2 + 1 uf_22  <= 0
 c_uf_23: - 1 uf_3 + 1 uf_23  <= 0
 c_uf_24: - 1 uf_4 + 1 uf_24  <= 0
 c_uf_25: - 1 uf_5 + 1 uf_25  <= 0
 c_uf_26: - 1 uf_6 + 1 uf_26  <= 0
 c_uf_27: - 1 uf_7 + 1 uf_27  <= 0
 c_uf_28: - 1 uf_8 + 1 uf_28  <= 0
 c_uf_29: - 1 uf_9 + 1 uf_29  <= 0
 c_uf_30: - 1 uf_10 + 1 uf_30  <= 0
 c_uf_31: - 1 uf_11 + 1 uf_31  <= 0
 c_uf_32: - 1 uf_12 + 1 uf_32  <= 0
 c_uf_33: - 1 uf_13 + 1 uf_33  <= 0
 c_uf_34: - 1 uf_14 + 1 uf_34  <= 0
 c_uf_35: - 1 uf_15 + 1 uf_35  <= 0
 c_uf_36: - 1 uf_16 + 1 uf_36  <= 0
 c_uf_37: - 1 uf_17 + 1 uf_37  <= 0
 c_uf_38: - 1 uf_18 + 1 uf_38  <= 0
 c_uf_39: - 1 uf_19 + 1 uf_39  <= 0
 c_uf_40: - 1 uf_20 + 1 uf_40  <= 0
 c_uf_41: + 1 cpmy_1 + 1 cpmy_2 - 1 uf_21 + 1 uf_41  <= 0
 c_uf_42: + 1 cpmy_57 + 1 cpmy_58 - 1 uf_22 + 1 uf_42  <= 0
 c_uf_43: + 1 cpmy_113 + 1 cpmy_114 - 1 uf_23 + 1 uf_43  <= 0
 c_uf_44: + 1 cpmy_169 + 1 cpmy_170 - 1 uf_24 + 1 uf_44  <= 0
 c_uf_45: + 1 cpmy_225 + 1 cpmy_226 - 1 uf_25 + 1 uf_45  <= 0
 c_uf_46: + 1 cpmy_15 + 1 cpmy_16 - 1 uf_26 + 1 uf_46  <= 0
 c_uf_47: + 1 cpmy_71 + 1 cpmy_72 - 1 uf_27 + 1 uf_47  <= 0
 c_uf_48: + 1 cpmy_127 + 1 cpmy_128 - 1 uf_28 + 1 uf_48  <= 0
 c_uf_49: + 1 cpmy_183 + 1 cpmy_184 - 1 uf_29 + 1 uf_49  <= 0
 c_uf_50: + 1 cpmy_239 + 1 cpmy_240 - 1 uf_30 + 1 uf_50  <= 0
 c_uf_51: + 1 cpmy_29 + 1 cpmy_30 - 1 uf_31 + 1 uf_51  <= 0
 c_uf_52: + 1 cpmy_85 + 1 cpmy_86 - 1 uf_32 + 1 uf_52  <= 0
 c_uf_53: + 1 cpmy_141 + 1 cpmy_142 - 1 uf_33 + 1 uf_53  <= 0
 c_uf_54: + 1 cpmy_197 + 1 cpmy_198 - 1 uf_34 + 1 uf_54  <= 0
 c_uf_55: + 1 cpmy_253 + 1 cpmy_254 - 1 uf_35 + 1 uf_55  <= 0
 c_uf_56: + 1 cpmy_43 + 1 cpmy_44 - 1 uf_36 + 1 uf_56  <= 0
 c_uf_57: + 1 cpmy_99 + 1 cpmy_100 - 1 uf_37 + 1 uf_57  <= 0
 c_uf_58: + 1 cpmy_155 + 1 cpmy_156 - 1 uf_38 + 1 uf_58  <= 0
 c_uf_59: + 1 cpmy_211 + 1 cpmy_212 - 1 uf_39 + 1 uf_59  <= 0
 c_uf_60: + 1 cpmy_267 + 1 cpmy_268 - 1 uf_40 + 1 uf_60  <= 0
 c_uf_61: + 1 cpmy_3 + 1 cpmy_4 + 1 cpmy_5 + 1 cpmy_6 - 1 uf_41 + 1 uf_61  <= 0
 c_uf_62: + 1 cpmy_59 + 1 cpmy_60 + 1 cpmy_61 + 1 cpmy_62 - 1 uf_42 + 1 uf_62  <= 0
 c_uf_63: + 1 cpmy_115 + 1 cpmy_116 + 1 cpmy_117 + 1 cpmy_118 - 1 uf_43 + 1 uf_63  <= 0
 c_uf_64: + 1 cpmy_171 + 1 cpmy_172 + 1 cpmy_173 + 1 cpmy_174 - 1 uf_44 + 1 uf_64  <= 0
 c_uf_65: + 1 cpmy_227 + 1 cpmy_228 + 1 cpmy_229 + 1 cpmy_230 - 1 uf_45 + 1 uf_65  <= 0
 c_uf_66: + 1 cpmy_17 + 1 cpmy_18 + 1 cpmy_19 + 1 cpmy_20 - 1 uf_46 + 1 uf_66  <= 0
 c_uf_67: + 1 cpmy_73 + 1 cpmy_74 + 1 cpmy_75 + 1 cpmy_76 - 1 uf_47 + 1 uf_67  <= 0
 c_uf_68: + 1 cpmy_129 + 1 cpmy_130 + 1 cpmy_131 + 1 cpmy_132 - 1 uf_48 + 1 uf_68  <= 0
 c_uf_69: + 1 cpmy_185 + 1 cpmy_186 + 1 cpmy_187 + 1 cpmy_188 - 1 uf_49 + 1 uf_69  <= 0
 c_uf_70: + 1 cpmy_241 + 1 cpmy_242 + 1 cpmy_243 + 1 cpmy_244 - 1 uf_50 + 1 uf_70  <= 0
 c_uf_71: + 1 cpmy_31 + 1 cpmy_32 + 1 cpmy_33 + 1 cpmy_34 - 1 uf_51 + 1 uf_71  <= 0
 c_uf_72: + 1 cpmy_87 + 1 cpmy_88 + 1 cpmy_89 + 1 cpmy_90 - 1 uf_52 + 1 uf_72  <= 0
 c_uf_73: + 1 cpmy_143 + 1 cpmy_144 + 1 cpmy_145 + 1 cpmy_146 - 1 uf_53 + 1 uf_73  <= 0
 c_uf_74: + 1 cpmy_199 + 1 cpmy_200 + 1 cpmy_201 + 1 cpmy_202 - 1 uf_54 + 1 uf_74  <= 0
 c_uf_75: + 1 cpmy_255 + 1 cpmy_256 + 1 cpmy_257 + 1 cpmy_258 - 1 uf_55 + 1 uf_75  <= 0
 c_uf_76: + 1 cpmy_45 + 1 cpmy_46 + 1 cpmy_47 + 1 cpmy_48 - 1 uf_56 + 1 uf_76  <= 0
 c_uf_77: + 1 cpmy_101 + 1 cpmy_102 + 1 cpmy_103 + 1 cpmy_104 - 1 uf_57 + 1 uf_77  <= 0
 c_uf_78: + 1 cpmy_157 + 1 cpmy_158 + 1 cpmy_159 + 1 cpmy_160 - 1 uf_58 + 1 uf_78  <= 0
 c_uf_79: + 1 cpmy_213 + 1 cpmy_214 + 1 cpmy_215 + 1 cpmy_216 - 1 uf_59 + 1 uf_79  <= 0
 c_uf_80: + 1 cpmy_269 + 1 cpmy_270 + 1 cpmy_271 + 1 cpmy_272 - 1 uf_60 + 1 uf_80  <= 0
 c_uf_81: - 1 cpmy_1 + 1 cpmy_7 + 1 cpmy_8 + 1 cpmy_9 + 1 cpmy_10 - 1 uf_61 + 1 uf_81  <= 0
 c_uf_82: - 1 cpmy_57 + 1 cpmy_63 + 1 cpmy_64 + 1 cpmy_65 + 1 cpmy_66 - 1 uf_62 + 1 uf_82  <= 0
 c_uf_83: - 1 cpmy_113 + 1 cpmy_119 + 1 cpmy_120 + 1 cpmy_121 + 1 cpmy_122 - 1 uf_63 + 1 uf_83  <= 0
 c_uf_84: - 1 cpmy_169 + 1 cpmy_175 + 1 cpmy_176 + 1 cpmy_177 + 1 cpmy_178 - 1 uf_64 + 1 uf_84  <= 0
 c_uf_85: - 1 cpmy_225 + 1 cpmy_231 + 1 cpmy_232 + 1 cpmy_233 + 1 cpmy_234 - 1 uf_65 + 1 uf_85  <= 0
 c_uf_86: - 1 cpmy_15 + 1 cpmy_21 + 1 cpmy_22 + 1 cpmy_23 + 1 cpmy_24 - 1 uf_66 + 1 uf_86  <= 0
 c_uf_87: - 1 cpmy_71 + 1 cpmy_77 + 1 cpmy_78 + 1 cpmy_79 + 1 cpmy_80 - 1 uf_67 + 1 uf_87  <= 0
 c_uf_88: - 1 cpmy_127 + 1 cpmy_133 + 1 cpmy_134 + 1 cpmy_135 + 1 cpmy_136 - 1 uf_68 + 1 uf_88  <= 0
 c_uf_89: - 1 cpmy_183 + 1 cpmy_189 + 1 cpmy_190 + 1 cpmy_191 + 1 cpmy_192 - 1 uf_69 + 1 uf_89  <= 0
 c_uf_90: - 1 cpmy_239 + 1 cpmy_245 + 1 cpmy_246 + 1 cpmy_247 + 1 cpmy_248 - 1 uf_70 + 1 uf_90  <= 0
 c_uf_91: - 1 cpmy_29 + 1 cpmy_35 + 1 cpmy_36 + 1 cpmy_37 + 1 cpmy_38 - 1 uf_71 + 1 uf_91  <= 0
 c_uf_92: - 1 cpmy_85 + 1 cpmy_91 + 1 cpmy_92 + 1 cpmy_93 + 1 cpmy_94 - 1 uf_72 + 1 uf_92  <= 0
 c_uf_93: - 1 cpmy_141 + 1 cpmy_147 + 1 cpmy_148 + 1 cpmy_149 + 1 cpmy_150 - 1 uf_73 + 1 uf_93  <= 0
 c_uf_94: - 1 cpmy_197 + 1 cpmy_203 + 1 cpmy_204 + 1 cpmy_205 + 1 cpmy_206 - 1 uf_74 + 1 uf_94  <= 0
 c_uf_95: - 1 cpmy_253 + 1 cpmy_259 + 1 cpmy_260 + 1 cpmy_261 + 1 cpmy_262 - 1 uf_75 + 1 uf_95  <= 0
 c_uf_96: - 1 cpmy_43 + 1 cpmy_49 + 1 cpmy_50 + 1 cpmy_51 + 1 cpmy_52 - 1 uf_76 + 1 uf_96  <= 0
 c_uf_97: - 1 cpmy_99 + 1 cpmy_105 + 1 cpmy_106 + 1 cpmy_107 + 1 cpmy_108 - 1 uf_77 + 1 uf_97  <= 0
 c_uf_98: - 1 cpmy_155 + 1 cpmy_161 + 1 cpmy_162 + 1 cpmy_163 + 1 cpmy_164 - 1 uf_78 + 1 uf_98  <= 0
 c_uf_99: - 1 cpmy_211 + 1 cpmy_217 + 1 cpmy_218 + 1 cpmy_219 + 1 cpmy_220 - 1 uf_79 + 1 uf_99  <= 0
 c_uf_100: - 1 cpmy_267 + 1 cpmy_273 + 1 cpmy_274 + 1 cpmy_275 + 1 cpmy_276 - 1 uf_80 + 1 uf_100  <= 0
 c_uf_101: - 1 cpmy_2 - 1 cpmy_3 + 1 cpmy_11 + 1 cpmy_12 - 1 uf_81 + 1 uf_101  <= 0
 c_uf_102: - 1 cpmy_58 - 1 cpmy_59 + 1 cpmy_67 + 1 cpmy_68 - 1 uf_82 + 1 uf_102  <= 0
 c_uf_103: - 1 cpmy_114 - 1 cpmy_115 + 1 cpmy_123 + 1 cpmy_124 - 1 uf_83 + 1 uf_103  <= 0
 c_uf_104: - 1 cpmy_170 - 1 cpmy_171 + 1 cpmy_179 + 1 cpmy_180 - 1 uf_84 + 1 uf_104  <= 0
 c_uf_105: - 1 cpmy_226 - 1 cpmy_227 + 1 cpmy_235 + 1 cpmy_236 - 1 uf_85 + 1 uf_105  <= 0
 c_uf_106: - 1 cpmy_16 - 1 cpmy_17 + 1 cpmy_25 + 1 cpmy_26 - 1 uf_86 + 1 uf_106  <= 0
 c_uf_107: - 1 cpmy_72 - 1 cpmy_73 + 1 cpmy_81 + 1 cpmy_82 - 1 uf_87 + 1 uf_107  <= 0
 c_uf_108: - 1 cpmy_128 - 1 cpmy_129 + 1 cpmy_137 + 1 cpmy_138 - 1 uf_88 + 1 uf_108  <= 0
 c_uf_109: - 1 cpmy_184 - 1 cpmy_185 + 1 cpmy_193 + 1 cpmy_194 - 1 uf_89 + 1 uf_109  <= 0
 c_uf_110: - 1 cpmy_240 - 1 cpmy_241 + 1 cpmy_249 + 1 cpmy_250 - 1 uf_90 + 1 uf_110  <= 0
 c_uf_111: - 1 cpmy_30 - 1 cpmy_31 + 1 cpmy_39 + 1 cpmy_40 - 1 uf_91 + 1 uf_111  <= 0
 c_uf_112: - 1 cpmy_86 - 1 cpmy_87 + 1 cpmy_95 + 1 cpmy_96 - 1 uf_92 + 1 uf_112  <= 0
 c_uf_113: - 1 cpmy_142 - 1 cpmy_143 + 1 cpmy_151 + 1 cpmy_152 - 1 uf_93 + 1 uf_113  <= 0
 c_uf_114: - 1 cpmy_198 - 1 cpmy_199 + 1 cpmy_207 + 1 cpmy_208 - 1 uf_94 + 1 uf_114  <= 0
 c_uf_115: - 1 cpmy_254 - 1 cpmy_255 + 1 cpmy_263 + 1 cpmy_264 - 1 uf_95 + 1 uf_115  <= 0
 c_uf_116: - 1 cpmy_44 - 1 cpmy_45 + 1 cpmy_53 + 1 cpmy_54 - 1 uf_96 + 1 uf_116  <= 0
 c_uf_117: - 1 cpmy_100 - 1 cpmy_101 + 1 cpmy_109 + 1 cpmy_110 - 1 uf_97 + 1 uf_117  <= 0
 c_uf_118: - 1 cpmy_156 - 1 cpmy_157 + 1 cpmy_165 + 1 cpmy_166 - 1 uf_98 + 1 uf_118  <= 0
 c_uf_119: - 1 cpmy_212 - 1 cpmy_213 + 1 cpmy_221 + 1 cpmy_222 - 1 uf_99 + 1 uf_119  <= 0
 c_uf_120: - 1 cpmy_268 - 1 cpmy_269 + 1 cpmy_277 + 1 cpmy_278 - 1 uf_100 + 1 uf_120  <= 0
 c_uf_121: - 1 cpmy_4 - 1 cpmy_7 + 1 cpmy_13 + 1 cpmy_14 - 1 uf_101 + 1 uf_121  <= 0
 c_uf_122: - 1 cpmy_60 - 1 cpmy_63 + 1 cpmy_69 + 1 cpmy_70 - 1 uf_102 + 1 uf_122  <= 0
 c_uf_123: - 1 cpmy_116 - 1 cpmy_119 + 1 cpmy_125 + 1 cpmy_126 - 1 uf_103 + 1 uf_123  <= 0
 c_uf_124: - 1 cpmy_172 - 1 cpmy_175 + 1 cpmy_181 + 1 cpmy_182 - 1 uf_104 + 1 uf_124  <= 0
 c_uf_125: - 1 cpmy_228 - 1 cpmy_231 + 1 cpmy_237 + 1 cpmy_238 - 1 uf_105 + 1 uf_125  <= 0
 c_uf_126: - 1 cpmy_18 - 1 cpmy_21 + 1 cpmy_27 + 1 cpmy_28 - 1 uf_106 + 1 uf_126  <= 0
 c_uf_127: - 1 cpmy_74 - 1 cpmy_77 + 1 cpmy_83 + 1 cpmy_84 - 1 uf_107 + 1 uf_127  <= 0
 c_uf_128: - 1 cpmy_130 - 1 cpmy_133 + 1 cpmy_139 + 1 cpmy_140 - 1 uf_108 + 1 uf_128  <= 0
 c_uf_129: - 1 cpmy_186 - 1 cpmy_189 + 1 cpmy_195 + 1 cpmy_196 - 1 uf_109 + 1 uf_129  <= 0
 c_uf_130: - 1 cpmy_242 - 1 cpmy_245 + 1 cpmy_251 + 1 cpmy_252 - 1 uf_110 + 1 uf_130  <= 0
 c_uf_131: - 1 cpmy_32 - 1 cpmy_35 + 1 cpmy_41 + 1 cpmy_42 - 1 uf_111 + 1 uf_131  <= 0
 c_uf_132: - 1 cpmy_88 - 1 cpmy_91 + 1 cpmy_97 + 1 cpmy_98 - 1 uf_112 + 1 uf_132  <= 0
 c_uf_133: - 1 cpmy_144 - 1 cpmy_147 + 1 cpmy_153 + 1 cpmy_154 - 1 uf_113 + 1 uf_133  <= 0
 c_uf_134: - 1 cpmy_200 - 1 cpmy_203 + 1 cpmy_209 + 1 cpmy_210 - 1 uf_114 + 1 uf_134  <= 0
 c_uf_135: - 1 cpmy_256 - 1 cpmy_259 + 1 cpmy_265 + 1 cpmy_266 - 1 uf_115 + 1 uf_135  <= 0
 c_uf_136: - 1 cpmy_46 - 1 cpmy_49 + 1 cpmy_55 + 1 cpmy_56 - 1 uf_116 + 1 uf_136  <= 0
 c_uf_137: - 1 cpmy_102 - 1 cpmy_105 + 1 cpmy_111 + 1 cpmy_112 - 1 uf_117 + 1 uf_137  <= 0
 c_uf_138: - 1 cpmy_158 - 1 cpmy_161 + 1 cpmy_167 + 1 cpmy_168 - 1 uf_118 + 1 uf_138  <= 0
 c_uf_139: - 1 cpmy_214 - 1 cpmy_217 + 1 cpmy_223 + 1 cpmy_224 - 1 uf_119 + 1 uf_139  <= 0
 c_uf_140: - 1 cpmy_270 - 1 cpmy_273 + 1 cpmy_279 + 1 cpmy_280 - 1 uf_120 + 1 uf_140  <= 0
 c_uf_141: - 1 cpmy_5 - 1 cpmy_6 - 1 cpmy_8 - 1 uf_121 + 1 uf_141  <= 0
 c_uf_142: - 1 cpmy_61 - 1 cpmy_62 - 1 cpmy_64 - 1 uf_122 + 1 uf_142  <= 0
 c_uf_143: - 1 cpmy_117 - 1 cpmy_118 - 1 cpmy_120 - 1 uf_123 + 1 uf_143  <= 0
 c_uf_144: - 1 cpmy_173 - 1 cpmy_174 - 1 cpmy_176 - 1 uf_124 + 1 uf_144  <= 0
 c_uf_145: - 1 cpmy_229 - 1 cpmy_230 - 1 cpmy_232 - 1 uf_125 + 1 uf_145  <= 0
 c_uf_146: - 1 cpmy_19 - 1 cpmy_20 - 1 cpmy_22 - 1 uf_126 + 1 uf_146  <= 0
 c_uf_147: - 1 cpmy_75 - 1 cpmy_76 - 1 cpmy_78 - 1 uf_127 + 1 uf_147  <= 0
 c_uf_148: - 1 cpmy_131 - 1 cpmy_132 - 1 cpmy_134 - 1 uf_128 + 1 uf_148  <= 0
 c_uf_149: - 1 cpmy_187 - 1 cpmy_188 - 1 cpmy_190 - 1 uf_129 + 1 uf_149  <= 0
 c_uf_150: - 1 cpmy_243 - 1 cpmy_244 - 1 cpmy_246 - 1 uf_130 + 1 uf_150  <= 0
 c_uf_151: - 1 cpmy_33 - 1 cpmy_34 - 1 cpmy_36 - 1 uf_131 + 1 uf_151  <= 0
 c_uf_152: - 1 cpmy_89 - 1 cpmy_90 - 1 cpmy_92 - 1 uf_132 + 1 uf_152  <= 0
 c_uf_153: - 1 cpmy_145 - 1 cpmy_146 - 1 cpmy_148 - 1 uf_133 + 1 uf_153  <= 0
 c_uf_154: - 1 cpmy_201 - 1 cpmy_202 - 1 cpmy_204 - 1 uf_134 + 1 uf_154  <= 0
 c_uf_155: - 1 cpmy_257 - 1 cpmy_258 - 1 cpmy_260 - 1 uf_135 + 1 uf_155  <= 0
 c_uf_156: - 1 cpmy_47 - 1 cpmy_48 - 1 cpmy_50 - 1 uf_136 + 1 uf_156  <= 0
 c_uf_157: - 1 cpmy_103 - 1 cpmy_104 - 1 cpmy_106 - 1 uf_137 + 1 uf_157  <= 0
 c_uf_158: - 1 cpmy_159 - 1 cpmy_160 - 1 cpmy_162 - 1 uf_138 + 1 uf_158  <= 0
 c_uf_159: - 1 cpmy_215 - 1 cpmy_216 - 1 cpmy_218 - 1 uf_139 + 1 uf_159  <= 0
 c_uf_160: - 1 cpmy_271 - 1 cpmy_272 - 1 cpmy_274 - 1 uf_140 + 1 uf_160  <= 0
 c_uf_161: - 1 cpmy_9 - 1 cpmy_10 - 1 uf_141 + 1 uf_161  <= 0
 c_uf_162: - 1 cpmy_65 - 1 cpmy_66 - 1 uf_142 + 1 uf_162  <= 0
 c_uf_163: - 1 cpmy_121 - 1 cpmy_122 - 1 uf_143 + 1 uf_163  <= 0
 c_uf_164: - 1 cpmy_177 - 1 cpmy_178 - 1 uf_144 + 1 uf_164  <= 0
 c_uf_165: - 1 cpmy_233 - 1 cpmy_234 - 1 uf_145 + 1 uf_165  <= 0
 c_uf_166: - 1 cpmy_23 - 1 cpmy_24 - 1 uf_146 + 1 uf_166  <= 0
 c_uf_167: - 1 cpmy_79 - 1 cpmy_80 - 1 uf_147 + 1 uf_167  <= 0
 c_uf_168: - 1 cpmy_135 - 1 cpmy_136 - 1 uf_148 + 1 uf_168  <= 0
 c_uf_169: - 1 cpmy_191 - 1 cpmy_192 - 1 uf_149 + 1 uf_169  <= 0
 c_uf_170: - 1 cpmy_247 - 1 cpmy_248 - 1 uf_150 + 1 uf_170  <= 0
 c_uf_171: - 1 cpmy_37 - 1 cpmy_38 - 1 uf_151 + 1 uf_171  <= 0
 c_uf_172: - 1 cpmy_93 - 1 cpmy_94 - 1 uf_152 + 1 uf_172  <= 0
 c_uf_173: - 1 cpmy_149 - 1 cpmy_150 - 1 uf_153 + 1 uf_173  <= 0
 c_uf_174: - 1 cpmy_205 - 1 cpmy_206 - 1 uf_154 + 1 uf_174  <= 0
 c_uf_175: - 1 cpmy_261 - 1 cpmy_262 - 1 uf_155 + 1 uf_175  <= 0
 c_uf_176: - 1 cpmy_51 - 1 cpmy_52 - 1 uf_156 + 1 uf_176  <= 0
 c_uf_177: - 1 cpmy_107 - 1 cpmy_108 - 1 uf_157 + 1 uf_177  <= 0
 c_uf_178: - 1 cpmy_163 - 1 cpmy_164 - 1 uf_158 + 1 uf_178  <= 0
 c_uf_179: - 1 cpmy_219 - 1 cpmy_220 - 1 uf_159 + 1 uf_179  <= 0
 c_uf_180: - 1 cpmy_275 - 1 cpmy_276 - 1 uf_160 + 1 uf_180  <= 0
 c_uf_181: - 1 cpmy_11 - 1 cpmy_12 - 1 uf_161 + 1 uf_181  <= 0
 c_uf_182: - 1 cpmy_67 - 1 cpmy_68 - 1 uf_162 + 1 uf_182  <= 0
 c_uf_183: - 1 cpmy_123 - 1 cpmy_124 - 1 uf_163 + 1 uf_183  <= 0
 c_uf_184: - 1 cpmy_179 - 1 cpmy_180 - 1 uf_164 + 1 uf_184  <= 0
 c_uf_185: - 1 cpmy_235 - 1 cpmy_236 - 1 uf_165 + 1 uf_185  <= 0
 c_uf_186: - 1 cpmy_25 - 1 cpmy_26 - 1 uf_166 + 1 uf_186  <= 0
 c_uf_187: - 1 cpmy_81 - 1 cpmy_82 - 1 uf_167 + 1 uf_187  <= 0
 c_uf_188: - 1 cpmy_137 - 1 cpmy_138 - 1 uf_168 + 1 uf_188  <= 0
 c_uf_189: - 1 cpmy_193 - 1 cpmy_194 - 1 uf_169 + 1 uf_189  <= 0
 c_uf_190: - 1 cpmy_249 - 1 cpmy_250 - 1 uf_170 + 1 uf_190  <= 0
 c_uf_191: - 1 cpmy_39 - 1 cpmy_40 - 1 uf_171 + 1 uf_191  <= 0
 c_uf_192: - 1 cpmy_95 - 1 cpmy_96 - 1 uf_172 + 1 uf_192  <= 0
 c_uf_193: - 1 cpmy_151 - 1 cpmy_152 - 1 uf_173 + 1 uf_193  <= 0
 c_uf_194: - 1 cpmy_207 - 1 cpmy_208 - 1 uf_174 + 1 uf_194  <= 0
 c_uf_195: - 1 cpmy_263 - 1 cpmy_264 - 1 uf_175 + 1 uf_195  <= 0
 c_uf_196: - 1 cpmy_53 - 1 cpmy_54 - 1 uf_176 + 1 uf_196  <= 0
 c_uf_197: - 1 cpmy_109 - 1 cpmy_110 - 1 uf_177 + 1 uf_197  <= 0
 c_uf_198: - 1 cpmy_165 - 1 cpmy_166 - 1 uf_178 + 1 uf_198  <= 0
 c_uf_199: - 1 cpmy_221 - 1 cpmy_222 - 1 uf_179 + 1 uf_199  <= 0
 c_uf_200: - 1 cpmy_277 - 1 cpmy_278 - 1 uf_180 + 1 uf_200  <= 0
 c_uf_201: - 1 cpmy_13 - 1 cpmy_14 - 1 uf_181 + 1 uf_201  <= 0
 c_uf_202: - 1 cpmy_69 - 1 cpmy_70 - 1 uf_182 + 1 uf_202  <= 0
 c_uf_203: - 1 cpmy_125 - 1 cpmy_126 - 1 uf_183 + 1 uf_203  <= 0
 c_uf_204: - 1 cpmy_181 - 1 cpmy_182 - 1 uf_184 + 1 uf_204  <= 0
 c_uf_205: - 1 cpmy_237 - 1 cpmy_238 - 1 uf_185 + 1 uf_205  <= 0
 c_uf_206: - 1 cpmy_27 - 1 cpmy_28 - 1 uf_186 + 1 uf_206  <= 0
 c_uf_207: - 1 cpmy_83 - 1 cpmy_84 - 1 uf_187 + 1 uf_207  <= 0
 c_uf_208: - 1 cpmy_139 - 1 cpmy_140 - 1 uf_188 + 1 uf_208  <= 0
 c_uf_209: - 1 cpmy_195 - 1 cpmy_196 - 1 uf_189 + 1 uf_209  <= 0
 c_uf_210: - 1 cpmy_251 - 1 cpmy_252 - 1 uf_190 + 1 uf_210  <= 0
 c_uf_211: - 1 cpmy_41 - 1 cpmy_42 - 1 uf_191 + 1 uf_211  <= 0
 c_uf_212: - 1 cpmy_97 - 1 cpmy_98 - 1 uf_192 + 1 uf_212  <= 0
 c_uf_213: - 1 cpmy_153 - 1 cpmy_154 - 1 uf_193 + 1 uf_213  <= 0
 c_uf_214: - 1 cpmy_209 - 1 cpmy_210 - 1 uf_194 + 1 uf_214  <= 0
 c_uf_215: - 1 cpmy_265 - 1 cpmy_266 - 1 uf_195 + 1 uf_215  <= 0
 c_uf_216: - 1 cpmy_55 - 1 cpmy_56 - 1 uf_196 + 1 uf_216  <= 0
 c_uf_217: - 1 cpmy_111 - 1 cpmy_112 - 1 uf_197 + 1 uf_217  <= 0
 c_uf_218: - 1 cpmy_167 - 1 cpmy_168 - 1 uf_198 + 1 uf_218  <= 0
 c_uf_219: - 1 cpmy_223 - 1 cpmy_224 - 1 uf_199 + 1 uf_219  <= 0
 c_uf_220: - 1 cpmy_279 - 1 cpmy_280 - 1 uf_200 + 1 uf_220  <= 0
 c_uf_221: - 1 uf_201 + 1 uf_221  <= 0
 c_uf_222: - 1 uf_202 + 1 uf_222  <= 0
 c_uf_223: - 1 uf_203 + 1 uf_223  <= 0
 c_uf_224: - 1 uf_204 + 1 uf_224  <= 0
 c_uf_225: - 1 uf_205 + 1 uf_225  <= 0
 c_uf_226: - 1 uf_206 + 1 uf_226  <= 0
 c_uf_227: - 1 uf_207 + 1 uf_227  <= 0
 c_uf_228: - 1 uf_208 + 1 uf_228  <= 0
 c_uf_229: - 1 uf_209 + 1 uf_229  <= 0
 c_uf_230: - 1 uf_210 + 1 uf_230  <= 0
 c_uf_231: - 1 uf_211 + 1 uf_231  <= 0
 c_uf_232: - 1 uf_212 + 1 uf_232  <= 0
 c_uf_233: - 1 uf_213 + 1 uf_233  <= 0
 c_uf_234: - 1 uf_214 + 1 uf_234  <= 0
 c_uf_235: - 1 uf_215 + 1 uf_235  <= 0
 c_uf_236: - 1 uf_216 + 1 uf_236  <= 0
 c_uf_237: - 1 uf_217 + 1 uf_237  <= 0
 c_uf_238: - 1 uf_218 + 1 uf_238  <= 0
 c_uf_239: - 1 uf_219 + 1 uf_239  <= 0
 c_uf_240: - 1 uf_220 + 1 uf_240  <= 0

 c_ud_1: + 50 cpmy_1 + 50 cpmy_3 + 50 cpmy_7 + 50 cpmy_57 + 50 cpmy_59 + 50 cpmy_63 + 50 cpmy_113 + 50 cpmy_115 + 50 cpmy_119 + 50 cpmy_169 + 50 cpmy_171 + 50 cpmy_175 + 50 cpmy_225 + 50 cpmy_227 + 50 cpmy_231 + 1 ud_1  >= 250
 c_ud_2: + 50 cpmy_2 + 50 cpmy_4 + 50 cpmy_8 + 50 cpmy_58 + 50 cpmy_60 + 50 cpmy_64 + 50 cpmy_114 + 50 cpmy_116 + 50 cpmy_120 + 50 cpmy_170 + 50 cpmy_172 + 50 cpmy_176 + 50 cpmy_226 + 50 cpmy_228 + 50 cpmy_232 + 1 ud_2  >= 250
 c_ud_3: + 62.5 cpmy_6 + 62.5 cpmy_10 + 62.5 cpmy_12 + 62.5 cpmy_14 + 62.5 cpmy_62 + 62.5 cpmy_66 + 62.5 cpmy_68 + 62.5 cpmy_70 + 62.5 cpmy_118 + 62.5 cpmy_122 + 62.5 cpmy_124 + 62.5 cpmy_126 + 62.5 cpmy_174 + 62.5 cpmy_178 + 62.5 cpmy_180 + 62.5 cpmy_182 + 62.5 cpmy_230 + 62.5 cpmy_234 + 62.5 cpmy_236 + 62.5 cpmy_238 + 1 ud_3  >= 75
 c_ud_4: + 62.5 cpmy_5 + 62.5 cpmy_9 + 62.5 cpmy_11 + 62.5 cpmy_13 + 62.5 cpmy_61 + 62.5 cpmy_65 + 62.5 cpmy_67 + 62.5 cpmy_69 + 62.5 cpmy_117 + 62.5 cpmy_121 + 62.5 cpmy_123 + 62.5 cpmy_125 + 62.5 cpmy_173 + 62.5 cpmy_177 + 62.5 cpmy_179 + 62.5 cpmy_181 + 62.5 cpmy_229 + 62.5 cpmy_233 + 62.5 cpmy_235 + 62.5 cpmy_237 + 1 ud_4  >= 20
 c_ud_5: + 50 cpmy_15 + 50 cpmy_17 + 50 cpmy_21 + 50 cpmy_71 + 50 cpmy_73 + 50 cpmy_77 + 50 cpmy_127 + 50 cpmy_129 + 50 cpmy_133 + 50 cpmy_183 + 50 cpmy_185 + 50 cpmy_189 + 50 cpmy_239 + 50 cpmy_241 + 50 cpmy_245 + 1 ud_5  >= 250
 c_ud_6: + 50 cpmy_16 + 50 cpmy_18 + 50 cpmy_22 + 50 cpmy_72 + 50 cpmy_74 + 50 cpmy_78 + 50 cpmy_128 + 50 cpmy_130 + 50 cpmy_134 + 50 cpmy_184 + 50 cpmy_186 + 50 cpmy_190 + 50 cpmy_240 + 50 cpmy_242 + 50 cpmy_246 + 1 ud_6  >= 250
 c_ud_7: + 62.5 cpmy_20 + 62.5 cpmy_24 + 62.5 cpmy_26 + 62.5 cpmy_28 + 62.5 cpmy_76 + 62.5 cpmy_80 + 62.5 cpmy_82 + 62.5 cpmy_84 + 62.5 cpmy_132 + 62.5 cpmy_136 + 62.5 cpmy_138 + 62.5 cpmy_140 + 62.5 cpmy_188 + 62.5 cpmy_192 + 62.5 cpmy_194 + 62.5 cpmy_196 + 62.5 cpmy_244 + 62.5 cpmy_248 + 62.5 cpmy_250 + 62.5 cpmy_252 + 1 ud_7  >= 75
 c_ud_8: + 62.5 cpmy_19 + 62.5 cpmy_23 + 62.5 cpmy_25 + 62.5 cpmy_27 + 62.5 cpmy_75 + 62.5 cpmy_79 + 62.5 cpmy_81 + 62.5 cpmy_83 + 62.5 cpmy_131 + 62.5 cpmy_135 + 62.5 cpmy_137 + 62.5 cpmy_139 + 62.5 cpmy_187 + 62.5 cpmy_191 + 62.5 cpmy_193 + 62.5 cpmy_195 + 62.5 cpmy_243 + 62.5 cpmy_247 + 62.5 cpmy_249 + 62.5 cpmy_251 + 1 ud_8  >= 20
 c_ud_9: + 50 cpmy_29 + 50 cpmy_31 + 50 cpmy_35 + 50 cpmy_85 + 50 cpmy_87 + 50 cpmy_91 + 50 cpmy_141 + 50 cpmy_143 + 50 cpmy_147 + 50 cpmy_197 + 50 cpmy_199 + 50 cpmy_203 + 50 cpmy_253 + 50 cpmy_255 + 50 cpmy_259 + 1 ud_9  >= 250
 c_ud_10: + 50 cpmy_30 + 50 cpmy_32 + 50 cpmy_36 + 50 cpmy_86 + 50 cpmy_88 + 50 cpmy_92 + 50 cpmy_142 + 50 cpmy_144 + 50 cpmy_148 + 50 cpmy_198 + 50 cpmy_200 + 50 cpmy_204 + 50 cpmy_254 + 50 cpmy_256 + 50 cpmy_260 + 1 ud_10  >= 250
 c_ud_11: + 62.5 cpmy_34 + 62.5 cpmy_38 + 62.5 cpmy_40 + 62.5 cpmy_42 + 62.5 cpmy_90 + 62.5 cpmy_94 + 62.5 cpmy_96 + 62.5 cpmy_98 + 62.5 cpmy_146 + 62.5 cpmy_150 + 62.5 cpmy_152 + 62.5 cpmy_154 + 62.5 cpmy_202 + 62.5 cpmy_206 + 62.5 cpmy_208 + 62.5 cpmy_210 + 62.5 cpmy_258 + 62.5 cpmy_262 + 62.5 cpmy_264 + 62.5 cpmy_266 + 1 ud_11  >= 75
 c_ud_12: + 62.5 cpmy_33 + 62.5 cpmy_37 + 62.5 cpmy_39 + 62.5 cpmy_41 + 62.5 cpmy_89 + 62.5 cpmy_93 + 62.5 cpmy_95 + 62.5 cpmy_97 + 62.5 cpmy_145 + 62.5 cpmy_149 + 62.5 cpmy_151 + 62.5 cpmy_153 + 62.5 cpmy_201 + 62.5 cpmy_205 + 62.5 cpmy_207 + 62.5 cpmy_209 + 62.5 cpmy_257 + 62.5 cpmy_261 + 62.5 cpmy_263 + 62.5 cpmy_265 + 1 ud_12  >= 20
 c_ud_13: + 50 cpmy_43 + 50 cpmy_45 + 50 cpmy_49 + 50 cpmy_99 + 50 cpmy_101 + 50 cpmy_105 + 50 cpmy_155 + 50 cpmy_157 + 50 cpmy_161 + 50 cpmy_211 + 50 cpmy_213 + 50 cpmy_217 + 50 cpmy_267 + 50 cpmy_269 + 50 cpmy_273 + 1 ud_13  >= 250
 c_ud_14: + 50 cpmy_44 + 50 cpmy_46 + 50 cpmy_50 + 50 cpmy_100 + 50 cpmy_102 + 50 cpmy_106 + 50 cpmy_156 + 50 cpmy_158 + 50 cpmy_162 + 50 cpmy_212 + 50 cpmy_214 + 50 cpmy_218 + 50 cpmy_268 + 50 cpmy_270 + 50 cpmy_274 + 1 ud_14  >= 250
 c_ud_15: + 62.5 cpmy_48 + 62.5 cpmy_52 + 62.5 cpmy_54 + 62.5 cpmy_56 + 62.5 cpmy_104 + 62.5 cpmy_108 + 62.5 cpmy_110 + 62.5 cpmy_112 + 62.5 cpmy_160 + 62.5 cpmy_164 + 62.5 cpmy_166 + 62.5 cpmy_168 + 62.5 cpmy_216 + 62.5 cpmy_220 + 62.5 cpmy_222 + 62.5 cpmy_224 + 62.5 cpmy_272 + 62.5 cpmy_276 + 62.5 cpmy_278 + 62.5 cpmy_280 + 1 ud_15  >= 75
 c_ud_16: + 62.5 cpmy_47 + 62.5 cpmy_51 + 62.5 cpmy_53 + 62.5 cpmy_55 + 62.5 cpmy_103 + 62.5 cpmy_107 + 62.5 cpmy_109 + 62.5 cpmy_111 + 62.5 cpmy_159 + 62.5 cpmy_163 + 62.5 cpmy_165 + 62.5 cpmy_167 + 62.5 cpmy_215 + 62.5 cpmy_219 + 62.5 cpmy_221 + 62.5 cpmy_223 + 62.5 cpmy_271 + 62.5 cpmy_275 + 62.5 cpmy_277 + 62.5 cpmy_279 + 1 ud_16  >= 20

 c_cpmy_1_1_Mar_1:  +1 cpmy_1  -1 dr_1  <= 0
 c_cpmy_1_1_Mar_2:  +1 cpmy_2  +1 dr_1  <= 1
 c_cpmy_1_1_Mar_3:  +1 cpmy_3  +1 cpmy_4  +1 cpmy_5  +1 cpmy_6  +1 dr_1  <= 1
 c_cpmy_1_1_Mar_4:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_1  <= 1
 c_cpmy_1_1_Mar_5:  +1 cpmy_11  +1 cpmy_12  +1 dr_1  <= 1
 c_cpmy_1_1_Mar_6:  +1 cpmy_13  +1 cpmy_14  +1 dr_1  <= 1
 c_cpmy_1_1_Mar_14:  +1 cpmy_15  +1 cpmy_16  +1 dr_1  <= 1
 c_cpmy_2_1_Mar_1:  +1 cpmy_2  -1 dr_2  <= 0
 c_cpmy_2_1_Mar_2:  +1 cpmy_1  +1 dr_2  <= 1
 c_cpmy_2_1_Mar_3:  +1 cpmy_3  +1 cpmy_4  +1 cpmy_5  +1 cpmy_6  +1 dr_2  <= 1
 c_cpmy_2_1_Mar_4:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_2  <= 1
 c_cpmy_2_1_Mar_5:  +1 cpmy_11  +1 cpmy_12  +1 dr_2  <= 1
 c_cpmy_2_1_Mar_6:  +1 cpmy_13  +1 cpmy_14  +1 dr_2  <= 1
 c_cpmy_2_1_Mar_14:  +1 cpmy_15  +1 cpmy_16  +1 dr_2  <= 1
 c_cpmy_2_1_Mar_15:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_2  <= 1
 c_cpmy_3_1_Apr_1:  +1 cpmy_3  -1 dr_3  <= 0
 c_cpmy_3_1_Apr_2:  +1 cpmy_4  +1 cpmy_5  +1 cpmy_6  +1 dr_3  <= 1
 c_cpmy_3_1_Apr_3:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_3  <= 1
 c_cpmy_3_1_Apr_4:  +1 cpmy_11  +1 cpmy_12  +1 dr_3  <= 1
 c_cpmy_3_1_Apr_5:  +1 cpmy_13  +1 cpmy_14  +1 dr_3  <= 1
 c_cpmy_3_1_Apr_13:  +1 cpmy_15  +1 cpmy_16  +1 dr_3  <= 1
 c_cpmy_3_1_Apr_14:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_3  <= 1
 c_cpmy_4_1_Apr_1:  +1 cpmy_4  -1 dr_4  <= 0
 c_cpmy_4_1_Apr_2:  +1 cpmy_3  +1 cpmy_5  +1 cpmy_6  +1 dr_4  <= 1
 c_cpmy_4_1_Apr_3:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_4  <= 1
 c_cpmy_4_1_Apr_4:  +1 cpmy_11  +1 cpmy_12  +1 dr_4  <= 1
 c_cpmy_4_1_Apr_5:  +1 cpmy_13  +1 cpmy_14  +1 dr_4  <= 1
 c_cpmy_4_1_Apr_13:  +1 cpmy_15  +1 cpmy_16  +1 dr_4  <= 1
 c_cpmy_4_1_Apr_14:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_4  <= 1
 c_cpmy_4_1_Apr_15:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_4  <= 1
 c_cpmy_5_1_Apr_1:  +1 cpmy_5  -1 dr_5  <= 0
 c_cpmy_5_1_Apr_2:  +1 cpmy_3  +1 cpmy_4  +1 cpmy_6  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_3:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_4:  +1 cpmy_11  +1 cpmy_12  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_5:  +1 cpmy_13  +1 cpmy_14  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_13:  +1 cpmy_15  +1 cpmy_16  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_14:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_15:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_5  <= 1
 c_cpmy_5_1_Apr_16:  +1 cpmy_25  +1 cpmy_26  +1 dr_5  <= 1
 c_cpmy_6_1_Apr_1:  +1 cpmy_6  -1 dr_6  <= 0
 c_cpmy_6_1_Apr_2:  +1 cpmy_3  +1 cpmy_4  +1 cpmy_5  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_3:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_4:  +1 cpmy_11  +1 cpmy_12  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_5:  +1 cpmy_13  +1 cpmy_14  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_13:  +1 cpmy_15  +1 cpmy_16  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_14:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_15:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_6  <= 1
 c_cpmy_6_1_Apr_16:  +1 cpmy_25  +1 cpmy_26  +1 dr_6  <= 1
 c_cpmy_7_1_May_1:  +1 cpmy_7  -1 dr_7  <= 0
 c_cpmy_7_1_May_2:  +1 cpmy_8  +1 cpmy_9  +1 cpmy_10  +1 dr_7  <= 1
 c_cpmy_7_1_May_3:  +1 cpmy_11  +1 cpmy_12  +1 dr_7  <= 1
 c_cpmy_7_1_May_4:  +1 cpmy_13  +1 cpmy_14  +1 dr_7  <= 1
 c_cpmy_7_1_May_12:  +1 cpmy_15  +1 cpmy_16  +1 dr_7  <= 1
 c_cpmy_7_1_May_13:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_7  <= 1
 c_cpmy_7_1_May_14:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_7  <= 1
 c_cpmy_8_1_May_1:  +1 cpmy_8  -1 dr_8  <= 0
 c_cpmy_8_1_May_2:  +1 cpmy_7  +1 cpmy_9  +1 cpmy_10  +1 dr_8  <= 1
 c_cpmy_8_1_May_3:  +1 cpmy_11  +1 cpmy_12  +1 dr_8  <= 1
 c_cpmy_8_1_May_4:  +1 cpmy_13  +1 cpmy_14  +1 dr_8  <= 1
 c_cpmy_8_1_May_12:  +1 cpmy_15  +1 cpmy_16  +1 dr_8  <= 1
 c_cpmy_8_1_May_13:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_8  <= 1
 c_cpmy_8_1_May_14:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_8  <= 1
 c_cpmy_8_1_May_15:  +1 cpmy_25  +1 cpmy_26  +1 dr_8  <= 1
 c_cpmy_9_1_May_1:  +1 cpmy_9  -1 dr_9  <= 0
 c_cpmy_9_1_May_2:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_10  +1 dr_9  <= 1
 c_cpmy_9_1_May_3:  +1 cpmy_11  +1 cpmy_12  +1 dr_9  <= 1
 c_cpmy_9_1_May_4:  +1 cpmy_13  +1 cpmy_14  +1 dr_9  <= 1
 c_cpmy_9_1_May_12:  +1 cpmy_15  +1 cpmy_16  +1 dr_9  <= 1
 c_cpmy_9_1_May_13:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_9  <= 1
 c_cpmy_9_1_May_14:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_9  <= 1
 c_cpmy_9_1_May_15:  +1 cpmy_25  +1 cpmy_26  +1 dr_9  <= 1
 c_cpmy_9_1_May_16:  +1 cpmy_27  +1 cpmy_28  +1 dr_9  <= 1
 c_cpmy_10_1_May_1:  +1 cpmy_10  -1 dr_10  <= 0
 c_cpmy_10_1_May_2:  +1 cpmy_7  +1 cpmy_8  +1 cpmy_9  +1 dr_10  <= 1
 c_cpmy_10_1_May_3:  +1 cpmy_11  +1 cpmy_12  +1 dr_10  <= 1
 c_cpmy_10_1_May_4:  +1 cpmy_13  +1 cpmy_14  +1 dr_10  <= 1
 c_cpmy_10_1_May_12:  +1 cpmy_15  +1 cpmy_16  +1 dr_10  <= 1
 c_cpmy_10_1_May_13:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_10  <= 1
 c_cpmy_10_1_May_14:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_10  <= 1
 c_cpmy_10_1_May_15:  +1 cpmy_25  +1 cpmy_26  +1 dr_10  <= 1
 c_cpmy_10_1_May_16:  +1 cpmy_27  +1 cpmy_28  +1 dr_10  <= 1
 c_cpmy_11_1_Jun_1:  +1 cpmy_11  -1 dr_11  <= 0
 c_cpmy_11_1_Jun_2:  +1 cpmy_12  +1 dr_11  <= 1
 c_cpmy_11_1_Jun_3:  +1 cpmy_13  +1 cpmy_14  +1 dr_11  <= 1
 c_cpmy_11_1_Jun_11:  +1 cpmy_15  +1 cpmy_16  +1 dr_11  <= 1
 c_cpmy_11_1_Jun_12:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_11  <= 1
 c_cpmy_11_1_Jun_13:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_11  <= 1
 c_cpmy_11_1_Jun_14:  +1 cpmy_25  +1 cpmy_26  +1 dr_11  <= 1
 c_cpmy_11_1_Jun_15:  +1 cpmy_27  +1 cpmy_28  +1 dr_11  <= 1
 c_cpmy_12_1_Jun_1:  +1 cpmy_12  -1 dr_12  <= 0
 c_cpmy_12_1_Jun_2:  +1 cpmy_11  +1 dr_12  <= 1
 c_cpmy_12_1_Jun_3:  +1 cpmy_13  +1 cpmy_14  +1 dr_12  <= 1
 c_cpmy_12_1_Jun_11:  +1 cpmy_15  +1 cpmy_16  +1 dr_12  <= 1
 c_cpmy_12_1_Jun_12:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_12  <= 1
 c_cpmy_12_1_Jun_13:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_12  <= 1
 c_cpmy_12_1_Jun_14:  +1 cpmy_25  +1 cpmy_26  +1 dr_12  <= 1
 c_cpmy_12_1_Jun_15:  +1 cpmy_27  +1 cpmy_28  +1 dr_12  <= 1
 c_cpmy_13_1_Jul_1:  +1 cpmy_13  -1 dr_13  <= 0
 c_cpmy_13_1_Jul_2:  +1 cpmy_14  +1 dr_13  <= 1
 c_cpmy_13_1_Jul_10:  +1 cpmy_15  +1 cpmy_16  +1 dr_13  <= 1
 c_cpmy_13_1_Jul_11:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_13  <= 1
 c_cpmy_13_1_Jul_12:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_13  <= 1
 c_cpmy_13_1_Jul_13:  +1 cpmy_25  +1 cpmy_26  +1 dr_13  <= 1
 c_cpmy_13_1_Jul_14:  +1 cpmy_27  +1 cpmy_28  +1 dr_13  <= 1
 c_cpmy_14_1_Jul_1:  +1 cpmy_14  -1 dr_14  <= 0
 c_cpmy_14_1_Jul_2:  +1 cpmy_13  +1 dr_14  <= 1
 c_cpmy_14_1_Jul_10:  +1 cpmy_15  +1 cpmy_16  +1 dr_14  <= 1
 c_cpmy_14_1_Jul_11:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_14  <= 1
 c_cpmy_14_1_Jul_12:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_14  <= 1
 c_cpmy_14_1_Jul_13:  +1 cpmy_25  +1 cpmy_26  +1 dr_14  <= 1
 c_cpmy_14_1_Jul_14:  +1 cpmy_27  +1 cpmy_28  +1 dr_14  <= 1
 c_cpmy_15_2_Mar_1:  +1 cpmy_15  -1 dr_15  <= 0
 c_cpmy_15_2_Mar_2:  +1 cpmy_16  +1 dr_15  <= 1
 c_cpmy_15_2_Mar_3:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_15  <= 1
 c_cpmy_15_2_Mar_4:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_15  <= 1
 c_cpmy_15_2_Mar_5:  +1 cpmy_25  +1 cpmy_26  +1 dr_15  <= 1
 c_cpmy_15_2_Mar_6:  +1 cpmy_27  +1 cpmy_28  +1 dr_15  <= 1
 c_cpmy_15_2_Mar_14:  +1 cpmy_29  +1 cpmy_30  +1 dr_15  <= 1
 c_cpmy_16_2_Mar_1:  +1 cpmy_16  -1 dr_16  <= 0
 c_cpmy_16_2_Mar_2:  +1 cpmy_15  +1 dr_16  <= 1
 c_cpmy_16_2_Mar_3:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_16  <= 1
 c_cpmy_16_2_Mar_4:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_16  <= 1
 c_cpmy_16_2_Mar_5:  +1 cpmy_25  +1 cpmy_26  +1 dr_16  <= 1
 c_cpmy_16_2_Mar_6:  +1 cpmy_27  +1 cpmy_28  +1 dr_16  <= 1
 c_cpmy_16_2_Mar_14:  +1 cpmy_29  +1 cpmy_30  +1 dr_16  <= 1
 c_cpmy_16_2_Mar_15:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_16  <= 1
 c_cpmy_17_2_Apr_1:  +1 cpmy_17  -1 dr_17  <= 0
 c_cpmy_17_2_Apr_2:  +1 cpmy_18  +1 cpmy_19  +1 cpmy_20  +1 dr_17  <= 1
 c_cpmy_17_2_Apr_3:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_17  <= 1
 c_cpmy_17_2_Apr_4:  +1 cpmy_25  +1 cpmy_26  +1 dr_17  <= 1
 c_cpmy_17_2_Apr_5:  +1 cpmy_27  +1 cpmy_28  +1 dr_17  <= 1
 c_cpmy_17_2_Apr_13:  +1 cpmy_29  +1 cpmy_30  +1 dr_17  <= 1
 c_cpmy_17_2_Apr_14:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_17  <= 1
 c_cpmy_18_2_Apr_1:  +1 cpmy_18  -1 dr_18  <= 0
 c_cpmy_18_2_Apr_2:  +1 cpmy_17  +1 cpmy_19  +1 cpmy_20  +1 dr_18  <= 1
 c_cpmy_18_2_Apr_3:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_18  <= 1
 c_cpmy_18_2_Apr_4:  +1 cpmy_25  +1 cpmy_26  +1 dr_18  <= 1
 c_cpmy_18_2_Apr_5:  +1 cpmy_27  +1 cpmy_28  +1 dr_18  <= 1
 c_cpmy_18_2_Apr_13:  +1 cpmy_29  +1 cpmy_30  +1 dr_18  <= 1
 c_cpmy_18_2_Apr_14:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_18  <= 1
 c_cpmy_18_2_Apr_15:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_18  <= 1
 c_cpmy_19_2_Apr_1:  +1 cpmy_19  -1 dr_19  <= 0
 c_cpmy_19_2_Apr_2:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_20  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_3:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_4:  +1 cpmy_25  +1 cpmy_26  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_5:  +1 cpmy_27  +1 cpmy_28  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_13:  +1 cpmy_29  +1 cpmy_30  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_14:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_15:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_19  <= 1
 c_cpmy_19_2_Apr_16:  +1 cpmy_39  +1 cpmy_40  +1 dr_19  <= 1
 c_cpmy_20_2_Apr_1:  +1 cpmy_20  -1 dr_20  <= 0
 c_cpmy_20_2_Apr_2:  +1 cpmy_17  +1 cpmy_18  +1 cpmy_19  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_3:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_4:  +1 cpmy_25  +1 cpmy_26  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_5:  +1 cpmy_27  +1 cpmy_28  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_13:  +1 cpmy_29  +1 cpmy_30  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_14:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_15:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_20  <= 1
 c_cpmy_20_2_Apr_16:  +1 cpmy_39  +1 cpmy_40  +1 dr_20  <= 1
 c_cpmy_21_2_May_1:  +1 cpmy_21  -1 dr_21  <= 0
 c_cpmy_21_2_May_2:  +1 cpmy_22  +1 cpmy_23  +1 cpmy_24  +1 dr_21  <= 1
 c_cpmy_21_2_May_3:  +1 cpmy_25  +1 cpmy_26  +1 dr_21  <= 1
 c_cpmy_21_2_May_4:  +1 cpmy_27  +1 cpmy_28  +1 dr_21  <= 1
 c_cpmy_21_2_May_12:  +1 cpmy_29  +1 cpmy_30  +1 dr_21  <= 1
 c_cpmy_21_2_May_13:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_21  <= 1
 c_cpmy_21_2_May_14:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_21  <= 1
 c_cpmy_22_2_May_1:  +1 cpmy_22  -1 dr_22  <= 0
 c_cpmy_22_2_May_2:  +1 cpmy_21  +1 cpmy_23  +1 cpmy_24  +1 dr_22  <= 1
 c_cpmy_22_2_May_3:  +1 cpmy_25  +1 cpmy_26  +1 dr_22  <= 1
 c_cpmy_22_2_May_4:  +1 cpmy_27  +1 cpmy_28  +1 dr_22  <= 1
 c_cpmy_22_2_May_12:  +1 cpmy_29  +1 cpmy_30  +1 dr_22  <= 1
 c_cpmy_22_2_May_13:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_22  <= 1
 c_cpmy_22_2_May_14:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_22  <= 1
 c_cpmy_22_2_May_15:  +1 cpmy_39  +1 cpmy_40  +1 dr_22  <= 1
 c_cpmy_23_2_May_1:  +1 cpmy_23  -1 dr_23  <= 0
 c_cpmy_23_2_May_2:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_24  +1 dr_23  <= 1
 c_cpmy_23_2_May_3:  +1 cpmy_25  +1 cpmy_26  +1 dr_23  <= 1
 c_cpmy_23_2_May_4:  +1 cpmy_27  +1 cpmy_28  +1 dr_23  <= 1
 c_cpmy_23_2_May_12:  +1 cpmy_29  +1 cpmy_30  +1 dr_23  <= 1
 c_cpmy_23_2_May_13:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_23  <= 1
 c_cpmy_23_2_May_14:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_23  <= 1
 c_cpmy_23_2_May_15:  +1 cpmy_39  +1 cpmy_40  +1 dr_23  <= 1
 c_cpmy_23_2_May_16:  +1 cpmy_41  +1 cpmy_42  +1 dr_23  <= 1
 c_cpmy_24_2_May_1:  +1 cpmy_24  -1 dr_24  <= 0
 c_cpmy_24_2_May_2:  +1 cpmy_21  +1 cpmy_22  +1 cpmy_23  +1 dr_24  <= 1
 c_cpmy_24_2_May_3:  +1 cpmy_25  +1 cpmy_26  +1 dr_24  <= 1
 c_cpmy_24_2_May_4:  +1 cpmy_27  +1 cpmy_28  +1 dr_24  <= 1
 c_cpmy_24_2_May_12:  +1 cpmy_29  +1 cpmy_30  +1 dr_24  <= 1
 c_cpmy_24_2_May_13:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_24  <= 1
 c_cpmy_24_2_May_14:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_24  <= 1
 c_cpmy_24_2_May_15:  +1 cpmy_39  +1 cpmy_40  +1 dr_24  <= 1
 c_cpmy_24_2_May_16:  +1 cpmy_41  +1 cpmy_42  +1 dr_24  <= 1
 c_cpmy_25_2_Jun_1:  +1 cpmy_25  -1 dr_25  <= 0
 c_cpmy_25_2_Jun_2:  +1 cpmy_26  +1 dr_25  <= 1
 c_cpmy_25_2_Jun_3:  +1 cpmy_27  +1 cpmy_28  +1 dr_25  <= 1
 c_cpmy_25_2_Jun_11:  +1 cpmy_29  +1 cpmy_30  +1 dr_25  <= 1
 c_cpmy_25_2_Jun_12:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_25  <= 1
 c_cpmy_25_2_Jun_13:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_25  <= 1
 c_cpmy_25_2_Jun_14:  +1 cpmy_39  +1 cpmy_40  +1 dr_25  <= 1
 c_cpmy_25_2_Jun_15:  +1 cpmy_41  +1 cpmy_42  +1 dr_25  <= 1
 c_cpmy_26_2_Jun_1:  +1 cpmy_26  -1 dr_26  <= 0
 c_cpmy_26_2_Jun_2:  +1 cpmy_25  +1 dr_26  <= 1
 c_cpmy_26_2_Jun_3:  +1 cpmy_27  +1 cpmy_28  +1 dr_26  <= 1
 c_cpmy_26_2_Jun_11:  +1 cpmy_29  +1 cpmy_30  +1 dr_26  <= 1
 c_cpmy_26_2_Jun_12:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_26  <= 1
 c_cpmy_26_2_Jun_13:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_26  <= 1
 c_cpmy_26_2_Jun_14:  +1 cpmy_39  +1 cpmy_40  +1 dr_26  <= 1
 c_cpmy_26_2_Jun_15:  +1 cpmy_41  +1 cpmy_42  +1 dr_26  <= 1
 c_cpmy_27_2_Jul_1:  +1 cpmy_27  -1 dr_27  <= 0
 c_cpmy_27_2_Jul_2:  +1 cpmy_28  +1 dr_27  <= 1
 c_cpmy_27_2_Jul_10:  +1 cpmy_29  +1 cpmy_30  +1 dr_27  <= 1
 c_cpmy_27_2_Jul_11:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_27  <= 1
 c_cpmy_27_2_Jul_12:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_27  <= 1
 c_cpmy_27_2_Jul_13:  +1 cpmy_39  +1 cpmy_40  +1 dr_27  <= 1
 c_cpmy_27_2_Jul_14:  +1 cpmy_41  +1 cpmy_42  +1 dr_27  <= 1
 c_cpmy_28_2_Jul_1:  +1 cpmy_28  -1 dr_28  <= 0
 c_cpmy_28_2_Jul_2:  +1 cpmy_27  +1 dr_28  <= 1
 c_cpmy_28_2_Jul_10:  +1 cpmy_29  +1 cpmy_30  +1 dr_28  <= 1
 c_cpmy_28_2_Jul_11:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_28  <= 1
 c_cpmy_28_2_Jul_12:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_28  <= 1
 c_cpmy_28_2_Jul_13:  +1 cpmy_39  +1 cpmy_40  +1 dr_28  <= 1
 c_cpmy_28_2_Jul_14:  +1 cpmy_41  +1 cpmy_42  +1 dr_28  <= 1
 c_cpmy_29_3_Mar_1:  +1 cpmy_29  -1 dr_29  <= 0
 c_cpmy_29_3_Mar_2:  +1 cpmy_30  +1 dr_29  <= 1
 c_cpmy_29_3_Mar_3:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_29  <= 1
 c_cpmy_29_3_Mar_4:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_29  <= 1
 c_cpmy_29_3_Mar_5:  +1 cpmy_39  +1 cpmy_40  +1 dr_29  <= 1
 c_cpmy_29_3_Mar_6:  +1 cpmy_41  +1 cpmy_42  +1 dr_29  <= 1
 c_cpmy_29_3_Mar_14:  +1 cpmy_43  +1 cpmy_44  +1 dr_29  <= 1
 c_cpmy_30_3_Mar_1:  +1 cpmy_30  -1 dr_30  <= 0
 c_cpmy_30_3_Mar_2:  +1 cpmy_29  +1 dr_30  <= 1
 c_cpmy_30_3_Mar_3:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_30  <= 1
 c_cpmy_30_3_Mar_4:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_30  <= 1
 c_cpmy_30_3_Mar_5:  +1 cpmy_39  +1 cpmy_40  +1 dr_30  <= 1
 c_cpmy_30_3_Mar_6:  +1 cpmy_41  +1 cpmy_42  +1 dr_30  <= 1
 c_cpmy_30_3_Mar_14:  +1 cpmy_43  +1 cpmy_44  +1 dr_30  <= 1
 c_cpmy_30_3_Mar_15:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_30  <= 1
 c_cpmy_31_3_Apr_1:  +1 cpmy_31  -1 dr_31  <= 0
 c_cpmy_31_3_Apr_2:  +1 cpmy_32  +1 cpmy_33  +1 cpmy_34  +1 dr_31  <= 1
 c_cpmy_31_3_Apr_3:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_31  <= 1
 c_cpmy_31_3_Apr_4:  +1 cpmy_39  +1 cpmy_40  +1 dr_31  <= 1
 c_cpmy_31_3_Apr_5:  +1 cpmy_41  +1 cpmy_42  +1 dr_31  <= 1
 c_cpmy_31_3_Apr_13:  +1 cpmy_43  +1 cpmy_44  +1 dr_31  <= 1
 c_cpmy_31_3_Apr_14:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_31  <= 1
 c_cpmy_32_3_Apr_1:  +1 cpmy_32  -1 dr_32  <= 0
 c_cpmy_32_3_Apr_2:  +1 cpmy_31  +1 cpmy_33  +1 cpmy_34  +1 dr_32  <= 1
 c_cpmy_32_3_Apr_3:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_32  <= 1
 c_cpmy_32_3_Apr_4:  +1 cpmy_39  +1 cpmy_40  +1 dr_32  <= 1
 c_cpmy_32_3_Apr_5:  +1 cpmy_41  +1 cpmy_42  +1 dr_32  <= 1
 c_cpmy_32_3_Apr_13:  +1 cpmy_43  +1 cpmy_44  +1 dr_32  <= 1
 c_cpmy_32_3_Apr_14:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_32  <= 1
 c_cpmy_32_3_Apr_15:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_32  <= 1
 c_cpmy_33_3_Apr_1:  +1 cpmy_33  -1 dr_33  <= 0
 c_cpmy_33_3_Apr_2:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_34  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_3:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_4:  +1 cpmy_39  +1 cpmy_40  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_5:  +1 cpmy_41  +1 cpmy_42  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_13:  +1 cpmy_43  +1 cpmy_44  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_14:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_15:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_33  <= 1
 c_cpmy_33_3_Apr_16:  +1 cpmy_53  +1 cpmy_54  +1 dr_33  <= 1
 c_cpmy_34_3_Apr_1:  +1 cpmy_34  -1 dr_34  <= 0
 c_cpmy_34_3_Apr_2:  +1 cpmy_31  +1 cpmy_32  +1 cpmy_33  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_3:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_4:  +1 cpmy_39  +1 cpmy_40  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_5:  +1 cpmy_41  +1 cpmy_42  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_13:  +1 cpmy_43  +1 cpmy_44  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_14:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_15:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_34  <= 1
 c_cpmy_34_3_Apr_16:  +1 cpmy_53  +1 cpmy_54  +1 dr_34  <= 1
 c_cpmy_35_3_May_1:  +1 cpmy_35  -1 dr_35  <= 0
 c_cpmy_35_3_May_2:  +1 cpmy_36  +1 cpmy_37  +1 cpmy_38  +1 dr_35  <= 1
 c_cpmy_35_3_May_3:  +1 cpmy_39  +1 cpmy_40  +1 dr_35  <= 1
 c_cpmy_35_3_May_4:  +1 cpmy_41  +1 cpmy_42  +1 dr_35  <= 1
 c_cpmy_35_3_May_12:  +1 cpmy_43  +1 cpmy_44  +1 dr_35  <= 1
 c_cpmy_35_3_May_13:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_35  <= 1
 c_cpmy_35_3_May_14:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_35  <= 1
 c_cpmy_36_3_May_1:  +1 cpmy_36  -1 dr_36  <= 0
 c_cpmy_36_3_May_2:  +1 cpmy_35  +1 cpmy_37  +1 cpmy_38  +1 dr_36  <= 1
 c_cpmy_36_3_May_3:  +1 cpmy_39  +1 cpmy_40  +1 dr_36  <= 1
 c_cpmy_36_3_May_4:  +1 cpmy_41  +1 cpmy_42  +1 dr_36  <= 1
 c_cpmy_36_3_May_12:  +1 cpmy_43  +1 cpmy_44  +1 dr_36  <= 1
 c_cpmy_36_3_May_13:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_36  <= 1
 c_cpmy_36_3_May_14:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_36  <= 1
 c_cpmy_36_3_May_15:  +1 cpmy_53  +1 cpmy_54  +1 dr_36  <= 1
 c_cpmy_37_3_May_1:  +1 cpmy_37  -1 dr_37  <= 0
 c_cpmy_37_3_May_2:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_38  +1 dr_37  <= 1
 c_cpmy_37_3_May_3:  +1 cpmy_39  +1 cpmy_40  +1 dr_37  <= 1
 c_cpmy_37_3_May_4:  +1 cpmy_41  +1 cpmy_42  +1 dr_37  <= 1
 c_cpmy_37_3_May_12:  +1 cpmy_43  +1 cpmy_44  +1 dr_37  <= 1
 c_cpmy_37_3_May_13:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_37  <= 1
 c_cpmy_37_3_May_14:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_37  <= 1
 c_cpmy_37_3_May_15:  +1 cpmy_53  +1 cpmy_54  +1 dr_37  <= 1
 c_cpmy_37_3_May_16:  +1 cpmy_55  +1 cpmy_56  +1 dr_37  <= 1
 c_cpmy_38_3_May_1:  +1 cpmy_38  -1 dr_38  <= 0
 c_cpmy_38_3_May_2:  +1 cpmy_35  +1 cpmy_36  +1 cpmy_37  +1 dr_38  <= 1
 c_cpmy_38_3_May_3:  +1 cpmy_39  +1 cpmy_40  +1 dr_38  <= 1
 c_cpmy_38_3_May_4:  +1 cpmy_41  +1 cpmy_42  +1 dr_38  <= 1
 c_cpmy_38_3_May_12:  +1 cpmy_43  +1 cpmy_44  +1 dr_38  <= 1
 c_cpmy_38_3_May_13:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_38  <= 1
 c_cpmy_38_3_May_14:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_38  <= 1
 c_cpmy_38_3_May_15:  +1 cpmy_53  +1 cpmy_54  +1 dr_38  <= 1
 c_cpmy_38_3_May_16:  +1 cpmy_55  +1 cpmy_56  +1 dr_38  <= 1
 c_cpmy_39_3_Jun_1:  +1 cpmy_39  -1 dr_39  <= 0
 c_cpmy_39_3_Jun_2:  +1 cpmy_40  +1 dr_39  <= 1
 c_cpmy_39_3_Jun_3:  +1 cpmy_41  +1 cpmy_42  +1 dr_39  <= 1
 c_cpmy_39_3_Jun_11:  +1 cpmy_43  +1 cpmy_44  +1 dr_39  <= 1
 c_cpmy_39_3_Jun_12:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_39  <= 1
 c_cpmy_39_3_Jun_13:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_39  <= 1
 c_cpmy_39_3_Jun_14:  +1 cpmy_53  +1 cpmy_54  +1 dr_39  <= 1
 c_cpmy_39_3_Jun_15:  +1 cpmy_55  +1 cpmy_56  +1 dr_39  <= 1
 c_cpmy_40_3_Jun_1:  +1 cpmy_40  -1 dr_40  <= 0
 c_cpmy_40_3_Jun_2:  +1 cpmy_39  +1 dr_40  <= 1
 c_cpmy_40_3_Jun_3:  +1 cpmy_41  +1 cpmy_42  +1 dr_40  <= 1
 c_cpmy_40_3_Jun_11:  +1 cpmy_43  +1 cpmy_44  +1 dr_40  <= 1
 c_cpmy_40_3_Jun_12:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_40  <= 1
 c_cpmy_40_3_Jun_13:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_40  <= 1
 c_cpmy_40_3_Jun_14:  +1 cpmy_53  +1 cpmy_54  +1 dr_40  <= 1
 c_cpmy_40_3_Jun_15:  +1 cpmy_55  +1 cpmy_56  +1 dr_40  <= 1
 c_cpmy_41_3_Jul_1:  +1 cpmy_41  -1 dr_41  <= 0
 c_cpmy_41_3_Jul_2:  +1 cpmy_42  +1 dr_41  <= 1
 c_cpmy_41_3_Jul_10:  +1 cpmy_43  +1 cpmy_44  +1 dr_41  <= 1
 c_cpmy_41_3_Jul_11:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_41  <= 1
 c_cpmy_41_3_Jul_12:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_41  <= 1
 c_cpmy_41_3_Jul_13:  +1 cpmy_53  +1 cpmy_54  +1 dr_41  <= 1
 c_cpmy_41_3_Jul_14:  +1 cpmy_55  +1 cpmy_56  +1 dr_41  <= 1
 c_cpmy_42_3_Jul_1:  +1 cpmy_42  -1 dr_42  <= 0
 c_cpmy_42_3_Jul_2:  +1 cpmy_41  +1 dr_42  <= 1
 c_cpmy_42_3_Jul_10:  +1 cpmy_43  +1 cpmy_44  +1 dr_42  <= 1
 c_cpmy_42_3_Jul_11:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_42  <= 1
 c_cpmy_42_3_Jul_12:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_42  <= 1
 c_cpmy_42_3_Jul_13:  +1 cpmy_53  +1 cpmy_54  +1 dr_42  <= 1
 c_cpmy_42_3_Jul_14:  +1 cpmy_55  +1 cpmy_56  +1 dr_42  <= 1
 c_cpmy_43_4_Mar_1:  +1 cpmy_43  -1 dr_43  <= 0
 c_cpmy_43_4_Mar_2:  +1 cpmy_44  +1 dr_43  <= 1
 c_cpmy_43_4_Mar_3:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_43  <= 1
 c_cpmy_43_4_Mar_4:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_43  <= 1
 c_cpmy_43_4_Mar_5:  +1 cpmy_53  +1 cpmy_54  +1 dr_43  <= 1
 c_cpmy_43_4_Mar_6:  +1 cpmy_55  +1 cpmy_56  +1 dr_43  <= 1
 c_cpmy_44_4_Mar_1:  +1 cpmy_44  -1 dr_44  <= 0
 c_cpmy_44_4_Mar_2:  +1 cpmy_43  +1 dr_44  <= 1
 c_cpmy_44_4_Mar_3:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_44  <= 1
 c_cpmy_44_4_Mar_4:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_44  <= 1
 c_cpmy_44_4_Mar_5:  +1 cpmy_53  +1 cpmy_54  +1 dr_44  <= 1
 c_cpmy_44_4_Mar_6:  +1 cpmy_55  +1 cpmy_56  +1 dr_44  <= 1
 c_cpmy_45_4_Apr_1:  +1 cpmy_45  -1 dr_45  <= 0
 c_cpmy_45_4_Apr_2:  +1 cpmy_46  +1 cpmy_47  +1 cpmy_48  +1 dr_45  <= 1
 c_cpmy_45_4_Apr_3:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_45  <= 1
 c_cpmy_45_4_Apr_4:  +1 cpmy_53  +1 cpmy_54  +1 dr_45  <= 1
 c_cpmy_45_4_Apr_5:  +1 cpmy_55  +1 cpmy_56  +1 dr_45  <= 1
 c_cpmy_46_4_Apr_1:  +1 cpmy_46  -1 dr_46  <= 0
 c_cpmy_46_4_Apr_2:  +1 cpmy_45  +1 cpmy_47  +1 cpmy_48  +1 dr_46  <= 1
 c_cpmy_46_4_Apr_3:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_46  <= 1
 c_cpmy_46_4_Apr_4:  +1 cpmy_53  +1 cpmy_54  +1 dr_46  <= 1
 c_cpmy_46_4_Apr_5:  +1 cpmy_55  +1 cpmy_56  +1 dr_46  <= 1
 c_cpmy_47_4_Apr_1:  +1 cpmy_47  -1 dr_47  <= 0
 c_cpmy_47_4_Apr_2:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_48  +1 dr_47  <= 1
 c_cpmy_47_4_Apr_3:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_47  <= 1
 c_cpmy_47_4_Apr_4:  +1 cpmy_53  +1 cpmy_54  +1 dr_47  <= 1
 c_cpmy_47_4_Apr_5:  +1 cpmy_55  +1 cpmy_56  +1 dr_47  <= 1
 c_cpmy_48_4_Apr_1:  +1 cpmy_48  -1 dr_48  <= 0
 c_cpmy_48_4_Apr_2:  +1 cpmy_45  +1 cpmy_46  +1 cpmy_47  +1 dr_48  <= 1
 c_cpmy_48_4_Apr_3:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_48  <= 1
 c_cpmy_48_4_Apr_4:  +1 cpmy_53  +1 cpmy_54  +1 dr_48  <= 1
 c_cpmy_48_4_Apr_5:  +1 cpmy_55  +1 cpmy_56  +1 dr_48  <= 1
 c_cpmy_49_4_May_1:  +1 cpmy_49  -1 dr_49  <= 0
 c_cpmy_49_4_May_2:  +1 cpmy_50  +1 cpmy_51  +1 cpmy_52  +1 dr_49  <= 1
 c_cpmy_49_4_May_3:  +1 cpmy_53  +1 cpmy_54  +1 dr_49  <= 1
 c_cpmy_49_4_May_4:  +1 cpmy_55  +1 cpmy_56  +1 dr_49  <= 1
 c_cpmy_50_4_May_1:  +1 cpmy_50  -1 dr_50  <= 0
 c_cpmy_50_4_May_2:  +1 cpmy_49  +1 cpmy_51  +1 cpmy_52  +1 dr_50  <= 1
 c_cpmy_50_4_May_3:  +1 cpmy_53  +1 cpmy_54  +1 dr_50  <= 1
 c_cpmy_50_4_May_4:  +1 cpmy_55  +1 cpmy_56  +1 dr_50  <= 1
 c_cpmy_51_4_May_1:  +1 cpmy_51  -1 dr_51  <= 0
 c_cpmy_51_4_May_2:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_52  +1 dr_51  <= 1
 c_cpmy_51_4_May_3:  +1 cpmy_53  +1 cpmy_54  +1 dr_51  <= 1
 c_cpmy_51_4_May_4:  +1 cpmy_55  +1 cpmy_56  +1 dr_51  <= 1
 c_cpmy_52_4_May_1:  +1 cpmy_52  -1 dr_52  <= 0
 c_cpmy_52_4_May_2:  +1 cpmy_49  +1 cpmy_50  +1 cpmy_51  +1 dr_52  <= 1
 c_cpmy_52_4_May_3:  +1 cpmy_53  +1 cpmy_54  +1 dr_52  <= 1
 c_cpmy_52_4_May_4:  +1 cpmy_55  +1 cpmy_56  +1 dr_52  <= 1
 c_cpmy_53_4_Jun_1:  +1 cpmy_53  -1 dr_53  <= 0
 c_cpmy_53_4_Jun_2:  +1 cpmy_54  +1 dr_53  <= 1
 c_cpmy_53_4_Jun_3:  +1 cpmy_55  +1 cpmy_56  +1 dr_53  <= 1
 c_cpmy_54_4_Jun_1:  +1 cpmy_54  -1 dr_54  <= 0
 c_cpmy_54_4_Jun_2:  +1 cpmy_53  +1 dr_54  <= 1
 c_cpmy_54_4_Jun_3:  +1 cpmy_55  +1 cpmy_56  +1 dr_54  <= 1
 c_cpmy_55_4_Jul_1:  +1 cpmy_55  -1 dr_55  <= 0
 c_cpmy_55_4_Jul_2:  +1 cpmy_56  +1 dr_55  <= 1
 c_cpmy_56_4_Jul_1:  +1 cpmy_56  -1 dr_56  <= 0
 c_cpmy_56_4_Jul_2:  +1 cpmy_55  +1 dr_56  <= 1
 c_cpmy_57_1_Mar_1:  +1 cpmy_57  -1 dr_57  <= 0
 c_cpmy_57_1_Mar_2:  +1 cpmy_58  +1 dr_57  <= 1
 c_cpmy_57_1_Mar_3:  +1 cpmy_59  +1 cpmy_60  +1 cpmy_61  +1 cpmy_62  +1 dr_57  <= 1
 c_cpmy_57_1_Mar_4:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_57  <= 1
 c_cpmy_57_1_Mar_5:  +1 cpmy_67  +1 cpmy_68  +1 dr_57  <= 1
 c_cpmy_57_1_Mar_6:  +1 cpmy_69  +1 cpmy_70  +1 dr_57  <= 1
 c_cpmy_57_1_Mar_14:  +1 cpmy_71  +1 cpmy_72  +1 dr_57  <= 1
 c_cpmy_58_1_Mar_1:  +1 cpmy_58  -1 dr_58  <= 0
 c_cpmy_58_1_Mar_2:  +1 cpmy_57  +1 dr_58  <= 1
 c_cpmy_58_1_Mar_3:  +1 cpmy_59  +1 cpmy_60  +1 cpmy_61  +1 cpmy_62  +1 dr_58  <= 1
 c_cpmy_58_1_Mar_4:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_58  <= 1
 c_cpmy_58_1_Mar_5:  +1 cpmy_67  +1 cpmy_68  +1 dr_58  <= 1
 c_cpmy_58_1_Mar_6:  +1 cpmy_69  +1 cpmy_70  +1 dr_58  <= 1
 c_cpmy_58_1_Mar_14:  +1 cpmy_71  +1 cpmy_72  +1 dr_58  <= 1
 c_cpmy_58_1_Mar_15:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_58  <= 1
 c_cpmy_59_1_Apr_1:  +1 cpmy_59  -1 dr_59  <= 0
 c_cpmy_59_1_Apr_2:  +1 cpmy_60  +1 cpmy_61  +1 cpmy_62  +1 dr_59  <= 1
 c_cpmy_59_1_Apr_3:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_59  <= 1
 c_cpmy_59_1_Apr_4:  +1 cpmy_67  +1 cpmy_68  +1 dr_59  <= 1
 c_cpmy_59_1_Apr_5:  +1 cpmy_69  +1 cpmy_70  +1 dr_59  <= 1
 c_cpmy_59_1_Apr_13:  +1 cpmy_71  +1 cpmy_72  +1 dr_59  <= 1
 c_cpmy_59_1_Apr_14:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_59  <= 1
 c_cpmy_60_1_Apr_1:  +1 cpmy_60  -1 dr_60  <= 0
 c_cpmy_60_1_Apr_2:  +1 cpmy_59  +1 cpmy_61  +1 cpmy_62  +1 dr_60  <= 1
 c_cpmy_60_1_Apr_3:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_60  <= 1
 c_cpmy_60_1_Apr_4:  +1 cpmy_67  +1 cpmy_68  +1 dr_60  <= 1
 c_cpmy_60_1_Apr_5:  +1 cpmy_69  +1 cpmy_70  +1 dr_60  <= 1
 c_cpmy_60_1_Apr_13:  +1 cpmy_71  +1 cpmy_72  +1 dr_60  <= 1
 c_cpmy_60_1_Apr_14:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_60  <= 1
 c_cpmy_60_1_Apr_15:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_60  <= 1
 c_cpmy_61_1_Apr_1:  +1 cpmy_61  -1 dr_61  <= 0
 c_cpmy_61_1_Apr_2:  +1 cpmy_59  +1 cpmy_60  +1 cpmy_62  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_3:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_4:  +1 cpmy_67  +1 cpmy_68  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_5:  +1 cpmy_69  +1 cpmy_70  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_13:  +1 cpmy_71  +1 cpmy_72  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_14:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_15:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_61  <= 1
 c_cpmy_61_1_Apr_16:  +1 cpmy_81  +1 cpmy_82  +1 dr_61  <= 1
 c_cpmy_62_1_Apr_1:  +1 cpmy_62  -1 dr_62  <= 0
 c_cpmy_62_1_Apr_2:  +1 cpmy_59  +1 cpmy_60  +1 cpmy_61  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_3:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_4:  +1 cpmy_67  +1 cpmy_68  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_5:  +1 cpmy_69  +1 cpmy_70  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_13:  +1 cpmy_71  +1 cpmy_72  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_14:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_15:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_62  <= 1
 c_cpmy_62_1_Apr_16:  +1 cpmy_81  +1 cpmy_82  +1 dr_62  <= 1
 c_cpmy_63_1_May_1:  +1 cpmy_63  -1 dr_63  <= 0
 c_cpmy_63_1_May_2:  +1 cpmy_64  +1 cpmy_65  +1 cpmy_66  +1 dr_63  <= 1
 c_cpmy_63_1_May_3:  +1 cpmy_67  +1 cpmy_68  +1 dr_63  <= 1
 c_cpmy_63_1_May_4:  +1 cpmy_69  +1 cpmy_70  +1 dr_63  <= 1
 c_cpmy_63_1_May_12:  +1 cpmy_71  +1 cpmy_72  +1 dr_63  <= 1
 c_cpmy_63_1_May_13:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_63  <= 1
 c_cpmy_63_1_May_14:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_63  <= 1
 c_cpmy_64_1_May_1:  +1 cpmy_64  -1 dr_64  <= 0
 c_cpmy_64_1_May_2:  +1 cpmy_63  +1 cpmy_65  +1 cpmy_66  +1 dr_64  <= 1
 c_cpmy_64_1_May_3:  +1 cpmy_67  +1 cpmy_68  +1 dr_64  <= 1
 c_cpmy_64_1_May_4:  +1 cpmy_69  +1 cpmy_70  +1 dr_64  <= 1
 c_cpmy_64_1_May_12:  +1 cpmy_71  +1 cpmy_72  +1 dr_64  <= 1
 c_cpmy_64_1_May_13:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_64  <= 1
 c_cpmy_64_1_May_14:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_64  <= 1
 c_cpmy_64_1_May_15:  +1 cpmy_81  +1 cpmy_82  +1 dr_64  <= 1
 c_cpmy_65_1_May_1:  +1 cpmy_65  -1 dr_65  <= 0
 c_cpmy_65_1_May_2:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_66  +1 dr_65  <= 1
 c_cpmy_65_1_May_3:  +1 cpmy_67  +1 cpmy_68  +1 dr_65  <= 1
 c_cpmy_65_1_May_4:  +1 cpmy_69  +1 cpmy_70  +1 dr_65  <= 1
 c_cpmy_65_1_May_12:  +1 cpmy_71  +1 cpmy_72  +1 dr_65  <= 1
 c_cpmy_65_1_May_13:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_65  <= 1
 c_cpmy_65_1_May_14:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_65  <= 1
 c_cpmy_65_1_May_15:  +1 cpmy_81  +1 cpmy_82  +1 dr_65  <= 1
 c_cpmy_65_1_May_16:  +1 cpmy_83  +1 cpmy_84  +1 dr_65  <= 1
 c_cpmy_66_1_May_1:  +1 cpmy_66  -1 dr_66  <= 0
 c_cpmy_66_1_May_2:  +1 cpmy_63  +1 cpmy_64  +1 cpmy_65  +1 dr_66  <= 1
 c_cpmy_66_1_May_3:  +1 cpmy_67  +1 cpmy_68  +1 dr_66  <= 1
 c_cpmy_66_1_May_4:  +1 cpmy_69  +1 cpmy_70  +1 dr_66  <= 1
 c_cpmy_66_1_May_12:  +1 cpmy_71  +1 cpmy_72  +1 dr_66  <= 1
 c_cpmy_66_1_May_13:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_66  <= 1
 c_cpmy_66_1_May_14:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_66  <= 1
 c_cpmy_66_1_May_15:  +1 cpmy_81  +1 cpmy_82  +1 dr_66  <= 1
 c_cpmy_66_1_May_16:  +1 cpmy_83  +1 cpmy_84  +1 dr_66  <= 1
 c_cpmy_67_1_Jun_1:  +1 cpmy_67  -1 dr_67  <= 0
 c_cpmy_67_1_Jun_2:  +1 cpmy_68  +1 dr_67  <= 1
 c_cpmy_67_1_Jun_3:  +1 cpmy_69  +1 cpmy_70  +1 dr_67  <= 1
 c_cpmy_67_1_Jun_11:  +1 cpmy_71  +1 cpmy_72  +1 dr_67  <= 1
 c_cpmy_67_1_Jun_12:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_67  <= 1
 c_cpmy_67_1_Jun_13:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_67  <= 1
 c_cpmy_67_1_Jun_14:  +1 cpmy_81  +1 cpmy_82  +1 dr_67  <= 1
 c_cpmy_67_1_Jun_15:  +1 cpmy_83  +1 cpmy_84  +1 dr_67  <= 1
 c_cpmy_68_1_Jun_1:  +1 cpmy_68  -1 dr_68  <= 0
 c_cpmy_68_1_Jun_2:  +1 cpmy_67  +1 dr_68  <= 1
 c_cpmy_68_1_Jun_3:  +1 cpmy_69  +1 cpmy_70  +1 dr_68  <= 1
 c_cpmy_68_1_Jun_11:  +1 cpmy_71  +1 cpmy_72  +1 dr_68  <= 1
 c_cpmy_68_1_Jun_12:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_68  <= 1
 c_cpmy_68_1_Jun_13:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_68  <= 1
 c_cpmy_68_1_Jun_14:  +1 cpmy_81  +1 cpmy_82  +1 dr_68  <= 1
 c_cpmy_68_1_Jun_15:  +1 cpmy_83  +1 cpmy_84  +1 dr_68  <= 1
 c_cpmy_69_1_Jul_1:  +1 cpmy_69  -1 dr_69  <= 0
 c_cpmy_69_1_Jul_2:  +1 cpmy_70  +1 dr_69  <= 1
 c_cpmy_69_1_Jul_10:  +1 cpmy_71  +1 cpmy_72  +1 dr_69  <= 1
 c_cpmy_69_1_Jul_11:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_69  <= 1
 c_cpmy_69_1_Jul_12:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_69  <= 1
 c_cpmy_69_1_Jul_13:  +1 cpmy_81  +1 cpmy_82  +1 dr_69  <= 1
 c_cpmy_69_1_Jul_14:  +1 cpmy_83  +1 cpmy_84  +1 dr_69  <= 1
 c_cpmy_70_1_Jul_1:  +1 cpmy_70  -1 dr_70  <= 0
 c_cpmy_70_1_Jul_2:  +1 cpmy_69  +1 dr_70  <= 1
 c_cpmy_70_1_Jul_10:  +1 cpmy_71  +1 cpmy_72  +1 dr_70  <= 1
 c_cpmy_70_1_Jul_11:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_70  <= 1
 c_cpmy_70_1_Jul_12:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_70  <= 1
 c_cpmy_70_1_Jul_13:  +1 cpmy_81  +1 cpmy_82  +1 dr_70  <= 1
 c_cpmy_70_1_Jul_14:  +1 cpmy_83  +1 cpmy_84  +1 dr_70  <= 1
 c_cpmy_71_2_Mar_1:  +1 cpmy_71  -1 dr_71  <= 0
 c_cpmy_71_2_Mar_2:  +1 cpmy_72  +1 dr_71  <= 1
 c_cpmy_71_2_Mar_3:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_71  <= 1
 c_cpmy_71_2_Mar_4:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_71  <= 1
 c_cpmy_71_2_Mar_5:  +1 cpmy_81  +1 cpmy_82  +1 dr_71  <= 1
 c_cpmy_71_2_Mar_6:  +1 cpmy_83  +1 cpmy_84  +1 dr_71  <= 1
 c_cpmy_71_2_Mar_14:  +1 cpmy_85  +1 cpmy_86  +1 dr_71  <= 1
 c_cpmy_72_2_Mar_1:  +1 cpmy_72  -1 dr_72  <= 0
 c_cpmy_72_2_Mar_2:  +1 cpmy_71  +1 dr_72  <= 1
 c_cpmy_72_2_Mar_3:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_72  <= 1
 c_cpmy_72_2_Mar_4:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_72  <= 1
 c_cpmy_72_2_Mar_5:  +1 cpmy_81  +1 cpmy_82  +1 dr_72  <= 1
 c_cpmy_72_2_Mar_6:  +1 cpmy_83  +1 cpmy_84  +1 dr_72  <= 1
 c_cpmy_72_2_Mar_14:  +1 cpmy_85  +1 cpmy_86  +1 dr_72  <= 1
 c_cpmy_72_2_Mar_15:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_72  <= 1
 c_cpmy_73_2_Apr_1:  +1 cpmy_73  -1 dr_73  <= 0
 c_cpmy_73_2_Apr_2:  +1 cpmy_74  +1 cpmy_75  +1 cpmy_76  +1 dr_73  <= 1
 c_cpmy_73_2_Apr_3:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_73  <= 1
 c_cpmy_73_2_Apr_4:  +1 cpmy_81  +1 cpmy_82  +1 dr_73  <= 1
 c_cpmy_73_2_Apr_5:  +1 cpmy_83  +1 cpmy_84  +1 dr_73  <= 1
 c_cpmy_73_2_Apr_13:  +1 cpmy_85  +1 cpmy_86  +1 dr_73  <= 1
 c_cpmy_73_2_Apr_14:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_73  <= 1
 c_cpmy_74_2_Apr_1:  +1 cpmy_74  -1 dr_74  <= 0
 c_cpmy_74_2_Apr_2:  +1 cpmy_73  +1 cpmy_75  +1 cpmy_76  +1 dr_74  <= 1
 c_cpmy_74_2_Apr_3:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_74  <= 1
 c_cpmy_74_2_Apr_4:  +1 cpmy_81  +1 cpmy_82  +1 dr_74  <= 1
 c_cpmy_74_2_Apr_5:  +1 cpmy_83  +1 cpmy_84  +1 dr_74  <= 1
 c_cpmy_74_2_Apr_13:  +1 cpmy_85  +1 cpmy_86  +1 dr_74  <= 1
 c_cpmy_74_2_Apr_14:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_74  <= 1
 c_cpmy_74_2_Apr_15:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_74  <= 1
 c_cpmy_75_2_Apr_1:  +1 cpmy_75  -1 dr_75  <= 0
 c_cpmy_75_2_Apr_2:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_76  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_3:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_4:  +1 cpmy_81  +1 cpmy_82  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_5:  +1 cpmy_83  +1 cpmy_84  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_13:  +1 cpmy_85  +1 cpmy_86  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_14:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_15:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_75  <= 1
 c_cpmy_75_2_Apr_16:  +1 cpmy_95  +1 cpmy_96  +1 dr_75  <= 1
 c_cpmy_76_2_Apr_1:  +1 cpmy_76  -1 dr_76  <= 0
 c_cpmy_76_2_Apr_2:  +1 cpmy_73  +1 cpmy_74  +1 cpmy_75  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_3:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_4:  +1 cpmy_81  +1 cpmy_82  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_5:  +1 cpmy_83  +1 cpmy_84  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_13:  +1 cpmy_85  +1 cpmy_86  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_14:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_15:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_76  <= 1
 c_cpmy_76_2_Apr_16:  +1 cpmy_95  +1 cpmy_96  +1 dr_76  <= 1
 c_cpmy_77_2_May_1:  +1 cpmy_77  -1 dr_77  <= 0
 c_cpmy_77_2_May_2:  +1 cpmy_78  +1 cpmy_79  +1 cpmy_80  +1 dr_77  <= 1
 c_cpmy_77_2_May_3:  +1 cpmy_81  +1 cpmy_82  +1 dr_77  <= 1
 c_cpmy_77_2_May_4:  +1 cpmy_83  +1 cpmy_84  +1 dr_77  <= 1
 c_cpmy_77_2_May_12:  +1 cpmy_85  +1 cpmy_86  +1 dr_77  <= 1
 c_cpmy_77_2_May_13:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_77  <= 1
 c_cpmy_77_2_May_14:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_77  <= 1
 c_cpmy_78_2_May_1:  +1 cpmy_78  -1 dr_78  <= 0
 c_cpmy_78_2_May_2:  +1 cpmy_77  +1 cpmy_79  +1 cpmy_80  +1 dr_78  <= 1
 c_cpmy_78_2_May_3:  +1 cpmy_81  +1 cpmy_82  +1 dr_78  <= 1
 c_cpmy_78_2_May_4:  +1 cpmy_83  +1 cpmy_84  +1 dr_78  <= 1
 c_cpmy_78_2_May_12:  +1 cpmy_85  +1 cpmy_86  +1 dr_78  <= 1
 c_cpmy_78_2_May_13:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_78  <= 1
 c_cpmy_78_2_May_14:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_78  <= 1
 c_cpmy_78_2_May_15:  +1 cpmy_95  +1 cpmy_96  +1 dr_78  <= 1
 c_cpmy_79_2_May_1:  +1 cpmy_79  -1 dr_79  <= 0
 c_cpmy_79_2_May_2:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_80  +1 dr_79  <= 1
 c_cpmy_79_2_May_3:  +1 cpmy_81  +1 cpmy_82  +1 dr_79  <= 1
 c_cpmy_79_2_May_4:  +1 cpmy_83  +1 cpmy_84  +1 dr_79  <= 1
 c_cpmy_79_2_May_12:  +1 cpmy_85  +1 cpmy_86  +1 dr_79  <= 1
 c_cpmy_79_2_May_13:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_79  <= 1
 c_cpmy_79_2_May_14:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_79  <= 1
 c_cpmy_79_2_May_15:  +1 cpmy_95  +1 cpmy_96  +1 dr_79  <= 1
 c_cpmy_79_2_May_16:  +1 cpmy_97  +1 cpmy_98  +1 dr_79  <= 1
 c_cpmy_80_2_May_1:  +1 cpmy_80  -1 dr_80  <= 0
 c_cpmy_80_2_May_2:  +1 cpmy_77  +1 cpmy_78  +1 cpmy_79  +1 dr_80  <= 1
 c_cpmy_80_2_May_3:  +1 cpmy_81  +1 cpmy_82  +1 dr_80  <= 1
 c_cpmy_80_2_May_4:  +1 cpmy_83  +1 cpmy_84  +1 dr_80  <= 1
 c_cpmy_80_2_May_12:  +1 cpmy_85  +1 cpmy_86  +1 dr_80  <= 1
 c_cpmy_80_2_May_13:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_80  <= 1
 c_cpmy_80_2_May_14:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_80  <= 1
 c_cpmy_80_2_May_15:  +1 cpmy_95  +1 cpmy_96  +1 dr_80  <= 1
 c_cpmy_80_2_May_16:  +1 cpmy_97  +1 cpmy_98  +1 dr_80  <= 1
 c_cpmy_81_2_Jun_1:  +1 cpmy_81  -1 dr_81  <= 0
 c_cpmy_81_2_Jun_2:  +1 cpmy_82  +1 dr_81  <= 1
 c_cpmy_81_2_Jun_3:  +1 cpmy_83  +1 cpmy_84  +1 dr_81  <= 1
 c_cpmy_81_2_Jun_11:  +1 cpmy_85  +1 cpmy_86  +1 dr_81  <= 1
 c_cpmy_81_2_Jun_12:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_81  <= 1
 c_cpmy_81_2_Jun_13:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_81  <= 1
 c_cpmy_81_2_Jun_14:  +1 cpmy_95  +1 cpmy_96  +1 dr_81  <= 1
 c_cpmy_81_2_Jun_15:  +1 cpmy_97  +1 cpmy_98  +1 dr_81  <= 1
 c_cpmy_82_2_Jun_1:  +1 cpmy_82  -1 dr_82  <= 0
 c_cpmy_82_2_Jun_2:  +1 cpmy_81  +1 dr_82  <= 1
 c_cpmy_82_2_Jun_3:  +1 cpmy_83  +1 cpmy_84  +1 dr_82  <= 1
 c_cpmy_82_2_Jun_11:  +1 cpmy_85  +1 cpmy_86  +1 dr_82  <= 1
 c_cpmy_82_2_Jun_12:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_82  <= 1
 c_cpmy_82_2_Jun_13:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_82  <= 1
 c_cpmy_82_2_Jun_14:  +1 cpmy_95  +1 cpmy_96  +1 dr_82  <= 1
 c_cpmy_82_2_Jun_15:  +1 cpmy_97  +1 cpmy_98  +1 dr_82  <= 1
 c_cpmy_83_2_Jul_1:  +1 cpmy_83  -1 dr_83  <= 0
 c_cpmy_83_2_Jul_2:  +1 cpmy_84  +1 dr_83  <= 1
 c_cpmy_83_2_Jul_10:  +1 cpmy_85  +1 cpmy_86  +1 dr_83  <= 1
 c_cpmy_83_2_Jul_11:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_83  <= 1
 c_cpmy_83_2_Jul_12:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_83  <= 1
 c_cpmy_83_2_Jul_13:  +1 cpmy_95  +1 cpmy_96  +1 dr_83  <= 1
 c_cpmy_83_2_Jul_14:  +1 cpmy_97  +1 cpmy_98  +1 dr_83  <= 1
 c_cpmy_84_2_Jul_1:  +1 cpmy_84  -1 dr_84  <= 0
 c_cpmy_84_2_Jul_2:  +1 cpmy_83  +1 dr_84  <= 1
 c_cpmy_84_2_Jul_10:  +1 cpmy_85  +1 cpmy_86  +1 dr_84  <= 1
 c_cpmy_84_2_Jul_11:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_84  <= 1
 c_cpmy_84_2_Jul_12:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_84  <= 1
 c_cpmy_84_2_Jul_13:  +1 cpmy_95  +1 cpmy_96  +1 dr_84  <= 1
 c_cpmy_84_2_Jul_14:  +1 cpmy_97  +1 cpmy_98  +1 dr_84  <= 1
 c_cpmy_85_3_Mar_1:  +1 cpmy_85  -1 dr_85  <= 0
 c_cpmy_85_3_Mar_2:  +1 cpmy_86  +1 dr_85  <= 1
 c_cpmy_85_3_Mar_3:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_85  <= 1
 c_cpmy_85_3_Mar_4:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_85  <= 1
 c_cpmy_85_3_Mar_5:  +1 cpmy_95  +1 cpmy_96  +1 dr_85  <= 1
 c_cpmy_85_3_Mar_6:  +1 cpmy_97  +1 cpmy_98  +1 dr_85  <= 1
 c_cpmy_85_3_Mar_14:  +1 cpmy_99  +1 cpmy_100  +1 dr_85  <= 1
 c_cpmy_86_3_Mar_1:  +1 cpmy_86  -1 dr_86  <= 0
 c_cpmy_86_3_Mar_2:  +1 cpmy_85  +1 dr_86  <= 1
 c_cpmy_86_3_Mar_3:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_86  <= 1
 c_cpmy_86_3_Mar_4:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_86  <= 1
 c_cpmy_86_3_Mar_5:  +1 cpmy_95  +1 cpmy_96  +1 dr_86  <= 1
 c_cpmy_86_3_Mar_6:  +1 cpmy_97  +1 cpmy_98  +1 dr_86  <= 1
 c_cpmy_86_3_Mar_14:  +1 cpmy_99  +1 cpmy_100  +1 dr_86  <= 1
 c_cpmy_86_3_Mar_15:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_86  <= 1
 c_cpmy_87_3_Apr_1:  +1 cpmy_87  -1 dr_87  <= 0
 c_cpmy_87_3_Apr_2:  +1 cpmy_88  +1 cpmy_89  +1 cpmy_90  +1 dr_87  <= 1
 c_cpmy_87_3_Apr_3:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_87  <= 1
 c_cpmy_87_3_Apr_4:  +1 cpmy_95  +1 cpmy_96  +1 dr_87  <= 1
 c_cpmy_87_3_Apr_5:  +1 cpmy_97  +1 cpmy_98  +1 dr_87  <= 1
 c_cpmy_87_3_Apr_13:  +1 cpmy_99  +1 cpmy_100  +1 dr_87  <= 1
 c_cpmy_87_3_Apr_14:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_87  <= 1
 c_cpmy_88_3_Apr_1:  +1 cpmy_88  -1 dr_88  <= 0
 c_cpmy_88_3_Apr_2:  +1 cpmy_87  +1 cpmy_89  +1 cpmy_90  +1 dr_88  <= 1
 c_cpmy_88_3_Apr_3:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_88  <= 1
 c_cpmy_88_3_Apr_4:  +1 cpmy_95  +1 cpmy_96  +1 dr_88  <= 1
 c_cpmy_88_3_Apr_5:  +1 cpmy_97  +1 cpmy_98  +1 dr_88  <= 1
 c_cpmy_88_3_Apr_13:  +1 cpmy_99  +1 cpmy_100  +1 dr_88  <= 1
 c_cpmy_88_3_Apr_14:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_88  <= 1
 c_cpmy_88_3_Apr_15:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_88  <= 1
 c_cpmy_89_3_Apr_1:  +1 cpmy_89  -1 dr_89  <= 0
 c_cpmy_89_3_Apr_2:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_90  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_3:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_4:  +1 cpmy_95  +1 cpmy_96  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_5:  +1 cpmy_97  +1 cpmy_98  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_13:  +1 cpmy_99  +1 cpmy_100  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_14:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_15:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_89  <= 1
 c_cpmy_89_3_Apr_16:  +1 cpmy_109  +1 cpmy_110  +1 dr_89  <= 1
 c_cpmy_90_3_Apr_1:  +1 cpmy_90  -1 dr_90  <= 0
 c_cpmy_90_3_Apr_2:  +1 cpmy_87  +1 cpmy_88  +1 cpmy_89  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_3:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_4:  +1 cpmy_95  +1 cpmy_96  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_5:  +1 cpmy_97  +1 cpmy_98  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_13:  +1 cpmy_99  +1 cpmy_100  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_14:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_15:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_90  <= 1
 c_cpmy_90_3_Apr_16:  +1 cpmy_109  +1 cpmy_110  +1 dr_90  <= 1
 c_cpmy_91_3_May_1:  +1 cpmy_91  -1 dr_91  <= 0
 c_cpmy_91_3_May_2:  +1 cpmy_92  +1 cpmy_93  +1 cpmy_94  +1 dr_91  <= 1
 c_cpmy_91_3_May_3:  +1 cpmy_95  +1 cpmy_96  +1 dr_91  <= 1
 c_cpmy_91_3_May_4:  +1 cpmy_97  +1 cpmy_98  +1 dr_91  <= 1
 c_cpmy_91_3_May_12:  +1 cpmy_99  +1 cpmy_100  +1 dr_91  <= 1
 c_cpmy_91_3_May_13:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_91  <= 1
 c_cpmy_91_3_May_14:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_91  <= 1
 c_cpmy_92_3_May_1:  +1 cpmy_92  -1 dr_92  <= 0
 c_cpmy_92_3_May_2:  +1 cpmy_91  +1 cpmy_93  +1 cpmy_94  +1 dr_92  <= 1
 c_cpmy_92_3_May_3:  +1 cpmy_95  +1 cpmy_96  +1 dr_92  <= 1
 c_cpmy_92_3_May_4:  +1 cpmy_97  +1 cpmy_98  +1 dr_92  <= 1
 c_cpmy_92_3_May_12:  +1 cpmy_99  +1 cpmy_100  +1 dr_92  <= 1
 c_cpmy_92_3_May_13:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_92  <= 1
 c_cpmy_92_3_May_14:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_92  <= 1
 c_cpmy_92_3_May_15:  +1 cpmy_109  +1 cpmy_110  +1 dr_92  <= 1
 c_cpmy_93_3_May_1:  +1 cpmy_93  -1 dr_93  <= 0
 c_cpmy_93_3_May_2:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_94  +1 dr_93  <= 1
 c_cpmy_93_3_May_3:  +1 cpmy_95  +1 cpmy_96  +1 dr_93  <= 1
 c_cpmy_93_3_May_4:  +1 cpmy_97  +1 cpmy_98  +1 dr_93  <= 1
 c_cpmy_93_3_May_12:  +1 cpmy_99  +1 cpmy_100  +1 dr_93  <= 1
 c_cpmy_93_3_May_13:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_93  <= 1
 c_cpmy_93_3_May_14:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_93  <= 1
 c_cpmy_93_3_May_15:  +1 cpmy_109  +1 cpmy_110  +1 dr_93  <= 1
 c_cpmy_93_3_May_16:  +1 cpmy_111  +1 cpmy_112  +1 dr_93  <= 1
 c_cpmy_94_3_May_1:  +1 cpmy_94  -1 dr_94  <= 0
 c_cpmy_94_3_May_2:  +1 cpmy_91  +1 cpmy_92  +1 cpmy_93  +1 dr_94  <= 1
 c_cpmy_94_3_May_3:  +1 cpmy_95  +1 cpmy_96  +1 dr_94  <= 1
 c_cpmy_94_3_May_4:  +1 cpmy_97  +1 cpmy_98  +1 dr_94  <= 1
 c_cpmy_94_3_May_12:  +1 cpmy_99  +1 cpmy_100  +1 dr_94  <= 1
 c_cpmy_94_3_May_13:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_94  <= 1
 c_cpmy_94_3_May_14:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_94  <= 1
 c_cpmy_94_3_May_15:  +1 cpmy_109  +1 cpmy_110  +1 dr_94  <= 1
 c_cpmy_94_3_May_16:  +1 cpmy_111  +1 cpmy_112  +1 dr_94  <= 1
 c_cpmy_95_3_Jun_1:  +1 cpmy_95  -1 dr_95  <= 0
 c_cpmy_95_3_Jun_2:  +1 cpmy_96  +1 dr_95  <= 1
 c_cpmy_95_3_Jun_3:  +1 cpmy_97  +1 cpmy_98  +1 dr_95  <= 1
 c_cpmy_95_3_Jun_11:  +1 cpmy_99  +1 cpmy_100  +1 dr_95  <= 1
 c_cpmy_95_3_Jun_12:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_95  <= 1
 c_cpmy_95_3_Jun_13:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_95  <= 1
 c_cpmy_95_3_Jun_14:  +1 cpmy_109  +1 cpmy_110  +1 dr_95  <= 1
 c_cpmy_95_3_Jun_15:  +1 cpmy_111  +1 cpmy_112  +1 dr_95  <= 1
 c_cpmy_96_3_Jun_1:  +1 cpmy_96  -1 dr_96  <= 0
 c_cpmy_96_3_Jun_2:  +1 cpmy_95  +1 dr_96  <= 1
 c_cpmy_96_3_Jun_3:  +1 cpmy_97  +1 cpmy_98  +1 dr_96  <= 1
 c_cpmy_96_3_Jun_11:  +1 cpmy_99  +1 cpmy_100  +1 dr_96  <= 1
 c_cpmy_96_3_Jun_12:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_96  <= 1
 c_cpmy_96_3_Jun_13:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_96  <= 1
 c_cpmy_96_3_Jun_14:  +1 cpmy_109  +1 cpmy_110  +1 dr_96  <= 1
 c_cpmy_96_3_Jun_15:  +1 cpmy_111  +1 cpmy_112  +1 dr_96  <= 1
 c_cpmy_97_3_Jul_1:  +1 cpmy_97  -1 dr_97  <= 0
 c_cpmy_97_3_Jul_2:  +1 cpmy_98  +1 dr_97  <= 1
 c_cpmy_97_3_Jul_10:  +1 cpmy_99  +1 cpmy_100  +1 dr_97  <= 1
 c_cpmy_97_3_Jul_11:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_97  <= 1
 c_cpmy_97_3_Jul_12:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_97  <= 1
 c_cpmy_97_3_Jul_13:  +1 cpmy_109  +1 cpmy_110  +1 dr_97  <= 1
 c_cpmy_97_3_Jul_14:  +1 cpmy_111  +1 cpmy_112  +1 dr_97  <= 1
 c_cpmy_98_3_Jul_1:  +1 cpmy_98  -1 dr_98  <= 0
 c_cpmy_98_3_Jul_2:  +1 cpmy_97  +1 dr_98  <= 1
 c_cpmy_98_3_Jul_10:  +1 cpmy_99  +1 cpmy_100  +1 dr_98  <= 1
 c_cpmy_98_3_Jul_11:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_98  <= 1
 c_cpmy_98_3_Jul_12:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_98  <= 1
 c_cpmy_98_3_Jul_13:  +1 cpmy_109  +1 cpmy_110  +1 dr_98  <= 1
 c_cpmy_98_3_Jul_14:  +1 cpmy_111  +1 cpmy_112  +1 dr_98  <= 1
 c_cpmy_99_4_Mar_1:  +1 cpmy_99  -1 dr_99  <= 0
 c_cpmy_99_4_Mar_2:  +1 cpmy_100  +1 dr_99  <= 1
 c_cpmy_99_4_Mar_3:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_99  <= 1
 c_cpmy_99_4_Mar_4:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_99  <= 1
 c_cpmy_99_4_Mar_5:  +1 cpmy_109  +1 cpmy_110  +1 dr_99  <= 1
 c_cpmy_99_4_Mar_6:  +1 cpmy_111  +1 cpmy_112  +1 dr_99  <= 1
 c_cpmy_100_4_Mar_1:  +1 cpmy_100  -1 dr_100  <= 0
 c_cpmy_100_4_Mar_2:  +1 cpmy_99  +1 dr_100  <= 1
 c_cpmy_100_4_Mar_3:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_100  <= 1
 c_cpmy_100_4_Mar_4:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_100  <= 1
 c_cpmy_100_4_Mar_5:  +1 cpmy_109  +1 cpmy_110  +1 dr_100  <= 1
 c_cpmy_100_4_Mar_6:  +1 cpmy_111  +1 cpmy_112  +1 dr_100  <= 1
 c_cpmy_101_4_Apr_1:  +1 cpmy_101  -1 dr_101  <= 0
 c_cpmy_101_4_Apr_2:  +1 cpmy_102  +1 cpmy_103  +1 cpmy_104  +1 dr_101  <= 1
 c_cpmy_101_4_Apr_3:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_101  <= 1
 c_cpmy_101_4_Apr_4:  +1 cpmy_109  +1 cpmy_110  +1 dr_101  <= 1
 c_cpmy_101_4_Apr_5:  +1 cpmy_111  +1 cpmy_112  +1 dr_101  <= 1
 c_cpmy_102_4_Apr_1:  +1 cpmy_102  -1 dr_102  <= 0
 c_cpmy_102_4_Apr_2:  +1 cpmy_101  +1 cpmy_103  +1 cpmy_104  +1 dr_102  <= 1
 c_cpmy_102_4_Apr_3:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_102  <= 1
 c_cpmy_102_4_Apr_4:  +1 cpmy_109  +1 cpmy_110  +1 dr_102  <= 1
 c_cpmy_102_4_Apr_5:  +1 cpmy_111  +1 cpmy_112  +1 dr_102  <= 1
 c_cpmy_103_4_Apr_1:  +1 cpmy_103  -1 dr_103  <= 0
 c_cpmy_103_4_Apr_2:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_104  +1 dr_103  <= 1
 c_cpmy_103_4_Apr_3:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_103  <= 1
 c_cpmy_103_4_Apr_4:  +1 cpmy_109  +1 cpmy_110  +1 dr_103  <= 1
 c_cpmy_103_4_Apr_5:  +1 cpmy_111  +1 cpmy_112  +1 dr_103  <= 1
 c_cpmy_104_4_Apr_1:  +1 cpmy_104  -1 dr_104  <= 0
 c_cpmy_104_4_Apr_2:  +1 cpmy_101  +1 cpmy_102  +1 cpmy_103  +1 dr_104  <= 1
 c_cpmy_104_4_Apr_3:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_104  <= 1
 c_cpmy_104_4_Apr_4:  +1 cpmy_109  +1 cpmy_110  +1 dr_104  <= 1
 c_cpmy_104_4_Apr_5:  +1 cpmy_111  +1 cpmy_112  +1 dr_104  <= 1
 c_cpmy_105_4_May_1:  +1 cpmy_105  -1 dr_105  <= 0
 c_cpmy_105_4_May_2:  +1 cpmy_106  +1 cpmy_107  +1 cpmy_108  +1 dr_105  <= 1
 c_cpmy_105_4_May_3:  +1 cpmy_109  +1 cpmy_110  +1 dr_105  <= 1
 c_cpmy_105_4_May_4:  +1 cpmy_111  +1 cpmy_112  +1 dr_105  <= 1
 c_cpmy_106_4_May_1:  +1 cpmy_106  -1 dr_106  <= 0
 c_cpmy_106_4_May_2:  +1 cpmy_105  +1 cpmy_107  +1 cpmy_108  +1 dr_106  <= 1
 c_cpmy_106_4_May_3:  +1 cpmy_109  +1 cpmy_110  +1 dr_106  <= 1
 c_cpmy_106_4_May_4:  +1 cpmy_111  +1 cpmy_112  +1 dr_106  <= 1
 c_cpmy_107_4_May_1:  +1 cpmy_107  -1 dr_107  <= 0
 c_cpmy_107_4_May_2:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_108  +1 dr_107  <= 1
 c_cpmy_107_4_May_3:  +1 cpmy_109  +1 cpmy_110  +1 dr_107  <= 1
 c_cpmy_107_4_May_4:  +1 cpmy_111  +1 cpmy_112  +1 dr_107  <= 1
 c_cpmy_108_4_May_1:  +1 cpmy_108  -1 dr_108  <= 0
 c_cpmy_108_4_May_2:  +1 cpmy_105  +1 cpmy_106  +1 cpmy_107  +1 dr_108  <= 1
 c_cpmy_108_4_May_3:  +1 cpmy_109  +1 cpmy_110  +1 dr_108  <= 1
 c_cpmy_108_4_May_4:  +1 cpmy_111  +1 cpmy_112  +1 dr_108  <= 1
 c_cpmy_109_4_Jun_1:  +1 cpmy_109  -1 dr_109  <= 0
 c_cpmy_109_4_Jun_2:  +1 cpmy_110  +1 dr_109  <= 1
 c_cpmy_109_4_Jun_3:  +1 cpmy_111  +1 cpmy_112  +1 dr_109  <= 1
 c_cpmy_110_4_Jun_1:  +1 cpmy_110  -1 dr_110  <= 0
 c_cpmy_110_4_Jun_2:  +1 cpmy_109  +1 dr_110  <= 1
 c_cpmy_110_4_Jun_3:  +1 cpmy_111  +1 cpmy_112  +1 dr_110  <= 1
 c_cpmy_111_4_Jul_1:  +1 cpmy_111  -1 dr_111  <= 0
 c_cpmy_111_4_Jul_2:  +1 cpmy_112  +1 dr_111  <= 1
 c_cpmy_112_4_Jul_1:  +1 cpmy_112  -1 dr_112  <= 0
 c_cpmy_112_4_Jul_2:  +1 cpmy_111  +1 dr_112  <= 1
 c_cpmy_113_1_Mar_1:  +1 cpmy_113  -1 dr_113  <= 0
 c_cpmy_113_1_Mar_2:  +1 cpmy_114  +1 dr_113  <= 1
 c_cpmy_113_1_Mar_3:  +1 cpmy_115  +1 cpmy_116  +1 cpmy_117  +1 cpmy_118  +1 dr_113  <= 1
 c_cpmy_113_1_Mar_4:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_113  <= 1
 c_cpmy_113_1_Mar_5:  +1 cpmy_123  +1 cpmy_124  +1 dr_113  <= 1
 c_cpmy_113_1_Mar_6:  +1 cpmy_125  +1 cpmy_126  +1 dr_113  <= 1
 c_cpmy_113_1_Mar_14:  +1 cpmy_127  +1 cpmy_128  +1 dr_113  <= 1
 c_cpmy_114_1_Mar_1:  +1 cpmy_114  -1 dr_114  <= 0
 c_cpmy_114_1_Mar_2:  +1 cpmy_113  +1 dr_114  <= 1
 c_cpmy_114_1_Mar_3:  +1 cpmy_115  +1 cpmy_116  +1 cpmy_117  +1 cpmy_118  +1 dr_114  <= 1
 c_cpmy_114_1_Mar_4:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_114  <= 1
 c_cpmy_114_1_Mar_5:  +1 cpmy_123  +1 cpmy_124  +1 dr_114  <= 1
 c_cpmy_114_1_Mar_6:  +1 cpmy_125  +1 cpmy_126  +1 dr_114  <= 1
 c_cpmy_114_1_Mar_14:  +1 cpmy_127  +1 cpmy_128  +1 dr_114  <= 1
 c_cpmy_114_1_Mar_15:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_114  <= 1
 c_cpmy_115_1_Apr_1:  +1 cpmy_115  -1 dr_115  <= 0
 c_cpmy_115_1_Apr_2:  +1 cpmy_116  +1 cpmy_117  +1 cpmy_118  +1 dr_115  <= 1
 c_cpmy_115_1_Apr_3:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_115  <= 1
 c_cpmy_115_1_Apr_4:  +1 cpmy_123  +1 cpmy_124  +1 dr_115  <= 1
 c_cpmy_115_1_Apr_5:  +1 cpmy_125  +1 cpmy_126  +1 dr_115  <= 1
 c_cpmy_115_1_Apr_13:  +1 cpmy_127  +1 cpmy_128  +1 dr_115  <= 1
 c_cpmy_115_1_Apr_14:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_115  <= 1
 c_cpmy_116_1_Apr_1:  +1 cpmy_116  -1 dr_116  <= 0
 c_cpmy_116_1_Apr_2:  +1 cpmy_115  +1 cpmy_117  +1 cpmy_118  +1 dr_116  <= 1
 c_cpmy_116_1_Apr_3:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_116  <= 1
 c_cpmy_116_1_Apr_4:  +1 cpmy_123  +1 cpmy_124  +1 dr_116  <= 1
 c_cpmy_116_1_Apr_5:  +1 cpmy_125  +1 cpmy_126  +1 dr_116  <= 1
 c_cpmy_116_1_Apr_13:  +1 cpmy_127  +1 cpmy_128  +1 dr_116  <= 1
 c_cpmy_116_1_Apr_14:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_116  <= 1
 c_cpmy_116_1_Apr_15:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_116  <= 1
 c_cpmy_117_1_Apr_1:  +1 cpmy_117  -1 dr_117  <= 0
 c_cpmy_117_1_Apr_2:  +1 cpmy_115  +1 cpmy_116  +1 cpmy_118  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_3:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_4:  +1 cpmy_123  +1 cpmy_124  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_5:  +1 cpmy_125  +1 cpmy_126  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_13:  +1 cpmy_127  +1 cpmy_128  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_14:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_15:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_117  <= 1
 c_cpmy_117_1_Apr_16:  +1 cpmy_137  +1 cpmy_138  +1 dr_117  <= 1
 c_cpmy_118_1_Apr_1:  +1 cpmy_118  -1 dr_118  <= 0
 c_cpmy_118_1_Apr_2:  +1 cpmy_115  +1 cpmy_116  +1 cpmy_117  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_3:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_4:  +1 cpmy_123  +1 cpmy_124  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_5:  +1 cpmy_125  +1 cpmy_126  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_13:  +1 cpmy_127  +1 cpmy_128  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_14:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_15:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_118  <= 1
 c_cpmy_118_1_Apr_16:  +1 cpmy_137  +1 cpmy_138  +1 dr_118  <= 1
 c_cpmy_119_1_May_1:  +1 cpmy_119  -1 dr_119  <= 0
 c_cpmy_119_1_May_2:  +1 cpmy_120  +1 cpmy_121  +1 cpmy_122  +1 dr_119  <= 1
 c_cpmy_119_1_May_3:  +1 cpmy_123  +1 cpmy_124  +1 dr_119  <= 1
 c_cpmy_119_1_May_4:  +1 cpmy_125  +1 cpmy_126  +1 dr_119  <= 1
 c_cpmy_119_1_May_12:  +1 cpmy_127  +1 cpmy_128  +1 dr_119  <= 1
 c_cpmy_119_1_May_13:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_119  <= 1
 c_cpmy_119_1_May_14:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_119  <= 1
 c_cpmy_120_1_May_1:  +1 cpmy_120  -1 dr_120  <= 0
 c_cpmy_120_1_May_2:  +1 cpmy_119  +1 cpmy_121  +1 cpmy_122  +1 dr_120  <= 1
 c_cpmy_120_1_May_3:  +1 cpmy_123  +1 cpmy_124  +1 dr_120  <= 1
 c_cpmy_120_1_May_4:  +1 cpmy_125  +1 cpmy_126  +1 dr_120  <= 1
 c_cpmy_120_1_May_12:  +1 cpmy_127  +1 cpmy_128  +1 dr_120  <= 1
 c_cpmy_120_1_May_13:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_120  <= 1
 c_cpmy_120_1_May_14:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_120  <= 1
 c_cpmy_120_1_May_15:  +1 cpmy_137  +1 cpmy_138  +1 dr_120  <= 1
 c_cpmy_121_1_May_1:  +1 cpmy_121  -1 dr_121  <= 0
 c_cpmy_121_1_May_2:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_122  +1 dr_121  <= 1
 c_cpmy_121_1_May_3:  +1 cpmy_123  +1 cpmy_124  +1 dr_121  <= 1
 c_cpmy_121_1_May_4:  +1 cpmy_125  +1 cpmy_126  +1 dr_121  <= 1
 c_cpmy_121_1_May_12:  +1 cpmy_127  +1 cpmy_128  +1 dr_121  <= 1
 c_cpmy_121_1_May_13:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_121  <= 1
 c_cpmy_121_1_May_14:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_121  <= 1
 c_cpmy_121_1_May_15:  +1 cpmy_137  +1 cpmy_138  +1 dr_121  <= 1
 c_cpmy_121_1_May_16:  +1 cpmy_139  +1 cpmy_140  +1 dr_121  <= 1
 c_cpmy_122_1_May_1:  +1 cpmy_122  -1 dr_122  <= 0
 c_cpmy_122_1_May_2:  +1 cpmy_119  +1 cpmy_120  +1 cpmy_121  +1 dr_122  <= 1
 c_cpmy_122_1_May_3:  +1 cpmy_123  +1 cpmy_124  +1 dr_122  <= 1
 c_cpmy_122_1_May_4:  +1 cpmy_125  +1 cpmy_126  +1 dr_122  <= 1
 c_cpmy_122_1_May_12:  +1 cpmy_127  +1 cpmy_128  +1 dr_122  <= 1
 c_cpmy_122_1_May_13:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_122  <= 1
 c_cpmy_122_1_May_14:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_122  <= 1
 c_cpmy_122_1_May_15:  +1 cpmy_137  +1 cpmy_138  +1 dr_122  <= 1
 c_cpmy_122_1_May_16:  +1 cpmy_139  +1 cpmy_140  +1 dr_122  <= 1
 c_cpmy_123_1_Jun_1:  +1 cpmy_123  -1 dr_123  <= 0
 c_cpmy_123_1_Jun_2:  +1 cpmy_124  +1 dr_123  <= 1
 c_cpmy_123_1_Jun_3:  +1 cpmy_125  +1 cpmy_126  +1 dr_123  <= 1
 c_cpmy_123_1_Jun_11:  +1 cpmy_127  +1 cpmy_128  +1 dr_123  <= 1
 c_cpmy_123_1_Jun_12:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_123  <= 1
 c_cpmy_123_1_Jun_13:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_123  <= 1
 c_cpmy_123_1_Jun_14:  +1 cpmy_137  +1 cpmy_138  +1 dr_123  <= 1
 c_cpmy_123_1_Jun_15:  +1 cpmy_139  +1 cpmy_140  +1 dr_123  <= 1
 c_cpmy_124_1_Jun_1:  +1 cpmy_124  -1 dr_124  <= 0
 c_cpmy_124_1_Jun_2:  +1 cpmy_123  +1 dr_124  <= 1
 c_cpmy_124_1_Jun_3:  +1 cpmy_125  +1 cpmy_126  +1 dr_124  <= 1
 c_cpmy_124_1_Jun_11:  +1 cpmy_127  +1 cpmy_128  +1 dr_124  <= 1
 c_cpmy_124_1_Jun_12:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_124  <= 1
 c_cpmy_124_1_Jun_13:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_124  <= 1
 c_cpmy_124_1_Jun_14:  +1 cpmy_137  +1 cpmy_138  +1 dr_124  <= 1
 c_cpmy_124_1_Jun_15:  +1 cpmy_139  +1 cpmy_140  +1 dr_124  <= 1
 c_cpmy_125_1_Jul_1:  +1 cpmy_125  -1 dr_125  <= 0
 c_cpmy_125_1_Jul_2:  +1 cpmy_126  +1 dr_125  <= 1
 c_cpmy_125_1_Jul_10:  +1 cpmy_127  +1 cpmy_128  +1 dr_125  <= 1
 c_cpmy_125_1_Jul_11:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_125  <= 1
 c_cpmy_125_1_Jul_12:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_125  <= 1
 c_cpmy_125_1_Jul_13:  +1 cpmy_137  +1 cpmy_138  +1 dr_125  <= 1
 c_cpmy_125_1_Jul_14:  +1 cpmy_139  +1 cpmy_140  +1 dr_125  <= 1
 c_cpmy_126_1_Jul_1:  +1 cpmy_126  -1 dr_126  <= 0
 c_cpmy_126_1_Jul_2:  +1 cpmy_125  +1 dr_126  <= 1
 c_cpmy_126_1_Jul_10:  +1 cpmy_127  +1 cpmy_128  +1 dr_126  <= 1
 c_cpmy_126_1_Jul_11:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_126  <= 1
 c_cpmy_126_1_Jul_12:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_126  <= 1
 c_cpmy_126_1_Jul_13:  +1 cpmy_137  +1 cpmy_138  +1 dr_126  <= 1
 c_cpmy_126_1_Jul_14:  +1 cpmy_139  +1 cpmy_140  +1 dr_126  <= 1
 c_cpmy_127_2_Mar_1:  +1 cpmy_127  -1 dr_127  <= 0
 c_cpmy_127_2_Mar_2:  +1 cpmy_128  +1 dr_127  <= 1
 c_cpmy_127_2_Mar_3:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_127  <= 1
 c_cpmy_127_2_Mar_4:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_127  <= 1
 c_cpmy_127_2_Mar_5:  +1 cpmy_137  +1 cpmy_138  +1 dr_127  <= 1
 c_cpmy_127_2_Mar_6:  +1 cpmy_139  +1 cpmy_140  +1 dr_127  <= 1
 c_cpmy_127_2_Mar_14:  +1 cpmy_141  +1 cpmy_142  +1 dr_127  <= 1
 c_cpmy_128_2_Mar_1:  +1 cpmy_128  -1 dr_128  <= 0
 c_cpmy_128_2_Mar_2:  +1 cpmy_127  +1 dr_128  <= 1
 c_cpmy_128_2_Mar_3:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_128  <= 1
 c_cpmy_128_2_Mar_4:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_128  <= 1
 c_cpmy_128_2_Mar_5:  +1 cpmy_137  +1 cpmy_138  +1 dr_128  <= 1
 c_cpmy_128_2_Mar_6:  +1 cpmy_139  +1 cpmy_140  +1 dr_128  <= 1
 c_cpmy_128_2_Mar_14:  +1 cpmy_141  +1 cpmy_142  +1 dr_128  <= 1
 c_cpmy_128_2_Mar_15:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_128  <= 1
 c_cpmy_129_2_Apr_1:  +1 cpmy_129  -1 dr_129  <= 0
 c_cpmy_129_2_Apr_2:  +1 cpmy_130  +1 cpmy_131  +1 cpmy_132  +1 dr_129  <= 1
 c_cpmy_129_2_Apr_3:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_129  <= 1
 c_cpmy_129_2_Apr_4:  +1 cpmy_137  +1 cpmy_138  +1 dr_129  <= 1
 c_cpmy_129_2_Apr_5:  +1 cpmy_139  +1 cpmy_140  +1 dr_129  <= 1
 c_cpmy_129_2_Apr_13:  +1 cpmy_141  +1 cpmy_142  +1 dr_129  <= 1
 c_cpmy_129_2_Apr_14:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_129  <= 1
 c_cpmy_130_2_Apr_1:  +1 cpmy_130  -1 dr_130  <= 0
 c_cpmy_130_2_Apr_2:  +1 cpmy_129  +1 cpmy_131  +1 cpmy_132  +1 dr_130  <= 1
 c_cpmy_130_2_Apr_3:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_130  <= 1
 c_cpmy_130_2_Apr_4:  +1 cpmy_137  +1 cpmy_138  +1 dr_130  <= 1
 c_cpmy_130_2_Apr_5:  +1 cpmy_139  +1 cpmy_140  +1 dr_130  <= 1
 c_cpmy_130_2_Apr_13:  +1 cpmy_141  +1 cpmy_142  +1 dr_130  <= 1
 c_cpmy_130_2_Apr_14:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_130  <= 1
 c_cpmy_130_2_Apr_15:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_130  <= 1
 c_cpmy_131_2_Apr_1:  +1 cpmy_131  -1 dr_131  <= 0
 c_cpmy_131_2_Apr_2:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_132  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_3:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_4:  +1 cpmy_137  +1 cpmy_138  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_5:  +1 cpmy_139  +1 cpmy_140  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_13:  +1 cpmy_141  +1 cpmy_142  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_14:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_15:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_131  <= 1
 c_cpmy_131_2_Apr_16:  +1 cpmy_151  +1 cpmy_152  +1 dr_131  <= 1
 c_cpmy_132_2_Apr_1:  +1 cpmy_132  -1 dr_132  <= 0
 c_cpmy_132_2_Apr_2:  +1 cpmy_129  +1 cpmy_130  +1 cpmy_131  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_3:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_4:  +1 cpmy_137  +1 cpmy_138  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_5:  +1 cpmy_139  +1 cpmy_140  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_13:  +1 cpmy_141  +1 cpmy_142  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_14:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_15:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_132  <= 1
 c_cpmy_132_2_Apr_16:  +1 cpmy_151  +1 cpmy_152  +1 dr_132  <= 1
 c_cpmy_133_2_May_1:  +1 cpmy_133  -1 dr_133  <= 0
 c_cpmy_133_2_May_2:  +1 cpmy_134  +1 cpmy_135  +1 cpmy_136  +1 dr_133  <= 1
 c_cpmy_133_2_May_3:  +1 cpmy_137  +1 cpmy_138  +1 dr_133  <= 1
 c_cpmy_133_2_May_4:  +1 cpmy_139  +1 cpmy_140  +1 dr_133  <= 1
 c_cpmy_133_2_May_12:  +1 cpmy_141  +1 cpmy_142  +1 dr_133  <= 1
 c_cpmy_133_2_May_13:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_133  <= 1
 c_cpmy_133_2_May_14:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_133  <= 1
 c_cpmy_134_2_May_1:  +1 cpmy_134  -1 dr_134  <= 0
 c_cpmy_134_2_May_2:  +1 cpmy_133  +1 cpmy_135  +1 cpmy_136  +1 dr_134  <= 1
 c_cpmy_134_2_May_3:  +1 cpmy_137  +1 cpmy_138  +1 dr_134  <= 1
 c_cpmy_134_2_May_4:  +1 cpmy_139  +1 cpmy_140  +1 dr_134  <= 1
 c_cpmy_134_2_May_12:  +1 cpmy_141  +1 cpmy_142  +1 dr_134  <= 1
 c_cpmy_134_2_May_13:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_134  <= 1
 c_cpmy_134_2_May_14:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_134  <= 1
 c_cpmy_134_2_May_15:  +1 cpmy_151  +1 cpmy_152  +1 dr_134  <= 1
 c_cpmy_135_2_May_1:  +1 cpmy_135  -1 dr_135  <= 0
 c_cpmy_135_2_May_2:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_136  +1 dr_135  <= 1
 c_cpmy_135_2_May_3:  +1 cpmy_137  +1 cpmy_138  +1 dr_135  <= 1
 c_cpmy_135_2_May_4:  +1 cpmy_139  +1 cpmy_140  +1 dr_135  <= 1
 c_cpmy_135_2_May_12:  +1 cpmy_141  +1 cpmy_142  +1 dr_135  <= 1
 c_cpmy_135_2_May_13:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_135  <= 1
 c_cpmy_135_2_May_14:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_135  <= 1
 c_cpmy_135_2_May_15:  +1 cpmy_151  +1 cpmy_152  +1 dr_135  <= 1
 c_cpmy_135_2_May_16:  +1 cpmy_153  +1 cpmy_154  +1 dr_135  <= 1
 c_cpmy_136_2_May_1:  +1 cpmy_136  -1 dr_136  <= 0
 c_cpmy_136_2_May_2:  +1 cpmy_133  +1 cpmy_134  +1 cpmy_135  +1 dr_136  <= 1
 c_cpmy_136_2_May_3:  +1 cpmy_137  +1 cpmy_138  +1 dr_136  <= 1
 c_cpmy_136_2_May_4:  +1 cpmy_139  +1 cpmy_140  +1 dr_136  <= 1
 c_cpmy_136_2_May_12:  +1 cpmy_141  +1 cpmy_142  +1 dr_136  <= 1
 c_cpmy_136_2_May_13:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_136  <= 1
 c_cpmy_136_2_May_14:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_136  <= 1
 c_cpmy_136_2_May_15:  +1 cpmy_151  +1 cpmy_152  +1 dr_136  <= 1
 c_cpmy_136_2_May_16:  +1 cpmy_153  +1 cpmy_154  +1 dr_136  <= 1
 c_cpmy_137_2_Jun_1:  +1 cpmy_137  -1 dr_137  <= 0
 c_cpmy_137_2_Jun_2:  +1 cpmy_138  +1 dr_137  <= 1
 c_cpmy_137_2_Jun_3:  +1 cpmy_139  +1 cpmy_140  +1 dr_137  <= 1
 c_cpmy_137_2_Jun_11:  +1 cpmy_141  +1 cpmy_142  +1 dr_137  <= 1
 c_cpmy_137_2_Jun_12:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_137  <= 1
 c_cpmy_137_2_Jun_13:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_137  <= 1
 c_cpmy_137_2_Jun_14:  +1 cpmy_151  +1 cpmy_152  +1 dr_137  <= 1
 c_cpmy_137_2_Jun_15:  +1 cpmy_153  +1 cpmy_154  +1 dr_137  <= 1
 c_cpmy_138_2_Jun_1:  +1 cpmy_138  -1 dr_138  <= 0
 c_cpmy_138_2_Jun_2:  +1 cpmy_137  +1 dr_138  <= 1
 c_cpmy_138_2_Jun_3:  +1 cpmy_139  +1 cpmy_140  +1 dr_138  <= 1
 c_cpmy_138_2_Jun_11:  +1 cpmy_141  +1 cpmy_142  +1 dr_138  <= 1
 c_cpmy_138_2_Jun_12:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_138  <= 1
 c_cpmy_138_2_Jun_13:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_138  <= 1
 c_cpmy_138_2_Jun_14:  +1 cpmy_151  +1 cpmy_152  +1 dr_138  <= 1
 c_cpmy_138_2_Jun_15:  +1 cpmy_153  +1 cpmy_154  +1 dr_138  <= 1
 c_cpmy_139_2_Jul_1:  +1 cpmy_139  -1 dr_139  <= 0
 c_cpmy_139_2_Jul_2:  +1 cpmy_140  +1 dr_139  <= 1
 c_cpmy_139_2_Jul_10:  +1 cpmy_141  +1 cpmy_142  +1 dr_139  <= 1
 c_cpmy_139_2_Jul_11:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_139  <= 1
 c_cpmy_139_2_Jul_12:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_139  <= 1
 c_cpmy_139_2_Jul_13:  +1 cpmy_151  +1 cpmy_152  +1 dr_139  <= 1
 c_cpmy_139_2_Jul_14:  +1 cpmy_153  +1 cpmy_154  +1 dr_139  <= 1
 c_cpmy_140_2_Jul_1:  +1 cpmy_140  -1 dr_140  <= 0
 c_cpmy_140_2_Jul_2:  +1 cpmy_139  +1 dr_140  <= 1
 c_cpmy_140_2_Jul_10:  +1 cpmy_141  +1 cpmy_142  +1 dr_140  <= 1
 c_cpmy_140_2_Jul_11:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_140  <= 1
 c_cpmy_140_2_Jul_12:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_140  <= 1
 c_cpmy_140_2_Jul_13:  +1 cpmy_151  +1 cpmy_152  +1 dr_140  <= 1
 c_cpmy_140_2_Jul_14:  +1 cpmy_153  +1 cpmy_154  +1 dr_140  <= 1
 c_cpmy_141_3_Mar_1:  +1 cpmy_141  -1 dr_141  <= 0
 c_cpmy_141_3_Mar_2:  +1 cpmy_142  +1 dr_141  <= 1
 c_cpmy_141_3_Mar_3:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_141  <= 1
 c_cpmy_141_3_Mar_4:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_141  <= 1
 c_cpmy_141_3_Mar_5:  +1 cpmy_151  +1 cpmy_152  +1 dr_141  <= 1
 c_cpmy_141_3_Mar_6:  +1 cpmy_153  +1 cpmy_154  +1 dr_141  <= 1
 c_cpmy_141_3_Mar_14:  +1 cpmy_155  +1 cpmy_156  +1 dr_141  <= 1
 c_cpmy_142_3_Mar_1:  +1 cpmy_142  -1 dr_142  <= 0
 c_cpmy_142_3_Mar_2:  +1 cpmy_141  +1 dr_142  <= 1
 c_cpmy_142_3_Mar_3:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_142  <= 1
 c_cpmy_142_3_Mar_4:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_142  <= 1
 c_cpmy_142_3_Mar_5:  +1 cpmy_151  +1 cpmy_152  +1 dr_142  <= 1
 c_cpmy_142_3_Mar_6:  +1 cpmy_153  +1 cpmy_154  +1 dr_142  <= 1
 c_cpmy_142_3_Mar_14:  +1 cpmy_155  +1 cpmy_156  +1 dr_142  <= 1
 c_cpmy_142_3_Mar_15:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_142  <= 1
 c_cpmy_143_3_Apr_1:  +1 cpmy_143  -1 dr_143  <= 0
 c_cpmy_143_3_Apr_2:  +1 cpmy_144  +1 cpmy_145  +1 cpmy_146  +1 dr_143  <= 1
 c_cpmy_143_3_Apr_3:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_143  <= 1
 c_cpmy_143_3_Apr_4:  +1 cpmy_151  +1 cpmy_152  +1 dr_143  <= 1
 c_cpmy_143_3_Apr_5:  +1 cpmy_153  +1 cpmy_154  +1 dr_143  <= 1
 c_cpmy_143_3_Apr_13:  +1 cpmy_155  +1 cpmy_156  +1 dr_143  <= 1
 c_cpmy_143_3_Apr_14:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_143  <= 1
 c_cpmy_144_3_Apr_1:  +1 cpmy_144  -1 dr_144  <= 0
 c_cpmy_144_3_Apr_2:  +1 cpmy_143  +1 cpmy_145  +1 cpmy_146  +1 dr_144  <= 1
 c_cpmy_144_3_Apr_3:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_144  <= 1
 c_cpmy_144_3_Apr_4:  +1 cpmy_151  +1 cpmy_152  +1 dr_144  <= 1
 c_cpmy_144_3_Apr_5:  +1 cpmy_153  +1 cpmy_154  +1 dr_144  <= 1
 c_cpmy_144_3_Apr_13:  +1 cpmy_155  +1 cpmy_156  +1 dr_144  <= 1
 c_cpmy_144_3_Apr_14:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_144  <= 1
 c_cpmy_144_3_Apr_15:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_144  <= 1
 c_cpmy_145_3_Apr_1:  +1 cpmy_145  -1 dr_145  <= 0
 c_cpmy_145_3_Apr_2:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_146  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_3:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_4:  +1 cpmy_151  +1 cpmy_152  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_5:  +1 cpmy_153  +1 cpmy_154  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_13:  +1 cpmy_155  +1 cpmy_156  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_14:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_15:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_145  <= 1
 c_cpmy_145_3_Apr_16:  +1 cpmy_165  +1 cpmy_166  +1 dr_145  <= 1
 c_cpmy_146_3_Apr_1:  +1 cpmy_146  -1 dr_146  <= 0
 c_cpmy_146_3_Apr_2:  +1 cpmy_143  +1 cpmy_144  +1 cpmy_145  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_3:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_4:  +1 cpmy_151  +1 cpmy_152  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_5:  +1 cpmy_153  +1 cpmy_154  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_13:  +1 cpmy_155  +1 cpmy_156  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_14:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_15:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_146  <= 1
 c_cpmy_146_3_Apr_16:  +1 cpmy_165  +1 cpmy_166  +1 dr_146  <= 1
 c_cpmy_147_3_May_1:  +1 cpmy_147  -1 dr_147  <= 0
 c_cpmy_147_3_May_2:  +1 cpmy_148  +1 cpmy_149  +1 cpmy_150  +1 dr_147  <= 1
 c_cpmy_147_3_May_3:  +1 cpmy_151  +1 cpmy_152  +1 dr_147  <= 1
 c_cpmy_147_3_May_4:  +1 cpmy_153  +1 cpmy_154  +1 dr_147  <= 1
 c_cpmy_147_3_May_12:  +1 cpmy_155  +1 cpmy_156  +1 dr_147  <= 1
 c_cpmy_147_3_May_13:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_147  <= 1
 c_cpmy_147_3_May_14:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_147  <= 1
 c_cpmy_148_3_May_1:  +1 cpmy_148  -1 dr_148  <= 0
 c_cpmy_148_3_May_2:  +1 cpmy_147  +1 cpmy_149  +1 cpmy_150  +1 dr_148  <= 1
 c_cpmy_148_3_May_3:  +1 cpmy_151  +1 cpmy_152  +1 dr_148  <= 1
 c_cpmy_148_3_May_4:  +1 cpmy_153  +1 cpmy_154  +1 dr_148  <= 1
 c_cpmy_148_3_May_12:  +1 cpmy_155  +1 cpmy_156  +1 dr_148  <= 1
 c_cpmy_148_3_May_13:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_148  <= 1
 c_cpmy_148_3_May_14:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_148  <= 1
 c_cpmy_148_3_May_15:  +1 cpmy_165  +1 cpmy_166  +1 dr_148  <= 1
 c_cpmy_149_3_May_1:  +1 cpmy_149  -1 dr_149  <= 0
 c_cpmy_149_3_May_2:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_150  +1 dr_149  <= 1
 c_cpmy_149_3_May_3:  +1 cpmy_151  +1 cpmy_152  +1 dr_149  <= 1
 c_cpmy_149_3_May_4:  +1 cpmy_153  +1 cpmy_154  +1 dr_149  <= 1
 c_cpmy_149_3_May_12:  +1 cpmy_155  +1 cpmy_156  +1 dr_149  <= 1
 c_cpmy_149_3_May_13:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_149  <= 1
 c_cpmy_149_3_May_14:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_149  <= 1
 c_cpmy_149_3_May_15:  +1 cpmy_165  +1 cpmy_166  +1 dr_149  <= 1
 c_cpmy_149_3_May_16:  +1 cpmy_167  +1 cpmy_168  +1 dr_149  <= 1
 c_cpmy_150_3_May_1:  +1 cpmy_150  -1 dr_150  <= 0
 c_cpmy_150_3_May_2:  +1 cpmy_147  +1 cpmy_148  +1 cpmy_149  +1 dr_150  <= 1
 c_cpmy_150_3_May_3:  +1 cpmy_151  +1 cpmy_152  +1 dr_150  <= 1
 c_cpmy_150_3_May_4:  +1 cpmy_153  +1 cpmy_154  +1 dr_150  <= 1
 c_cpmy_150_3_May_12:  +1 cpmy_155  +1 cpmy_156  +1 dr_150  <= 1
 c_cpmy_150_3_May_13:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_150  <= 1
 c_cpmy_150_3_May_14:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_150  <= 1
 c_cpmy_150_3_May_15:  +1 cpmy_165  +1 cpmy_166  +1 dr_150  <= 1
 c_cpmy_150_3_May_16:  +1 cpmy_167  +1 cpmy_168  +1 dr_150  <= 1
 c_cpmy_151_3_Jun_1:  +1 cpmy_151  -1 dr_151  <= 0
 c_cpmy_151_3_Jun_2:  +1 cpmy_152  +1 dr_151  <= 1
 c_cpmy_151_3_Jun_3:  +1 cpmy_153  +1 cpmy_154  +1 dr_151  <= 1
 c_cpmy_151_3_Jun_11:  +1 cpmy_155  +1 cpmy_156  +1 dr_151  <= 1
 c_cpmy_151_3_Jun_12:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_151  <= 1
 c_cpmy_151_3_Jun_13:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_151  <= 1
 c_cpmy_151_3_Jun_14:  +1 cpmy_165  +1 cpmy_166  +1 dr_151  <= 1
 c_cpmy_151_3_Jun_15:  +1 cpmy_167  +1 cpmy_168  +1 dr_151  <= 1
 c_cpmy_152_3_Jun_1:  +1 cpmy_152  -1 dr_152  <= 0
 c_cpmy_152_3_Jun_2:  +1 cpmy_151  +1 dr_152  <= 1
 c_cpmy_152_3_Jun_3:  +1 cpmy_153  +1 cpmy_154  +1 dr_152  <= 1
 c_cpmy_152_3_Jun_11:  +1 cpmy_155  +1 cpmy_156  +1 dr_152  <= 1
 c_cpmy_152_3_Jun_12:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_152  <= 1
 c_cpmy_152_3_Jun_13:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_152  <= 1
 c_cpmy_152_3_Jun_14:  +1 cpmy_165  +1 cpmy_166  +1 dr_152  <= 1
 c_cpmy_152_3_Jun_15:  +1 cpmy_167  +1 cpmy_168  +1 dr_152  <= 1
 c_cpmy_153_3_Jul_1:  +1 cpmy_153  -1 dr_153  <= 0
 c_cpmy_153_3_Jul_2:  +1 cpmy_154  +1 dr_153  <= 1
 c_cpmy_153_3_Jul_10:  +1 cpmy_155  +1 cpmy_156  +1 dr_153  <= 1
 c_cpmy_153_3_Jul_11:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_153  <= 1
 c_cpmy_153_3_Jul_12:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_153  <= 1
 c_cpmy_153_3_Jul_13:  +1 cpmy_165  +1 cpmy_166  +1 dr_153  <= 1
 c_cpmy_153_3_Jul_14:  +1 cpmy_167  +1 cpmy_168  +1 dr_153  <= 1
 c_cpmy_154_3_Jul_1:  +1 cpmy_154  -1 dr_154  <= 0
 c_cpmy_154_3_Jul_2:  +1 cpmy_153  +1 dr_154  <= 1
 c_cpmy_154_3_Jul_10:  +1 cpmy_155  +1 cpmy_156  +1 dr_154  <= 1
 c_cpmy_154_3_Jul_11:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_154  <= 1
 c_cpmy_154_3_Jul_12:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_154  <= 1
 c_cpmy_154_3_Jul_13:  +1 cpmy_165  +1 cpmy_166  +1 dr_154  <= 1
 c_cpmy_154_3_Jul_14:  +1 cpmy_167  +1 cpmy_168  +1 dr_154  <= 1
 c_cpmy_155_4_Mar_1:  +1 cpmy_155  -1 dr_155  <= 0
 c_cpmy_155_4_Mar_2:  +1 cpmy_156  +1 dr_155  <= 1
 c_cpmy_155_4_Mar_3:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_155  <= 1
 c_cpmy_155_4_Mar_4:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_155  <= 1
 c_cpmy_155_4_Mar_5:  +1 cpmy_165  +1 cpmy_166  +1 dr_155  <= 1
 c_cpmy_155_4_Mar_6:  +1 cpmy_167  +1 cpmy_168  +1 dr_155  <= 1
 c_cpmy_156_4_Mar_1:  +1 cpmy_156  -1 dr_156  <= 0
 c_cpmy_156_4_Mar_2:  +1 cpmy_155  +1 dr_156  <= 1
 c_cpmy_156_4_Mar_3:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_156  <= 1
 c_cpmy_156_4_Mar_4:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_156  <= 1
 c_cpmy_156_4_Mar_5:  +1 cpmy_165  +1 cpmy_166  +1 dr_156  <= 1
 c_cpmy_156_4_Mar_6:  +1 cpmy_167  +1 cpmy_168  +1 dr_156  <= 1
 c_cpmy_157_4_Apr_1:  +1 cpmy_157  -1 dr_157  <= 0
 c_cpmy_157_4_Apr_2:  +1 cpmy_158  +1 cpmy_159  +1 cpmy_160  +1 dr_157  <= 1
 c_cpmy_157_4_Apr_3:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_157  <= 1
 c_cpmy_157_4_Apr_4:  +1 cpmy_165  +1 cpmy_166  +1 dr_157  <= 1
 c_cpmy_157_4_Apr_5:  +1 cpmy_167  +1 cpmy_168  +1 dr_157  <= 1
 c_cpmy_158_4_Apr_1:  +1 cpmy_158  -1 dr_158  <= 0
 c_cpmy_158_4_Apr_2:  +1 cpmy_157  +1 cpmy_159  +1 cpmy_160  +1 dr_158  <= 1
 c_cpmy_158_4_Apr_3:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_158  <= 1
 c_cpmy_158_4_Apr_4:  +1 cpmy_165  +1 cpmy_166  +1 dr_158  <= 1
 c_cpmy_158_4_Apr_5:  +1 cpmy_167  +1 cpmy_168  +1 dr_158  <= 1
 c_cpmy_159_4_Apr_1:  +1 cpmy_159  -1 dr_159  <= 0
 c_cpmy_159_4_Apr_2:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_160  +1 dr_159  <= 1
 c_cpmy_159_4_Apr_3:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_159  <= 1
 c_cpmy_159_4_Apr_4:  +1 cpmy_165  +1 cpmy_166  +1 dr_159  <= 1
 c_cpmy_159_4_Apr_5:  +1 cpmy_167  +1 cpmy_168  +1 dr_159  <= 1
 c_cpmy_160_4_Apr_1:  +1 cpmy_160  -1 dr_160  <= 0
 c_cpmy_160_4_Apr_2:  +1 cpmy_157  +1 cpmy_158  +1 cpmy_159  +1 dr_160  <= 1
 c_cpmy_160_4_Apr_3:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_160  <= 1
 c_cpmy_160_4_Apr_4:  +1 cpmy_165  +1 cpmy_166  +1 dr_160  <= 1
 c_cpmy_160_4_Apr_5:  +1 cpmy_167  +1 cpmy_168  +1 dr_160  <= 1
 c_cpmy_161_4_May_1:  +1 cpmy_161  -1 dr_161  <= 0
 c_cpmy_161_4_May_2:  +1 cpmy_162  +1 cpmy_163  +1 cpmy_164  +1 dr_161  <= 1
 c_cpmy_161_4_May_3:  +1 cpmy_165  +1 cpmy_166  +1 dr_161  <= 1
 c_cpmy_161_4_May_4:  +1 cpmy_167  +1 cpmy_168  +1 dr_161  <= 1
 c_cpmy_162_4_May_1:  +1 cpmy_162  -1 dr_162  <= 0
 c_cpmy_162_4_May_2:  +1 cpmy_161  +1 cpmy_163  +1 cpmy_164  +1 dr_162  <= 1
 c_cpmy_162_4_May_3:  +1 cpmy_165  +1 cpmy_166  +1 dr_162  <= 1
 c_cpmy_162_4_May_4:  +1 cpmy_167  +1 cpmy_168  +1 dr_162  <= 1
 c_cpmy_163_4_May_1:  +1 cpmy_163  -1 dr_163  <= 0
 c_cpmy_163_4_May_2:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_164  +1 dr_163  <= 1
 c_cpmy_163_4_May_3:  +1 cpmy_165  +1 cpmy_166  +1 dr_163  <= 1
 c_cpmy_163_4_May_4:  +1 cpmy_167  +1 cpmy_168  +1 dr_163  <= 1
 c_cpmy_164_4_May_1:  +1 cpmy_164  -1 dr_164  <= 0
 c_cpmy_164_4_May_2:  +1 cpmy_161  +1 cpmy_162  +1 cpmy_163  +1 dr_164  <= 1
 c_cpmy_164_4_May_3:  +1 cpmy_165  +1 cpmy_166  +1 dr_164  <= 1
 c_cpmy_164_4_May_4:  +1 cpmy_167  +1 cpmy_168  +1 dr_164  <= 1
 c_cpmy_165_4_Jun_1:  +1 cpmy_165  -1 dr_165  <= 0
 c_cpmy_165_4_Jun_2:  +1 cpmy_166  +1 dr_165  <= 1
 c_cpmy_165_4_Jun_3:  +1 cpmy_167  +1 cpmy_168  +1 dr_165  <= 1
 c_cpmy_166_4_Jun_1:  +1 cpmy_166  -1 dr_166  <= 0
 c_cpmy_166_4_Jun_2:  +1 cpmy_165  +1 dr_166  <= 1
 c_cpmy_166_4_Jun_3:  +1 cpmy_167  +1 cpmy_168  +1 dr_166  <= 1
 c_cpmy_167_4_Jul_1:  +1 cpmy_167  -1 dr_167  <= 0
 c_cpmy_167_4_Jul_2:  +1 cpmy_168  +1 dr_167  <= 1
 c_cpmy_168_4_Jul_1:  +1 cpmy_168  -1 dr_168  <= 0
 c_cpmy_168_4_Jul_2:  +1 cpmy_167  +1 dr_168  <= 1
 c_cpmy_169_1_Mar_1:  +1 cpmy_169  -1 dr_169  <= 0
 c_cpmy_169_1_Mar_2:  +1 cpmy_170  +1 dr_169  <= 1
 c_cpmy_169_1_Mar_3:  +1 cpmy_171  +1 cpmy_172  +1 cpmy_173  +1 cpmy_174  +1 dr_169  <= 1
 c_cpmy_169_1_Mar_4:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_169  <= 1
 c_cpmy_169_1_Mar_5:  +1 cpmy_179  +1 cpmy_180  +1 dr_169  <= 1
 c_cpmy_169_1_Mar_6:  +1 cpmy_181  +1 cpmy_182  +1 dr_169  <= 1
 c_cpmy_169_1_Mar_14:  +1 cpmy_183  +1 cpmy_184  +1 dr_169  <= 1
 c_cpmy_170_1_Mar_1:  +1 cpmy_170  -1 dr_170  <= 0
 c_cpmy_170_1_Mar_2:  +1 cpmy_169  +1 dr_170  <= 1
 c_cpmy_170_1_Mar_3:  +1 cpmy_171  +1 cpmy_172  +1 cpmy_173  +1 cpmy_174  +1 dr_170  <= 1
 c_cpmy_170_1_Mar_4:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_170  <= 1
 c_cpmy_170_1_Mar_5:  +1 cpmy_179  +1 cpmy_180  +1 dr_170  <= 1
 c_cpmy_170_1_Mar_6:  +1 cpmy_181  +1 cpmy_182  +1 dr_170  <= 1
 c_cpmy_170_1_Mar_14:  +1 cpmy_183  +1 cpmy_184  +1 dr_170  <= 1
 c_cpmy_170_1_Mar_15:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_170  <= 1
 c_cpmy_171_1_Apr_1:  +1 cpmy_171  -1 dr_171  <= 0
 c_cpmy_171_1_Apr_2:  +1 cpmy_172  +1 cpmy_173  +1 cpmy_174  +1 dr_171  <= 1
 c_cpmy_171_1_Apr_3:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_171  <= 1
 c_cpmy_171_1_Apr_4:  +1 cpmy_179  +1 cpmy_180  +1 dr_171  <= 1
 c_cpmy_171_1_Apr_5:  +1 cpmy_181  +1 cpmy_182  +1 dr_171  <= 1
 c_cpmy_171_1_Apr_13:  +1 cpmy_183  +1 cpmy_184  +1 dr_171  <= 1
 c_cpmy_171_1_Apr_14:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_171  <= 1
 c_cpmy_172_1_Apr_1:  +1 cpmy_172  -1 dr_172  <= 0
 c_cpmy_172_1_Apr_2:  +1 cpmy_171  +1 cpmy_173  +1 cpmy_174  +1 dr_172  <= 1
 c_cpmy_172_1_Apr_3:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_172  <= 1
 c_cpmy_172_1_Apr_4:  +1 cpmy_179  +1 cpmy_180  +1 dr_172  <= 1
 c_cpmy_172_1_Apr_5:  +1 cpmy_181  +1 cpmy_182  +1 dr_172  <= 1
 c_cpmy_172_1_Apr_13:  +1 cpmy_183  +1 cpmy_184  +1 dr_172  <= 1
 c_cpmy_172_1_Apr_14:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_172  <= 1
 c_cpmy_172_1_Apr_15:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_172  <= 1
 c_cpmy_173_1_Apr_1:  +1 cpmy_173  -1 dr_173  <= 0
 c_cpmy_173_1_Apr_2:  +1 cpmy_171  +1 cpmy_172  +1 cpmy_174  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_3:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_4:  +1 cpmy_179  +1 cpmy_180  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_5:  +1 cpmy_181  +1 cpmy_182  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_13:  +1 cpmy_183  +1 cpmy_184  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_14:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_15:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_173  <= 1
 c_cpmy_173_1_Apr_16:  +1 cpmy_193  +1 cpmy_194  +1 dr_173  <= 1
 c_cpmy_174_1_Apr_1:  +1 cpmy_174  -1 dr_174  <= 0
 c_cpmy_174_1_Apr_2:  +1 cpmy_171  +1 cpmy_172  +1 cpmy_173  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_3:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_4:  +1 cpmy_179  +1 cpmy_180  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_5:  +1 cpmy_181  +1 cpmy_182  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_13:  +1 cpmy_183  +1 cpmy_184  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_14:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_15:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_174  <= 1
 c_cpmy_174_1_Apr_16:  +1 cpmy_193  +1 cpmy_194  +1 dr_174  <= 1
 c_cpmy_175_1_May_1:  +1 cpmy_175  -1 dr_175  <= 0
 c_cpmy_175_1_May_2:  +1 cpmy_176  +1 cpmy_177  +1 cpmy_178  +1 dr_175  <= 1
 c_cpmy_175_1_May_3:  +1 cpmy_179  +1 cpmy_180  +1 dr_175  <= 1
 c_cpmy_175_1_May_4:  +1 cpmy_181  +1 cpmy_182  +1 dr_175  <= 1
 c_cpmy_175_1_May_12:  +1 cpmy_183  +1 cpmy_184  +1 dr_175  <= 1
 c_cpmy_175_1_May_13:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_175  <= 1
 c_cpmy_175_1_May_14:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_175  <= 1
 c_cpmy_176_1_May_1:  +1 cpmy_176  -1 dr_176  <= 0
 c_cpmy_176_1_May_2:  +1 cpmy_175  +1 cpmy_177  +1 cpmy_178  +1 dr_176  <= 1
 c_cpmy_176_1_May_3:  +1 cpmy_179  +1 cpmy_180  +1 dr_176  <= 1
 c_cpmy_176_1_May_4:  +1 cpmy_181  +1 cpmy_182  +1 dr_176  <= 1
 c_cpmy_176_1_May_12:  +1 cpmy_183  +1 cpmy_184  +1 dr_176  <= 1
 c_cpmy_176_1_May_13:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_176  <= 1
 c_cpmy_176_1_May_14:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_176  <= 1
 c_cpmy_176_1_May_15:  +1 cpmy_193  +1 cpmy_194  +1 dr_176  <= 1
 c_cpmy_177_1_May_1:  +1 cpmy_177  -1 dr_177  <= 0
 c_cpmy_177_1_May_2:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_178  +1 dr_177  <= 1
 c_cpmy_177_1_May_3:  +1 cpmy_179  +1 cpmy_180  +1 dr_177  <= 1
 c_cpmy_177_1_May_4:  +1 cpmy_181  +1 cpmy_182  +1 dr_177  <= 1
 c_cpmy_177_1_May_12:  +1 cpmy_183  +1 cpmy_184  +1 dr_177  <= 1
 c_cpmy_177_1_May_13:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_177  <= 1
 c_cpmy_177_1_May_14:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_177  <= 1
 c_cpmy_177_1_May_15:  +1 cpmy_193  +1 cpmy_194  +1 dr_177  <= 1
 c_cpmy_177_1_May_16:  +1 cpmy_195  +1 cpmy_196  +1 dr_177  <= 1
 c_cpmy_178_1_May_1:  +1 cpmy_178  -1 dr_178  <= 0
 c_cpmy_178_1_May_2:  +1 cpmy_175  +1 cpmy_176  +1 cpmy_177  +1 dr_178  <= 1
 c_cpmy_178_1_May_3:  +1 cpmy_179  +1 cpmy_180  +1 dr_178  <= 1
 c_cpmy_178_1_May_4:  +1 cpmy_181  +1 cpmy_182  +1 dr_178  <= 1
 c_cpmy_178_1_May_12:  +1 cpmy_183  +1 cpmy_184  +1 dr_178  <= 1
 c_cpmy_178_1_May_13:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_178  <= 1
 c_cpmy_178_1_May_14:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_178  <= 1
 c_cpmy_178_1_May_15:  +1 cpmy_193  +1 cpmy_194  +1 dr_178  <= 1
 c_cpmy_178_1_May_16:  +1 cpmy_195  +1 cpmy_196  +1 dr_178  <= 1
 c_cpmy_179_1_Jun_1:  +1 cpmy_179  -1 dr_179  <= 0
 c_cpmy_179_1_Jun_2:  +1 cpmy_180  +1 dr_179  <= 1
 c_cpmy_179_1_Jun_3:  +1 cpmy_181  +1 cpmy_182  +1 dr_179  <= 1
 c_cpmy_179_1_Jun_11:  +1 cpmy_183  +1 cpmy_184  +1 dr_179  <= 1
 c_cpmy_179_1_Jun_12:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_179  <= 1
 c_cpmy_179_1_Jun_13:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_179  <= 1
 c_cpmy_179_1_Jun_14:  +1 cpmy_193  +1 cpmy_194  +1 dr_179  <= 1
 c_cpmy_179_1_Jun_15:  +1 cpmy_195  +1 cpmy_196  +1 dr_179  <= 1
 c_cpmy_180_1_Jun_1:  +1 cpmy_180  -1 dr_180  <= 0
 c_cpmy_180_1_Jun_2:  +1 cpmy_179  +1 dr_180  <= 1
 c_cpmy_180_1_Jun_3:  +1 cpmy_181  +1 cpmy_182  +1 dr_180  <= 1
 c_cpmy_180_1_Jun_11:  +1 cpmy_183  +1 cpmy_184  +1 dr_180  <= 1
 c_cpmy_180_1_Jun_12:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_180  <= 1
 c_cpmy_180_1_Jun_13:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_180  <= 1
 c_cpmy_180_1_Jun_14:  +1 cpmy_193  +1 cpmy_194  +1 dr_180  <= 1
 c_cpmy_180_1_Jun_15:  +1 cpmy_195  +1 cpmy_196  +1 dr_180  <= 1
 c_cpmy_181_1_Jul_1:  +1 cpmy_181  -1 dr_181  <= 0
 c_cpmy_181_1_Jul_2:  +1 cpmy_182  +1 dr_181  <= 1
 c_cpmy_181_1_Jul_10:  +1 cpmy_183  +1 cpmy_184  +1 dr_181  <= 1
 c_cpmy_181_1_Jul_11:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_181  <= 1
 c_cpmy_181_1_Jul_12:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_181  <= 1
 c_cpmy_181_1_Jul_13:  +1 cpmy_193  +1 cpmy_194  +1 dr_181  <= 1
 c_cpmy_181_1_Jul_14:  +1 cpmy_195  +1 cpmy_196  +1 dr_181  <= 1
 c_cpmy_182_1_Jul_1:  +1 cpmy_182  -1 dr_182  <= 0
 c_cpmy_182_1_Jul_2:  +1 cpmy_181  +1 dr_182  <= 1
 c_cpmy_182_1_Jul_10:  +1 cpmy_183  +1 cpmy_184  +1 dr_182  <= 1
 c_cpmy_182_1_Jul_11:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_182  <= 1
 c_cpmy_182_1_Jul_12:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_182  <= 1
 c_cpmy_182_1_Jul_13:  +1 cpmy_193  +1 cpmy_194  +1 dr_182  <= 1
 c_cpmy_182_1_Jul_14:  +1 cpmy_195  +1 cpmy_196  +1 dr_182  <= 1
 c_cpmy_183_2_Mar_1:  +1 cpmy_183  -1 dr_183  <= 0
 c_cpmy_183_2_Mar_2:  +1 cpmy_184  +1 dr_183  <= 1
 c_cpmy_183_2_Mar_3:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_183  <= 1
 c_cpmy_183_2_Mar_4:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_183  <= 1
 c_cpmy_183_2_Mar_5:  +1 cpmy_193  +1 cpmy_194  +1 dr_183  <= 1
 c_cpmy_183_2_Mar_6:  +1 cpmy_195  +1 cpmy_196  +1 dr_183  <= 1
 c_cpmy_183_2_Mar_14:  +1 cpmy_197  +1 cpmy_198  +1 dr_183  <= 1
 c_cpmy_184_2_Mar_1:  +1 cpmy_184  -1 dr_184  <= 0
 c_cpmy_184_2_Mar_2:  +1 cpmy_183  +1 dr_184  <= 1
 c_cpmy_184_2_Mar_3:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_184  <= 1
 c_cpmy_184_2_Mar_4:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_184  <= 1
 c_cpmy_184_2_Mar_5:  +1 cpmy_193  +1 cpmy_194  +1 dr_184  <= 1
 c_cpmy_184_2_Mar_6:  +1 cpmy_195  +1 cpmy_196  +1 dr_184  <= 1
 c_cpmy_184_2_Mar_14:  +1 cpmy_197  +1 cpmy_198  +1 dr_184  <= 1
 c_cpmy_184_2_Mar_15:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_184  <= 1
 c_cpmy_185_2_Apr_1:  +1 cpmy_185  -1 dr_185  <= 0
 c_cpmy_185_2_Apr_2:  +1 cpmy_186  +1 cpmy_187  +1 cpmy_188  +1 dr_185  <= 1
 c_cpmy_185_2_Apr_3:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_185  <= 1
 c_cpmy_185_2_Apr_4:  +1 cpmy_193  +1 cpmy_194  +1 dr_185  <= 1
 c_cpmy_185_2_Apr_5:  +1 cpmy_195  +1 cpmy_196  +1 dr_185  <= 1
 c_cpmy_185_2_Apr_13:  +1 cpmy_197  +1 cpmy_198  +1 dr_185  <= 1
 c_cpmy_185_2_Apr_14:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_185  <= 1
 c_cpmy_186_2_Apr_1:  +1 cpmy_186  -1 dr_186  <= 0
 c_cpmy_186_2_Apr_2:  +1 cpmy_185  +1 cpmy_187  +1 cpmy_188  +1 dr_186  <= 1
 c_cpmy_186_2_Apr_3:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_186  <= 1
 c_cpmy_186_2_Apr_4:  +1 cpmy_193  +1 cpmy_194  +1 dr_186  <= 1
 c_cpmy_186_2_Apr_5:  +1 cpmy_195  +1 cpmy_196  +1 dr_186  <= 1
 c_cpmy_186_2_Apr_13:  +1 cpmy_197  +1 cpmy_198  +1 dr_186  <= 1
 c_cpmy_186_2_Apr_14:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_186  <= 1
 c_cpmy_186_2_Apr_15:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_186  <= 1
 c_cpmy_187_2_Apr_1:  +1 cpmy_187  -1 dr_187  <= 0
 c_cpmy_187_2_Apr_2:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_188  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_3:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_4:  +1 cpmy_193  +1 cpmy_194  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_5:  +1 cpmy_195  +1 cpmy_196  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_13:  +1 cpmy_197  +1 cpmy_198  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_14:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_15:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_187  <= 1
 c_cpmy_187_2_Apr_16:  +1 cpmy_207  +1 cpmy_208  +1 dr_187  <= 1
 c_cpmy_188_2_Apr_1:  +1 cpmy_188  -1 dr_188  <= 0
 c_cpmy_188_2_Apr_2:  +1 cpmy_185  +1 cpmy_186  +1 cpmy_187  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_3:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_4:  +1 cpmy_193  +1 cpmy_194  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_5:  +1 cpmy_195  +1 cpmy_196  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_13:  +1 cpmy_197  +1 cpmy_198  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_14:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_15:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_188  <= 1
 c_cpmy_188_2_Apr_16:  +1 cpmy_207  +1 cpmy_208  +1 dr_188  <= 1
 c_cpmy_189_2_May_1:  +1 cpmy_189  -1 dr_189  <= 0
 c_cpmy_189_2_May_2:  +1 cpmy_190  +1 cpmy_191  +1 cpmy_192  +1 dr_189  <= 1
 c_cpmy_189_2_May_3:  +1 cpmy_193  +1 cpmy_194  +1 dr_189  <= 1
 c_cpmy_189_2_May_4:  +1 cpmy_195  +1 cpmy_196  +1 dr_189  <= 1
 c_cpmy_189_2_May_12:  +1 cpmy_197  +1 cpmy_198  +1 dr_189  <= 1
 c_cpmy_189_2_May_13:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_189  <= 1
 c_cpmy_189_2_May_14:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_189  <= 1
 c_cpmy_190_2_May_1:  +1 cpmy_190  -1 dr_190  <= 0
 c_cpmy_190_2_May_2:  +1 cpmy_189  +1 cpmy_191  +1 cpmy_192  +1 dr_190  <= 1
 c_cpmy_190_2_May_3:  +1 cpmy_193  +1 cpmy_194  +1 dr_190  <= 1
 c_cpmy_190_2_May_4:  +1 cpmy_195  +1 cpmy_196  +1 dr_190  <= 1
 c_cpmy_190_2_May_12:  +1 cpmy_197  +1 cpmy_198  +1 dr_190  <= 1
 c_cpmy_190_2_May_13:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_190  <= 1
 c_cpmy_190_2_May_14:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_190  <= 1
 c_cpmy_190_2_May_15:  +1 cpmy_207  +1 cpmy_208  +1 dr_190  <= 1
 c_cpmy_191_2_May_1:  +1 cpmy_191  -1 dr_191  <= 0
 c_cpmy_191_2_May_2:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_192  +1 dr_191  <= 1
 c_cpmy_191_2_May_3:  +1 cpmy_193  +1 cpmy_194  +1 dr_191  <= 1
 c_cpmy_191_2_May_4:  +1 cpmy_195  +1 cpmy_196  +1 dr_191  <= 1
 c_cpmy_191_2_May_12:  +1 cpmy_197  +1 cpmy_198  +1 dr_191  <= 1
 c_cpmy_191_2_May_13:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_191  <= 1
 c_cpmy_191_2_May_14:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_191  <= 1
 c_cpmy_191_2_May_15:  +1 cpmy_207  +1 cpmy_208  +1 dr_191  <= 1
 c_cpmy_191_2_May_16:  +1 cpmy_209  +1 cpmy_210  +1 dr_191  <= 1
 c_cpmy_192_2_May_1:  +1 cpmy_192  -1 dr_192  <= 0
 c_cpmy_192_2_May_2:  +1 cpmy_189  +1 cpmy_190  +1 cpmy_191  +1 dr_192  <= 1
 c_cpmy_192_2_May_3:  +1 cpmy_193  +1 cpmy_194  +1 dr_192  <= 1
 c_cpmy_192_2_May_4:  +1 cpmy_195  +1 cpmy_196  +1 dr_192  <= 1
 c_cpmy_192_2_May_12:  +1 cpmy_197  +1 cpmy_198  +1 dr_192  <= 1
 c_cpmy_192_2_May_13:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_192  <= 1
 c_cpmy_192_2_May_14:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_192  <= 1
 c_cpmy_192_2_May_15:  +1 cpmy_207  +1 cpmy_208  +1 dr_192  <= 1
 c_cpmy_192_2_May_16:  +1 cpmy_209  +1 cpmy_210  +1 dr_192  <= 1
 c_cpmy_193_2_Jun_1:  +1 cpmy_193  -1 dr_193  <= 0
 c_cpmy_193_2_Jun_2:  +1 cpmy_194  +1 dr_193  <= 1
 c_cpmy_193_2_Jun_3:  +1 cpmy_195  +1 cpmy_196  +1 dr_193  <= 1
 c_cpmy_193_2_Jun_11:  +1 cpmy_197  +1 cpmy_198  +1 dr_193  <= 1
 c_cpmy_193_2_Jun_12:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_193  <= 1
 c_cpmy_193_2_Jun_13:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_193  <= 1
 c_cpmy_193_2_Jun_14:  +1 cpmy_207  +1 cpmy_208  +1 dr_193  <= 1
 c_cpmy_193_2_Jun_15:  +1 cpmy_209  +1 cpmy_210  +1 dr_193  <= 1
 c_cpmy_194_2_Jun_1:  +1 cpmy_194  -1 dr_194  <= 0
 c_cpmy_194_2_Jun_2:  +1 cpmy_193  +1 dr_194  <= 1
 c_cpmy_194_2_Jun_3:  +1 cpmy_195  +1 cpmy_196  +1 dr_194  <= 1
 c_cpmy_194_2_Jun_11:  +1 cpmy_197  +1 cpmy_198  +1 dr_194  <= 1
 c_cpmy_194_2_Jun_12:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_194  <= 1
 c_cpmy_194_2_Jun_13:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_194  <= 1
 c_cpmy_194_2_Jun_14:  +1 cpmy_207  +1 cpmy_208  +1 dr_194  <= 1
 c_cpmy_194_2_Jun_15:  +1 cpmy_209  +1 cpmy_210  +1 dr_194  <= 1
 c_cpmy_195_2_Jul_1:  +1 cpmy_195  -1 dr_195  <= 0
 c_cpmy_195_2_Jul_2:  +1 cpmy_196  +1 dr_195  <= 1
 c_cpmy_195_2_Jul_10:  +1 cpmy_197  +1 cpmy_198  +1 dr_195  <= 1
 c_cpmy_195_2_Jul_11:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_195  <= 1
 c_cpmy_195_2_Jul_12:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_195  <= 1
 c_cpmy_195_2_Jul_13:  +1 cpmy_207  +1 cpmy_208  +1 dr_195  <= 1
 c_cpmy_195_2_Jul_14:  +1 cpmy_209  +1 cpmy_210  +1 dr_195  <= 1
 c_cpmy_196_2_Jul_1:  +1 cpmy_196  -1 dr_196  <= 0
 c_cpmy_196_2_Jul_2:  +1 cpmy_195  +1 dr_196  <= 1
 c_cpmy_196_2_Jul_10:  +1 cpmy_197  +1 cpmy_198  +1 dr_196  <= 1
 c_cpmy_196_2_Jul_11:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_196  <= 1
 c_cpmy_196_2_Jul_12:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_196  <= 1
 c_cpmy_196_2_Jul_13:  +1 cpmy_207  +1 cpmy_208  +1 dr_196  <= 1
 c_cpmy_196_2_Jul_14:  +1 cpmy_209  +1 cpmy_210  +1 dr_196  <= 1
 c_cpmy_197_3_Mar_1:  +1 cpmy_197  -1 dr_197  <= 0
 c_cpmy_197_3_Mar_2:  +1 cpmy_198  +1 dr_197  <= 1
 c_cpmy_197_3_Mar_3:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_197  <= 1
 c_cpmy_197_3_Mar_4:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_197  <= 1
 c_cpmy_197_3_Mar_5:  +1 cpmy_207  +1 cpmy_208  +1 dr_197  <= 1
 c_cpmy_197_3_Mar_6:  +1 cpmy_209  +1 cpmy_210  +1 dr_197  <= 1
 c_cpmy_197_3_Mar_14:  +1 cpmy_211  +1 cpmy_212  +1 dr_197  <= 1
 c_cpmy_198_3_Mar_1:  +1 cpmy_198  -1 dr_198  <= 0
 c_cpmy_198_3_Mar_2:  +1 cpmy_197  +1 dr_198  <= 1
 c_cpmy_198_3_Mar_3:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_198  <= 1
 c_cpmy_198_3_Mar_4:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_198  <= 1
 c_cpmy_198_3_Mar_5:  +1 cpmy_207  +1 cpmy_208  +1 dr_198  <= 1
 c_cpmy_198_3_Mar_6:  +1 cpmy_209  +1 cpmy_210  +1 dr_198  <= 1
 c_cpmy_198_3_Mar_14:  +1 cpmy_211  +1 cpmy_212  +1 dr_198  <= 1
 c_cpmy_198_3_Mar_15:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_198  <= 1
 c_cpmy_199_3_Apr_1:  +1 cpmy_199  -1 dr_199  <= 0
 c_cpmy_199_3_Apr_2:  +1 cpmy_200  +1 cpmy_201  +1 cpmy_202  +1 dr_199  <= 1
 c_cpmy_199_3_Apr_3:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_199  <= 1
 c_cpmy_199_3_Apr_4:  +1 cpmy_207  +1 cpmy_208  +1 dr_199  <= 1
 c_cpmy_199_3_Apr_5:  +1 cpmy_209  +1 cpmy_210  +1 dr_199  <= 1
 c_cpmy_199_3_Apr_13:  +1 cpmy_211  +1 cpmy_212  +1 dr_199  <= 1
 c_cpmy_199_3_Apr_14:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_199  <= 1
 c_cpmy_200_3_Apr_1:  +1 cpmy_200  -1 dr_200  <= 0
 c_cpmy_200_3_Apr_2:  +1 cpmy_199  +1 cpmy_201  +1 cpmy_202  +1 dr_200  <= 1
 c_cpmy_200_3_Apr_3:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_200  <= 1
 c_cpmy_200_3_Apr_4:  +1 cpmy_207  +1 cpmy_208  +1 dr_200  <= 1
 c_cpmy_200_3_Apr_5:  +1 cpmy_209  +1 cpmy_210  +1 dr_200  <= 1
 c_cpmy_200_3_Apr_13:  +1 cpmy_211  +1 cpmy_212  +1 dr_200  <= 1
 c_cpmy_200_3_Apr_14:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_200  <= 1
 c_cpmy_200_3_Apr_15:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_200  <= 1
 c_cpmy_201_3_Apr_1:  +1 cpmy_201  -1 dr_201  <= 0
 c_cpmy_201_3_Apr_2:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_202  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_3:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_4:  +1 cpmy_207  +1 cpmy_208  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_5:  +1 cpmy_209  +1 cpmy_210  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_13:  +1 cpmy_211  +1 cpmy_212  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_14:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_15:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_201  <= 1
 c_cpmy_201_3_Apr_16:  +1 cpmy_221  +1 cpmy_222  +1 dr_201  <= 1
 c_cpmy_202_3_Apr_1:  +1 cpmy_202  -1 dr_202  <= 0
 c_cpmy_202_3_Apr_2:  +1 cpmy_199  +1 cpmy_200  +1 cpmy_201  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_3:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_4:  +1 cpmy_207  +1 cpmy_208  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_5:  +1 cpmy_209  +1 cpmy_210  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_13:  +1 cpmy_211  +1 cpmy_212  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_14:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_15:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_202  <= 1
 c_cpmy_202_3_Apr_16:  +1 cpmy_221  +1 cpmy_222  +1 dr_202  <= 1
 c_cpmy_203_3_May_1:  +1 cpmy_203  -1 dr_203  <= 0
 c_cpmy_203_3_May_2:  +1 cpmy_204  +1 cpmy_205  +1 cpmy_206  +1 dr_203  <= 1
 c_cpmy_203_3_May_3:  +1 cpmy_207  +1 cpmy_208  +1 dr_203  <= 1
 c_cpmy_203_3_May_4:  +1 cpmy_209  +1 cpmy_210  +1 dr_203  <= 1
 c_cpmy_203_3_May_12:  +1 cpmy_211  +1 cpmy_212  +1 dr_203  <= 1
 c_cpmy_203_3_May_13:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_203  <= 1
 c_cpmy_203_3_May_14:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_203  <= 1
 c_cpmy_204_3_May_1:  +1 cpmy_204  -1 dr_204  <= 0
 c_cpmy_204_3_May_2:  +1 cpmy_203  +1 cpmy_205  +1 cpmy_206  +1 dr_204  <= 1
 c_cpmy_204_3_May_3:  +1 cpmy_207  +1 cpmy_208  +1 dr_204  <= 1
 c_cpmy_204_3_May_4:  +1 cpmy_209  +1 cpmy_210  +1 dr_204  <= 1
 c_cpmy_204_3_May_12:  +1 cpmy_211  +1 cpmy_212  +1 dr_204  <= 1
 c_cpmy_204_3_May_13:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_204  <= 1
 c_cpmy_204_3_May_14:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_204  <= 1
 c_cpmy_204_3_May_15:  +1 cpmy_221  +1 cpmy_222  +1 dr_204  <= 1
 c_cpmy_205_3_May_1:  +1 cpmy_205  -1 dr_205  <= 0
 c_cpmy_205_3_May_2:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_206  +1 dr_205  <= 1
 c_cpmy_205_3_May_3:  +1 cpmy_207  +1 cpmy_208  +1 dr_205  <= 1
 c_cpmy_205_3_May_4:  +1 cpmy_209  +1 cpmy_210  +1 dr_205  <= 1
 c_cpmy_205_3_May_12:  +1 cpmy_211  +1 cpmy_212  +1 dr_205  <= 1
 c_cpmy_205_3_May_13:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_205  <= 1
 c_cpmy_205_3_May_14:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_205  <= 1
 c_cpmy_205_3_May_15:  +1 cpmy_221  +1 cpmy_222  +1 dr_205  <= 1
 c_cpmy_205_3_May_16:  +1 cpmy_223  +1 cpmy_224  +1 dr_205  <= 1
 c_cpmy_206_3_May_1:  +1 cpmy_206  -1 dr_206  <= 0
 c_cpmy_206_3_May_2:  +1 cpmy_203  +1 cpmy_204  +1 cpmy_205  +1 dr_206  <= 1
 c_cpmy_206_3_May_3:  +1 cpmy_207  +1 cpmy_208  +1 dr_206  <= 1
 c_cpmy_206_3_May_4:  +1 cpmy_209  +1 cpmy_210  +1 dr_206  <= 1
 c_cpmy_206_3_May_12:  +1 cpmy_211  +1 cpmy_212  +1 dr_206  <= 1
 c_cpmy_206_3_May_13:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_206  <= 1
 c_cpmy_206_3_May_14:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_206  <= 1
 c_cpmy_206_3_May_15:  +1 cpmy_221  +1 cpmy_222  +1 dr_206  <= 1
 c_cpmy_206_3_May_16:  +1 cpmy_223  +1 cpmy_224  +1 dr_206  <= 1
 c_cpmy_207_3_Jun_1:  +1 cpmy_207  -1 dr_207  <= 0
 c_cpmy_207_3_Jun_2:  +1 cpmy_208  +1 dr_207  <= 1
 c_cpmy_207_3_Jun_3:  +1 cpmy_209  +1 cpmy_210  +1 dr_207  <= 1
 c_cpmy_207_3_Jun_11:  +1 cpmy_211  +1 cpmy_212  +1 dr_207  <= 1
 c_cpmy_207_3_Jun_12:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_207  <= 1
 c_cpmy_207_3_Jun_13:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_207  <= 1
 c_cpmy_207_3_Jun_14:  +1 cpmy_221  +1 cpmy_222  +1 dr_207  <= 1
 c_cpmy_207_3_Jun_15:  +1 cpmy_223  +1 cpmy_224  +1 dr_207  <= 1
 c_cpmy_208_3_Jun_1:  +1 cpmy_208  -1 dr_208  <= 0
 c_cpmy_208_3_Jun_2:  +1 cpmy_207  +1 dr_208  <= 1
 c_cpmy_208_3_Jun_3:  +1 cpmy_209  +1 cpmy_210  +1 dr_208  <= 1
 c_cpmy_208_3_Jun_11:  +1 cpmy_211  +1 cpmy_212  +1 dr_208  <= 1
 c_cpmy_208_3_Jun_12:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_208  <= 1
 c_cpmy_208_3_Jun_13:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_208  <= 1
 c_cpmy_208_3_Jun_14:  +1 cpmy_221  +1 cpmy_222  +1 dr_208  <= 1
 c_cpmy_208_3_Jun_15:  +1 cpmy_223  +1 cpmy_224  +1 dr_208  <= 1
 c_cpmy_209_3_Jul_1:  +1 cpmy_209  -1 dr_209  <= 0
 c_cpmy_209_3_Jul_2:  +1 cpmy_210  +1 dr_209  <= 1
 c_cpmy_209_3_Jul_10:  +1 cpmy_211  +1 cpmy_212  +1 dr_209  <= 1
 c_cpmy_209_3_Jul_11:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_209  <= 1
 c_cpmy_209_3_Jul_12:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_209  <= 1
 c_cpmy_209_3_Jul_13:  +1 cpmy_221  +1 cpmy_222  +1 dr_209  <= 1
 c_cpmy_209_3_Jul_14:  +1 cpmy_223  +1 cpmy_224  +1 dr_209  <= 1
 c_cpmy_210_3_Jul_1:  +1 cpmy_210  -1 dr_210  <= 0
 c_cpmy_210_3_Jul_2:  +1 cpmy_209  +1 dr_210  <= 1
 c_cpmy_210_3_Jul_10:  +1 cpmy_211  +1 cpmy_212  +1 dr_210  <= 1
 c_cpmy_210_3_Jul_11:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_210  <= 1
 c_cpmy_210_3_Jul_12:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_210  <= 1
 c_cpmy_210_3_Jul_13:  +1 cpmy_221  +1 cpmy_222  +1 dr_210  <= 1
 c_cpmy_210_3_Jul_14:  +1 cpmy_223  +1 cpmy_224  +1 dr_210  <= 1
 c_cpmy_211_4_Mar_1:  +1 cpmy_211  -1 dr_211  <= 0
 c_cpmy_211_4_Mar_2:  +1 cpmy_212  +1 dr_211  <= 1
 c_cpmy_211_4_Mar_3:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_211  <= 1
 c_cpmy_211_4_Mar_4:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_211  <= 1
 c_cpmy_211_4_Mar_5:  +1 cpmy_221  +1 cpmy_222  +1 dr_211  <= 1
 c_cpmy_211_4_Mar_6:  +1 cpmy_223  +1 cpmy_224  +1 dr_211  <= 1
 c_cpmy_212_4_Mar_1:  +1 cpmy_212  -1 dr_212  <= 0
 c_cpmy_212_4_Mar_2:  +1 cpmy_211  +1 dr_212  <= 1
 c_cpmy_212_4_Mar_3:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_212  <= 1
 c_cpmy_212_4_Mar_4:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_212  <= 1
 c_cpmy_212_4_Mar_5:  +1 cpmy_221  +1 cpmy_222  +1 dr_212  <= 1
 c_cpmy_212_4_Mar_6:  +1 cpmy_223  +1 cpmy_224  +1 dr_212  <= 1
 c_cpmy_213_4_Apr_1:  +1 cpmy_213  -1 dr_213  <= 0
 c_cpmy_213_4_Apr_2:  +1 cpmy_214  +1 cpmy_215  +1 cpmy_216  +1 dr_213  <= 1
 c_cpmy_213_4_Apr_3:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_213  <= 1
 c_cpmy_213_4_Apr_4:  +1 cpmy_221  +1 cpmy_222  +1 dr_213  <= 1
 c_cpmy_213_4_Apr_5:  +1 cpmy_223  +1 cpmy_224  +1 dr_213  <= 1
 c_cpmy_214_4_Apr_1:  +1 cpmy_214  -1 dr_214  <= 0
 c_cpmy_214_4_Apr_2:  +1 cpmy_213  +1 cpmy_215  +1 cpmy_216  +1 dr_214  <= 1
 c_cpmy_214_4_Apr_3:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_214  <= 1
 c_cpmy_214_4_Apr_4:  +1 cpmy_221  +1 cpmy_222  +1 dr_214  <= 1
 c_cpmy_214_4_Apr_5:  +1 cpmy_223  +1 cpmy_224  +1 dr_214  <= 1
 c_cpmy_215_4_Apr_1:  +1 cpmy_215  -1 dr_215  <= 0
 c_cpmy_215_4_Apr_2:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_216  +1 dr_215  <= 1
 c_cpmy_215_4_Apr_3:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_215  <= 1
 c_cpmy_215_4_Apr_4:  +1 cpmy_221  +1 cpmy_222  +1 dr_215  <= 1
 c_cpmy_215_4_Apr_5:  +1 cpmy_223  +1 cpmy_224  +1 dr_215  <= 1
 c_cpmy_216_4_Apr_1:  +1 cpmy_216  -1 dr_216  <= 0
 c_cpmy_216_4_Apr_2:  +1 cpmy_213  +1 cpmy_214  +1 cpmy_215  +1 dr_216  <= 1
 c_cpmy_216_4_Apr_3:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_216  <= 1
 c_cpmy_216_4_Apr_4:  +1 cpmy_221  +1 cpmy_222  +1 dr_216  <= 1
 c_cpmy_216_4_Apr_5:  +1 cpmy_223  +1 cpmy_224  +1 dr_216  <= 1
 c_cpmy_217_4_May_1:  +1 cpmy_217  -1 dr_217  <= 0
 c_cpmy_217_4_May_2:  +1 cpmy_218  +1 cpmy_219  +1 cpmy_220  +1 dr_217  <= 1
 c_cpmy_217_4_May_3:  +1 cpmy_221  +1 cpmy_222  +1 dr_217  <= 1
 c_cpmy_217_4_May_4:  +1 cpmy_223  +1 cpmy_224  +1 dr_217  <= 1
 c_cpmy_218_4_May_1:  +1 cpmy_218  -1 dr_218  <= 0
 c_cpmy_218_4_May_2:  +1 cpmy_217  +1 cpmy_219  +1 cpmy_220  +1 dr_218  <= 1
 c_cpmy_218_4_May_3:  +1 cpmy_221  +1 cpmy_222  +1 dr_218  <= 1
 c_cpmy_218_4_May_4:  +1 cpmy_223  +1 cpmy_224  +1 dr_218  <= 1
 c_cpmy_219_4_May_1:  +1 cpmy_219  -1 dr_219  <= 0
 c_cpmy_219_4_May_2:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_220  +1 dr_219  <= 1
 c_cpmy_219_4_May_3:  +1 cpmy_221  +1 cpmy_222  +1 dr_219  <= 1
 c_cpmy_219_4_May_4:  +1 cpmy_223  +1 cpmy_224  +1 dr_219  <= 1
 c_cpmy_220_4_May_1:  +1 cpmy_220  -1 dr_220  <= 0
 c_cpmy_220_4_May_2:  +1 cpmy_217  +1 cpmy_218  +1 cpmy_219  +1 dr_220  <= 1
 c_cpmy_220_4_May_3:  +1 cpmy_221  +1 cpmy_222  +1 dr_220  <= 1
 c_cpmy_220_4_May_4:  +1 cpmy_223  +1 cpmy_224  +1 dr_220  <= 1
 c_cpmy_221_4_Jun_1:  +1 cpmy_221  -1 dr_221  <= 0
 c_cpmy_221_4_Jun_2:  +1 cpmy_222  +1 dr_221  <= 1
 c_cpmy_221_4_Jun_3:  +1 cpmy_223  +1 cpmy_224  +1 dr_221  <= 1
 c_cpmy_222_4_Jun_1:  +1 cpmy_222  -1 dr_222  <= 0
 c_cpmy_222_4_Jun_2:  +1 cpmy_221  +1 dr_222  <= 1
 c_cpmy_222_4_Jun_3:  +1 cpmy_223  +1 cpmy_224  +1 dr_222  <= 1
 c_cpmy_223_4_Jul_1:  +1 cpmy_223  -1 dr_223  <= 0
 c_cpmy_223_4_Jul_2:  +1 cpmy_224  +1 dr_223  <= 1
 c_cpmy_224_4_Jul_1:  +1 cpmy_224  -1 dr_224  <= 0
 c_cpmy_224_4_Jul_2:  +1 cpmy_223  +1 dr_224  <= 1
 c_cpmy_225_1_Mar_1:  +1 cpmy_225  -1 dr_225  <= 0
 c_cpmy_225_1_Mar_2:  +1 cpmy_226  +1 dr_225  <= 1
 c_cpmy_225_1_Mar_3:  +1 cpmy_227  +1 cpmy_228  +1 cpmy_229  +1 cpmy_230  +1 dr_225  <= 1
 c_cpmy_225_1_Mar_4:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_225  <= 1
 c_cpmy_225_1_Mar_5:  +1 cpmy_235  +1 cpmy_236  +1 dr_225  <= 1
 c_cpmy_225_1_Mar_6:  +1 cpmy_237  +1 cpmy_238  +1 dr_225  <= 1
 c_cpmy_225_1_Mar_14:  +1 cpmy_239  +1 cpmy_240  +1 dr_225  <= 1
 c_cpmy_226_1_Mar_1:  +1 cpmy_226  -1 dr_226  <= 0
 c_cpmy_226_1_Mar_2:  +1 cpmy_225  +1 dr_226  <= 1
 c_cpmy_226_1_Mar_3:  +1 cpmy_227  +1 cpmy_228  +1 cpmy_229  +1 cpmy_230  +1 dr_226  <= 1
 c_cpmy_226_1_Mar_4:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_226  <= 1
 c_cpmy_226_1_Mar_5:  +1 cpmy_235  +1 cpmy_236  +1 dr_226  <= 1
 c_cpmy_226_1_Mar_6:  +1 cpmy_237  +1 cpmy_238  +1 dr_226  <= 1
 c_cpmy_226_1_Mar_14:  +1 cpmy_239  +1 cpmy_240  +1 dr_226  <= 1
 c_cpmy_226_1_Mar_15:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_226  <= 1
 c_cpmy_227_1_Apr_1:  +1 cpmy_227  -1 dr_227  <= 0
 c_cpmy_227_1_Apr_2:  +1 cpmy_228  +1 cpmy_229  +1 cpmy_230  +1 dr_227  <= 1
 c_cpmy_227_1_Apr_3:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_227  <= 1
 c_cpmy_227_1_Apr_4:  +1 cpmy_235  +1 cpmy_236  +1 dr_227  <= 1
 c_cpmy_227_1_Apr_5:  +1 cpmy_237  +1 cpmy_238  +1 dr_227  <= 1
 c_cpmy_227_1_Apr_13:  +1 cpmy_239  +1 cpmy_240  +1 dr_227  <= 1
 c_cpmy_227_1_Apr_14:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_227  <= 1
 c_cpmy_228_1_Apr_1:  +1 cpmy_228  -1 dr_228  <= 0
 c_cpmy_228_1_Apr_2:  +1 cpmy_227  +1 cpmy_229  +1 cpmy_230  +1 dr_228  <= 1
 c_cpmy_228_1_Apr_3:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_228  <= 1
 c_cpmy_228_1_Apr_4:  +1 cpmy_235  +1 cpmy_236  +1 dr_228  <= 1
 c_cpmy_228_1_Apr_5:  +1 cpmy_237  +1 cpmy_238  +1 dr_228  <= 1
 c_cpmy_228_1_Apr_13:  +1 cpmy_239  +1 cpmy_240  +1 dr_228  <= 1
 c_cpmy_228_1_Apr_14:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_228  <= 1
 c_cpmy_228_1_Apr_15:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_228  <= 1
 c_cpmy_229_1_Apr_1:  +1 cpmy_229  -1 dr_229  <= 0
 c_cpmy_229_1_Apr_2:  +1 cpmy_227  +1 cpmy_228  +1 cpmy_230  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_3:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_4:  +1 cpmy_235  +1 cpmy_236  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_5:  +1 cpmy_237  +1 cpmy_238  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_13:  +1 cpmy_239  +1 cpmy_240  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_14:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_15:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_229  <= 1
 c_cpmy_229_1_Apr_16:  +1 cpmy_249  +1 cpmy_250  +1 dr_229  <= 1
 c_cpmy_230_1_Apr_1:  +1 cpmy_230  -1 dr_230  <= 0
 c_cpmy_230_1_Apr_2:  +1 cpmy_227  +1 cpmy_228  +1 cpmy_229  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_3:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_4:  +1 cpmy_235  +1 cpmy_236  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_5:  +1 cpmy_237  +1 cpmy_238  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_13:  +1 cpmy_239  +1 cpmy_240  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_14:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_15:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_230  <= 1
 c_cpmy_230_1_Apr_16:  +1 cpmy_249  +1 cpmy_250  +1 dr_230  <= 1
 c_cpmy_231_1_May_1:  +1 cpmy_231  -1 dr_231  <= 0
 c_cpmy_231_1_May_2:  +1 cpmy_232  +1 cpmy_233  +1 cpmy_234  +1 dr_231  <= 1
 c_cpmy_231_1_May_3:  +1 cpmy_235  +1 cpmy_236  +1 dr_231  <= 1
 c_cpmy_231_1_May_4:  +1 cpmy_237  +1 cpmy_238  +1 dr_231  <= 1
 c_cpmy_231_1_May_12:  +1 cpmy_239  +1 cpmy_240  +1 dr_231  <= 1
 c_cpmy_231_1_May_13:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_231  <= 1
 c_cpmy_231_1_May_14:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_231  <= 1
 c_cpmy_232_1_May_1:  +1 cpmy_232  -1 dr_232  <= 0
 c_cpmy_232_1_May_2:  +1 cpmy_231  +1 cpmy_233  +1 cpmy_234  +1 dr_232  <= 1
 c_cpmy_232_1_May_3:  +1 cpmy_235  +1 cpmy_236  +1 dr_232  <= 1
 c_cpmy_232_1_May_4:  +1 cpmy_237  +1 cpmy_238  +1 dr_232  <= 1
 c_cpmy_232_1_May_12:  +1 cpmy_239  +1 cpmy_240  +1 dr_232  <= 1
 c_cpmy_232_1_May_13:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_232  <= 1
 c_cpmy_232_1_May_14:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_232  <= 1
 c_cpmy_232_1_May_15:  +1 cpmy_249  +1 cpmy_250  +1 dr_232  <= 1
 c_cpmy_233_1_May_1:  +1 cpmy_233  -1 dr_233  <= 0
 c_cpmy_233_1_May_2:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_234  +1 dr_233  <= 1
 c_cpmy_233_1_May_3:  +1 cpmy_235  +1 cpmy_236  +1 dr_233  <= 1
 c_cpmy_233_1_May_4:  +1 cpmy_237  +1 cpmy_238  +1 dr_233  <= 1
 c_cpmy_233_1_May_12:  +1 cpmy_239  +1 cpmy_240  +1 dr_233  <= 1
 c_cpmy_233_1_May_13:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_233  <= 1
 c_cpmy_233_1_May_14:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_233  <= 1
 c_cpmy_233_1_May_15:  +1 cpmy_249  +1 cpmy_250  +1 dr_233  <= 1
 c_cpmy_233_1_May_16:  +1 cpmy_251  +1 cpmy_252  +1 dr_233  <= 1
 c_cpmy_234_1_May_1:  +1 cpmy_234  -1 dr_234  <= 0
 c_cpmy_234_1_May_2:  +1 cpmy_231  +1 cpmy_232  +1 cpmy_233  +1 dr_234  <= 1
 c_cpmy_234_1_May_3:  +1 cpmy_235  +1 cpmy_236  +1 dr_234  <= 1
 c_cpmy_234_1_May_4:  +1 cpmy_237  +1 cpmy_238  +1 dr_234  <= 1
 c_cpmy_234_1_May_12:  +1 cpmy_239  +1 cpmy_240  +1 dr_234  <= 1
 c_cpmy_234_1_May_13:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_234  <= 1
 c_cpmy_234_1_May_14:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_234  <= 1
 c_cpmy_234_1_May_15:  +1 cpmy_249  +1 cpmy_250  +1 dr_234  <= 1
 c_cpmy_234_1_May_16:  +1 cpmy_251  +1 cpmy_252  +1 dr_234  <= 1
 c_cpmy_235_1_Jun_1:  +1 cpmy_235  -1 dr_235  <= 0
 c_cpmy_235_1_Jun_2:  +1 cpmy_236  +1 dr_235  <= 1
 c_cpmy_235_1_Jun_3:  +1 cpmy_237  +1 cpmy_238  +1 dr_235  <= 1
 c_cpmy_235_1_Jun_11:  +1 cpmy_239  +1 cpmy_240  +1 dr_235  <= 1
 c_cpmy_235_1_Jun_12:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_235  <= 1
 c_cpmy_235_1_Jun_13:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_235  <= 1
 c_cpmy_235_1_Jun_14:  +1 cpmy_249  +1 cpmy_250  +1 dr_235  <= 1
 c_cpmy_235_1_Jun_15:  +1 cpmy_251  +1 cpmy_252  +1 dr_235  <= 1
 c_cpmy_236_1_Jun_1:  +1 cpmy_236  -1 dr_236  <= 0
 c_cpmy_236_1_Jun_2:  +1 cpmy_235  +1 dr_236  <= 1
 c_cpmy_236_1_Jun_3:  +1 cpmy_237  +1 cpmy_238  +1 dr_236  <= 1
 c_cpmy_236_1_Jun_11:  +1 cpmy_239  +1 cpmy_240  +1 dr_236  <= 1
 c_cpmy_236_1_Jun_12:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_236  <= 1
 c_cpmy_236_1_Jun_13:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_236  <= 1
 c_cpmy_236_1_Jun_14:  +1 cpmy_249  +1 cpmy_250  +1 dr_236  <= 1
 c_cpmy_236_1_Jun_15:  +1 cpmy_251  +1 cpmy_252  +1 dr_236  <= 1
 c_cpmy_237_1_Jul_1:  +1 cpmy_237  -1 dr_237  <= 0
 c_cpmy_237_1_Jul_2:  +1 cpmy_238  +1 dr_237  <= 1
 c_cpmy_237_1_Jul_10:  +1 cpmy_239  +1 cpmy_240  +1 dr_237  <= 1
 c_cpmy_237_1_Jul_11:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_237  <= 1
 c_cpmy_237_1_Jul_12:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_237  <= 1
 c_cpmy_237_1_Jul_13:  +1 cpmy_249  +1 cpmy_250  +1 dr_237  <= 1
 c_cpmy_237_1_Jul_14:  +1 cpmy_251  +1 cpmy_252  +1 dr_237  <= 1
 c_cpmy_238_1_Jul_1:  +1 cpmy_238  -1 dr_238  <= 0
 c_cpmy_238_1_Jul_2:  +1 cpmy_237  +1 dr_238  <= 1
 c_cpmy_238_1_Jul_10:  +1 cpmy_239  +1 cpmy_240  +1 dr_238  <= 1
 c_cpmy_238_1_Jul_11:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_238  <= 1
 c_cpmy_238_1_Jul_12:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_238  <= 1
 c_cpmy_238_1_Jul_13:  +1 cpmy_249  +1 cpmy_250  +1 dr_238  <= 1
 c_cpmy_238_1_Jul_14:  +1 cpmy_251  +1 cpmy_252  +1 dr_238  <= 1
 c_cpmy_239_2_Mar_1:  +1 cpmy_239  -1 dr_239  <= 0
 c_cpmy_239_2_Mar_2:  +1 cpmy_240  +1 dr_239  <= 1
 c_cpmy_239_2_Mar_3:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_239  <= 1
 c_cpmy_239_2_Mar_4:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_239  <= 1
 c_cpmy_239_2_Mar_5:  +1 cpmy_249  +1 cpmy_250  +1 dr_239  <= 1
 c_cpmy_239_2_Mar_6:  +1 cpmy_251  +1 cpmy_252  +1 dr_239  <= 1
 c_cpmy_239_2_Mar_14:  +1 cpmy_253  +1 cpmy_254  +1 dr_239  <= 1
 c_cpmy_240_2_Mar_1:  +1 cpmy_240  -1 dr_240  <= 0
 c_cpmy_240_2_Mar_2:  +1 cpmy_239  +1 dr_240  <= 1
 c_cpmy_240_2_Mar_3:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_240  <= 1
 c_cpmy_240_2_Mar_4:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_240  <= 1
 c_cpmy_240_2_Mar_5:  +1 cpmy_249  +1 cpmy_250  +1 dr_240  <= 1
 c_cpmy_240_2_Mar_6:  +1 cpmy_251  +1 cpmy_252  +1 dr_240  <= 1
 c_cpmy_240_2_Mar_14:  +1 cpmy_253  +1 cpmy_254  +1 dr_240  <= 1
 c_cpmy_240_2_Mar_15:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_240  <= 1
 c_cpmy_241_2_Apr_1:  +1 cpmy_241  -1 dr_241  <= 0
 c_cpmy_241_2_Apr_2:  +1 cpmy_242  +1 cpmy_243  +1 cpmy_244  +1 dr_241  <= 1
 c_cpmy_241_2_Apr_3:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_241  <= 1
 c_cpmy_241_2_Apr_4:  +1 cpmy_249  +1 cpmy_250  +1 dr_241  <= 1
 c_cpmy_241_2_Apr_5:  +1 cpmy_251  +1 cpmy_252  +1 dr_241  <= 1
 c_cpmy_241_2_Apr_13:  +1 cpmy_253  +1 cpmy_254  +1 dr_241  <= 1
 c_cpmy_241_2_Apr_14:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_241  <= 1
 c_cpmy_242_2_Apr_1:  +1 cpmy_242  -1 dr_242  <= 0
 c_cpmy_242_2_Apr_2:  +1 cpmy_241  +1 cpmy_243  +1 cpmy_244  +1 dr_242  <= 1
 c_cpmy_242_2_Apr_3:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_242  <= 1
 c_cpmy_242_2_Apr_4:  +1 cpmy_249  +1 cpmy_250  +1 dr_242  <= 1
 c_cpmy_242_2_Apr_5:  +1 cpmy_251  +1 cpmy_252  +1 dr_242  <= 1
 c_cpmy_242_2_Apr_13:  +1 cpmy_253  +1 cpmy_254  +1 dr_242  <= 1
 c_cpmy_242_2_Apr_14:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_242  <= 1
 c_cpmy_242_2_Apr_15:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_242  <= 1
 c_cpmy_243_2_Apr_1:  +1 cpmy_243  -1 dr_243  <= 0
 c_cpmy_243_2_Apr_2:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_244  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_3:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_4:  +1 cpmy_249  +1 cpmy_250  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_5:  +1 cpmy_251  +1 cpmy_252  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_13:  +1 cpmy_253  +1 cpmy_254  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_14:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_15:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_243  <= 1
 c_cpmy_243_2_Apr_16:  +1 cpmy_263  +1 cpmy_264  +1 dr_243  <= 1
 c_cpmy_244_2_Apr_1:  +1 cpmy_244  -1 dr_244  <= 0
 c_cpmy_244_2_Apr_2:  +1 cpmy_241  +1 cpmy_242  +1 cpmy_243  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_3:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_4:  +1 cpmy_249  +1 cpmy_250  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_5:  +1 cpmy_251  +1 cpmy_252  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_13:  +1 cpmy_253  +1 cpmy_254  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_14:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_15:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_244  <= 1
 c_cpmy_244_2_Apr_16:  +1 cpmy_263  +1 cpmy_264  +1 dr_244  <= 1
 c_cpmy_245_2_May_1:  +1 cpmy_245  -1 dr_245  <= 0
 c_cpmy_245_2_May_2:  +1 cpmy_246  +1 cpmy_247  +1 cpmy_248  +1 dr_245  <= 1
 c_cpmy_245_2_May_3:  +1 cpmy_249  +1 cpmy_250  +1 dr_245  <= 1
 c_cpmy_245_2_May_4:  +1 cpmy_251  +1 cpmy_252  +1 dr_245  <= 1
 c_cpmy_245_2_May_12:  +1 cpmy_253  +1 cpmy_254  +1 dr_245  <= 1
 c_cpmy_245_2_May_13:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_245  <= 1
 c_cpmy_245_2_May_14:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_245  <= 1
 c_cpmy_246_2_May_1:  +1 cpmy_246  -1 dr_246  <= 0
 c_cpmy_246_2_May_2:  +1 cpmy_245  +1 cpmy_247  +1 cpmy_248  +1 dr_246  <= 1
 c_cpmy_246_2_May_3:  +1 cpmy_249  +1 cpmy_250  +1 dr_246  <= 1
 c_cpmy_246_2_May_4:  +1 cpmy_251  +1 cpmy_252  +1 dr_246  <= 1
 c_cpmy_246_2_May_12:  +1 cpmy_253  +1 cpmy_254  +1 dr_246  <= 1
 c_cpmy_246_2_May_13:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_246  <= 1
 c_cpmy_246_2_May_14:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_246  <= 1
 c_cpmy_246_2_May_15:  +1 cpmy_263  +1 cpmy_264  +1 dr_246  <= 1
 c_cpmy_247_2_May_1:  +1 cpmy_247  -1 dr_247  <= 0
 c_cpmy_247_2_May_2:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_248  +1 dr_247  <= 1
 c_cpmy_247_2_May_3:  +1 cpmy_249  +1 cpmy_250  +1 dr_247  <= 1
 c_cpmy_247_2_May_4:  +1 cpmy_251  +1 cpmy_252  +1 dr_247  <= 1
 c_cpmy_247_2_May_12:  +1 cpmy_253  +1 cpmy_254  +1 dr_247  <= 1
 c_cpmy_247_2_May_13:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_247  <= 1
 c_cpmy_247_2_May_14:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_247  <= 1
 c_cpmy_247_2_May_15:  +1 cpmy_263  +1 cpmy_264  +1 dr_247  <= 1
 c_cpmy_247_2_May_16:  +1 cpmy_265  +1 cpmy_266  +1 dr_247  <= 1
 c_cpmy_248_2_May_1:  +1 cpmy_248  -1 dr_248  <= 0
 c_cpmy_248_2_May_2:  +1 cpmy_245  +1 cpmy_246  +1 cpmy_247  +1 dr_248  <= 1
 c_cpmy_248_2_May_3:  +1 cpmy_249  +1 cpmy_250  +1 dr_248  <= 1
 c_cpmy_248_2_May_4:  +1 cpmy_251  +1 cpmy_252  +1 dr_248  <= 1
 c_cpmy_248_2_May_12:  +1 cpmy_253  +1 cpmy_254  +1 dr_248  <= 1
 c_cpmy_248_2_May_13:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_248  <= 1
 c_cpmy_248_2_May_14:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_248  <= 1
 c_cpmy_248_2_May_15:  +1 cpmy_263  +1 cpmy_264  +1 dr_248  <= 1
 c_cpmy_248_2_May_16:  +1 cpmy_265  +1 cpmy_266  +1 dr_248  <= 1
 c_cpmy_249_2_Jun_1:  +1 cpmy_249  -1 dr_249  <= 0
 c_cpmy_249_2_Jun_2:  +1 cpmy_250  +1 dr_249  <= 1
 c_cpmy_249_2_Jun_3:  +1 cpmy_251  +1 cpmy_252  +1 dr_249  <= 1
 c_cpmy_249_2_Jun_11:  +1 cpmy_253  +1 cpmy_254  +1 dr_249  <= 1
 c_cpmy_249_2_Jun_12:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_249  <= 1
 c_cpmy_249_2_Jun_13:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_249  <= 1
 c_cpmy_249_2_Jun_14:  +1 cpmy_263  +1 cpmy_264  +1 dr_249  <= 1
 c_cpmy_249_2_Jun_15:  +1 cpmy_265  +1 cpmy_266  +1 dr_249  <= 1
 c_cpmy_250_2_Jun_1:  +1 cpmy_250  -1 dr_250  <= 0
 c_cpmy_250_2_Jun_2:  +1 cpmy_249  +1 dr_250  <= 1
 c_cpmy_250_2_Jun_3:  +1 cpmy_251  +1 cpmy_252  +1 dr_250  <= 1
 c_cpmy_250_2_Jun_11:  +1 cpmy_253  +1 cpmy_254  +1 dr_250  <= 1
 c_cpmy_250_2_Jun_12:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_250  <= 1
 c_cpmy_250_2_Jun_13:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_250  <= 1
 c_cpmy_250_2_Jun_14:  +1 cpmy_263  +1 cpmy_264  +1 dr_250  <= 1
 c_cpmy_250_2_Jun_15:  +1 cpmy_265  +1 cpmy_266  +1 dr_250  <= 1
 c_cpmy_251_2_Jul_1:  +1 cpmy_251  -1 dr_251  <= 0
 c_cpmy_251_2_Jul_2:  +1 cpmy_252  +1 dr_251  <= 1
 c_cpmy_251_2_Jul_10:  +1 cpmy_253  +1 cpmy_254  +1 dr_251  <= 1
 c_cpmy_251_2_Jul_11:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_251  <= 1
 c_cpmy_251_2_Jul_12:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_251  <= 1
 c_cpmy_251_2_Jul_13:  +1 cpmy_263  +1 cpmy_264  +1 dr_251  <= 1
 c_cpmy_251_2_Jul_14:  +1 cpmy_265  +1 cpmy_266  +1 dr_251  <= 1
 c_cpmy_252_2_Jul_1:  +1 cpmy_252  -1 dr_252  <= 0
 c_cpmy_252_2_Jul_2:  +1 cpmy_251  +1 dr_252  <= 1
 c_cpmy_252_2_Jul_10:  +1 cpmy_253  +1 cpmy_254  +1 dr_252  <= 1
 c_cpmy_252_2_Jul_11:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_252  <= 1
 c_cpmy_252_2_Jul_12:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_252  <= 1
 c_cpmy_252_2_Jul_13:  +1 cpmy_263  +1 cpmy_264  +1 dr_252  <= 1
 c_cpmy_252_2_Jul_14:  +1 cpmy_265  +1 cpmy_266  +1 dr_252  <= 1
 c_cpmy_253_3_Mar_1:  +1 cpmy_253  -1 dr_253  <= 0
 c_cpmy_253_3_Mar_2:  +1 cpmy_254  +1 dr_253  <= 1
 c_cpmy_253_3_Mar_3:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_253  <= 1
 c_cpmy_253_3_Mar_4:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_253  <= 1
 c_cpmy_253_3_Mar_5:  +1 cpmy_263  +1 cpmy_264  +1 dr_253  <= 1
 c_cpmy_253_3_Mar_6:  +1 cpmy_265  +1 cpmy_266  +1 dr_253  <= 1
 c_cpmy_253_3_Mar_14:  +1 cpmy_267  +1 cpmy_268  +1 dr_253  <= 1
 c_cpmy_254_3_Mar_1:  +1 cpmy_254  -1 dr_254  <= 0
 c_cpmy_254_3_Mar_2:  +1 cpmy_253  +1 dr_254  <= 1
 c_cpmy_254_3_Mar_3:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_254  <= 1
 c_cpmy_254_3_Mar_4:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_254  <= 1
 c_cpmy_254_3_Mar_5:  +1 cpmy_263  +1 cpmy_264  +1 dr_254  <= 1
 c_cpmy_254_3_Mar_6:  +1 cpmy_265  +1 cpmy_266  +1 dr_254  <= 1
 c_cpmy_254_3_Mar_14:  +1 cpmy_267  +1 cpmy_268  +1 dr_254  <= 1
 c_cpmy_254_3_Mar_15:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_254  <= 1
 c_cpmy_255_3_Apr_1:  +1 cpmy_255  -1 dr_255  <= 0
 c_cpmy_255_3_Apr_2:  +1 cpmy_256  +1 cpmy_257  +1 cpmy_258  +1 dr_255  <= 1
 c_cpmy_255_3_Apr_3:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_255  <= 1
 c_cpmy_255_3_Apr_4:  +1 cpmy_263  +1 cpmy_264  +1 dr_255  <= 1
 c_cpmy_255_3_Apr_5:  +1 cpmy_265  +1 cpmy_266  +1 dr_255  <= 1
 c_cpmy_255_3_Apr_13:  +1 cpmy_267  +1 cpmy_268  +1 dr_255  <= 1
 c_cpmy_255_3_Apr_14:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_255  <= 1
 c_cpmy_256_3_Apr_1:  +1 cpmy_256  -1 dr_256  <= 0
 c_cpmy_256_3_Apr_2:  +1 cpmy_255  +1 cpmy_257  +1 cpmy_258  +1 dr_256  <= 1
 c_cpmy_256_3_Apr_3:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_256  <= 1
 c_cpmy_256_3_Apr_4:  +1 cpmy_263  +1 cpmy_264  +1 dr_256  <= 1
 c_cpmy_256_3_Apr_5:  +1 cpmy_265  +1 cpmy_266  +1 dr_256  <= 1
 c_cpmy_256_3_Apr_13:  +1 cpmy_267  +1 cpmy_268  +1 dr_256  <= 1
 c_cpmy_256_3_Apr_14:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_256  <= 1
 c_cpmy_256_3_Apr_15:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_256  <= 1
 c_cpmy_257_3_Apr_1:  +1 cpmy_257  -1 dr_257  <= 0
 c_cpmy_257_3_Apr_2:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_258  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_3:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_4:  +1 cpmy_263  +1 cpmy_264  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_5:  +1 cpmy_265  +1 cpmy_266  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_13:  +1 cpmy_267  +1 cpmy_268  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_14:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_15:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_257  <= 1
 c_cpmy_257_3_Apr_16:  +1 cpmy_277  +1 cpmy_278  +1 dr_257  <= 1
 c_cpmy_258_3_Apr_1:  +1 cpmy_258  -1 dr_258  <= 0
 c_cpmy_258_3_Apr_2:  +1 cpmy_255  +1 cpmy_256  +1 cpmy_257  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_3:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_4:  +1 cpmy_263  +1 cpmy_264  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_5:  +1 cpmy_265  +1 cpmy_266  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_13:  +1 cpmy_267  +1 cpmy_268  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_14:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_15:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_258  <= 1
 c_cpmy_258_3_Apr_16:  +1 cpmy_277  +1 cpmy_278  +1 dr_258  <= 1
 c_cpmy_259_3_May_1:  +1 cpmy_259  -1 dr_259  <= 0
 c_cpmy_259_3_May_2:  +1 cpmy_260  +1 cpmy_261  +1 cpmy_262  +1 dr_259  <= 1
 c_cpmy_259_3_May_3:  +1 cpmy_263  +1 cpmy_264  +1 dr_259  <= 1
 c_cpmy_259_3_May_4:  +1 cpmy_265  +1 cpmy_266  +1 dr_259  <= 1
 c_cpmy_259_3_May_12:  +1 cpmy_267  +1 cpmy_268  +1 dr_259  <= 1
 c_cpmy_259_3_May_13:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_259  <= 1
 c_cpmy_259_3_May_14:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_259  <= 1
 c_cpmy_260_3_May_1:  +1 cpmy_260  -1 dr_260  <= 0
 c_cpmy_260_3_May_2:  +1 cpmy_259  +1 cpmy_261  +1 cpmy_262  +1 dr_260  <= 1
 c_cpmy_260_3_May_3:  +1 cpmy_263  +1 cpmy_264  +1 dr_260  <= 1
 c_cpmy_260_3_May_4:  +1 cpmy_265  +1 cpmy_266  +1 dr_260  <= 1
 c_cpmy_260_3_May_12:  +1 cpmy_267  +1 cpmy_268  +1 dr_260  <= 1
 c_cpmy_260_3_May_13:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_260  <= 1
 c_cpmy_260_3_May_14:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_260  <= 1
 c_cpmy_260_3_May_15:  +1 cpmy_277  +1 cpmy_278  +1 dr_260  <= 1
 c_cpmy_261_3_May_1:  +1 cpmy_261  -1 dr_261  <= 0
 c_cpmy_261_3_May_2:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_262  +1 dr_261  <= 1
 c_cpmy_261_3_May_3:  +1 cpmy_263  +1 cpmy_264  +1 dr_261  <= 1
 c_cpmy_261_3_May_4:  +1 cpmy_265  +1 cpmy_266  +1 dr_261  <= 1
 c_cpmy_261_3_May_12:  +1 cpmy_267  +1 cpmy_268  +1 dr_261  <= 1
 c_cpmy_261_3_May_13:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_261  <= 1
 c_cpmy_261_3_May_14:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_261  <= 1
 c_cpmy_261_3_May_15:  +1 cpmy_277  +1 cpmy_278  +1 dr_261  <= 1
 c_cpmy_261_3_May_16:  +1 cpmy_279  +1 cpmy_280  +1 dr_261  <= 1
 c_cpmy_262_3_May_1:  +1 cpmy_262  -1 dr_262  <= 0
 c_cpmy_262_3_May_2:  +1 cpmy_259  +1 cpmy_260  +1 cpmy_261  +1 dr_262  <= 1
 c_cpmy_262_3_May_3:  +1 cpmy_263  +1 cpmy_264  +1 dr_262  <= 1
 c_cpmy_262_3_May_4:  +1 cpmy_265  +1 cpmy_266  +1 dr_262  <= 1
 c_cpmy_262_3_May_12:  +1 cpmy_267  +1 cpmy_268  +1 dr_262  <= 1
 c_cpmy_262_3_May_13:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_262  <= 1
 c_cpmy_262_3_May_14:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_262  <= 1
 c_cpmy_262_3_May_15:  +1 cpmy_277  +1 cpmy_278  +1 dr_262  <= 1
 c_cpmy_262_3_May_16:  +1 cpmy_279  +1 cpmy_280  +1 dr_262  <= 1
 c_cpmy_263_3_Jun_1:  +1 cpmy_263  -1 dr_263  <= 0
 c_cpmy_263_3_Jun_2:  +1 cpmy_264  +1 dr_263  <= 1
 c_cpmy_263_3_Jun_3:  +1 cpmy_265  +1 cpmy_266  +1 dr_263  <= 1
 c_cpmy_263_3_Jun_11:  +1 cpmy_267  +1 cpmy_268  +1 dr_263  <= 1
 c_cpmy_263_3_Jun_12:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_263  <= 1
 c_cpmy_263_3_Jun_13:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_263  <= 1
 c_cpmy_263_3_Jun_14:  +1 cpmy_277  +1 cpmy_278  +1 dr_263  <= 1
 c_cpmy_263_3_Jun_15:  +1 cpmy_279  +1 cpmy_280  +1 dr_263  <= 1
 c_cpmy_264_3_Jun_1:  +1 cpmy_264  -1 dr_264  <= 0
 c_cpmy_264_3_Jun_2:  +1 cpmy_263  +1 dr_264  <= 1
 c_cpmy_264_3_Jun_3:  +1 cpmy_265  +1 cpmy_266  +1 dr_264  <= 1
 c_cpmy_264_3_Jun_11:  +1 cpmy_267  +1 cpmy_268  +1 dr_264  <= 1
 c_cpmy_264_3_Jun_12:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_264  <= 1
 c_cpmy_264_3_Jun_13:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_264  <= 1
 c_cpmy_264_3_Jun_14:  +1 cpmy_277  +1 cpmy_278  +1 dr_264  <= 1
 c_cpmy_264_3_Jun_15:  +1 cpmy_279  +1 cpmy_280  +1 dr_264  <= 1
 c_cpmy_265_3_Jul_1:  +1 cpmy_265  -1 dr_265  <= 0
 c_cpmy_265_3_Jul_2:  +1 cpmy_266  +1 dr_265  <= 1
 c_cpmy_265_3_Jul_10:  +1 cpmy_267  +1 cpmy_268  +1 dr_265  <= 1
 c_cpmy_265_3_Jul_11:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_265  <= 1
 c_cpmy_265_3_Jul_12:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_265  <= 1
 c_cpmy_265_3_Jul_13:  +1 cpmy_277  +1 cpmy_278  +1 dr_265  <= 1
 c_cpmy_265_3_Jul_14:  +1 cpmy_279  +1 cpmy_280  +1 dr_265  <= 1
 c_cpmy_266_3_Jul_1:  +1 cpmy_266  -1 dr_266  <= 0
 c_cpmy_266_3_Jul_2:  +1 cpmy_265  +1 dr_266  <= 1
 c_cpmy_266_3_Jul_10:  +1 cpmy_267  +1 cpmy_268  +1 dr_266  <= 1
 c_cpmy_266_3_Jul_11:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_266  <= 1
 c_cpmy_266_3_Jul_12:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_266  <= 1
 c_cpmy_266_3_Jul_13:  +1 cpmy_277  +1 cpmy_278  +1 dr_266  <= 1
 c_cpmy_266_3_Jul_14:  +1 cpmy_279  +1 cpmy_280  +1 dr_266  <= 1
 c_cpmy_267_4_Mar_1:  +1 cpmy_267  -1 dr_267  <= 0
 c_cpmy_267_4_Mar_2:  +1 cpmy_268  +1 dr_267  <= 1
 c_cpmy_267_4_Mar_3:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_267  <= 1
 c_cpmy_267_4_Mar_4:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_267  <= 1
 c_cpmy_267_4_Mar_5:  +1 cpmy_277  +1 cpmy_278  +1 dr_267  <= 1
 c_cpmy_267_4_Mar_6:  +1 cpmy_279  +1 cpmy_280  +1 dr_267  <= 1
 c_cpmy_268_4_Mar_1:  +1 cpmy_268  -1 dr_268  <= 0
 c_cpmy_268_4_Mar_2:  +1 cpmy_267  +1 dr_268  <= 1
 c_cpmy_268_4_Mar_3:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_268  <= 1
 c_cpmy_268_4_Mar_4:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_268  <= 1
 c_cpmy_268_4_Mar_5:  +1 cpmy_277  +1 cpmy_278  +1 dr_268  <= 1
 c_cpmy_268_4_Mar_6:  +1 cpmy_279  +1 cpmy_280  +1 dr_268  <= 1
 c_cpmy_269_4_Apr_1:  +1 cpmy_269  -1 dr_269  <= 0
 c_cpmy_269_4_Apr_2:  +1 cpmy_270  +1 cpmy_271  +1 cpmy_272  +1 dr_269  <= 1
 c_cpmy_269_4_Apr_3:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_269  <= 1
 c_cpmy_269_4_Apr_4:  +1 cpmy_277  +1 cpmy_278  +1 dr_269  <= 1
 c_cpmy_269_4_Apr_5:  +1 cpmy_279  +1 cpmy_280  +1 dr_269  <= 1
 c_cpmy_270_4_Apr_1:  +1 cpmy_270  -1 dr_270  <= 0
 c_cpmy_270_4_Apr_2:  +1 cpmy_269  +1 cpmy_271  +1 cpmy_272  +1 dr_270  <= 1
 c_cpmy_270_4_Apr_3:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_270  <= 1
 c_cpmy_270_4_Apr_4:  +1 cpmy_277  +1 cpmy_278  +1 dr_270  <= 1
 c_cpmy_270_4_Apr_5:  +1 cpmy_279  +1 cpmy_280  +1 dr_270  <= 1
 c_cpmy_271_4_Apr_1:  +1 cpmy_271  -1 dr_271  <= 0
 c_cpmy_271_4_Apr_2:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_272  +1 dr_271  <= 1
 c_cpmy_271_4_Apr_3:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_271  <= 1
 c_cpmy_271_4_Apr_4:  +1 cpmy_277  +1 cpmy_278  +1 dr_271  <= 1
 c_cpmy_271_4_Apr_5:  +1 cpmy_279  +1 cpmy_280  +1 dr_271  <= 1
 c_cpmy_272_4_Apr_1:  +1 cpmy_272  -1 dr_272  <= 0
 c_cpmy_272_4_Apr_2:  +1 cpmy_269  +1 cpmy_270  +1 cpmy_271  +1 dr_272  <= 1
 c_cpmy_272_4_Apr_3:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_272  <= 1
 c_cpmy_272_4_Apr_4:  +1 cpmy_277  +1 cpmy_278  +1 dr_272  <= 1
 c_cpmy_272_4_Apr_5:  +1 cpmy_279  +1 cpmy_280  +1 dr_272  <= 1
 c_cpmy_273_4_May_1:  +1 cpmy_273  -1 dr_273  <= 0
 c_cpmy_273_4_May_2:  +1 cpmy_274  +1 cpmy_275  +1 cpmy_276  +1 dr_273  <= 1
 c_cpmy_273_4_May_3:  +1 cpmy_277  +1 cpmy_278  +1 dr_273  <= 1
 c_cpmy_273_4_May_4:  +1 cpmy_279  +1 cpmy_280  +1 dr_273  <= 1
 c_cpmy_274_4_May_1:  +1 cpmy_274  -1 dr_274  <= 0
 c_cpmy_274_4_May_2:  +1 cpmy_273  +1 cpmy_275  +1 cpmy_276  +1 dr_274  <= 1
 c_cpmy_274_4_May_3:  +1 cpmy_277  +1 cpmy_278  +1 dr_274  <= 1
 c_cpmy_274_4_May_4:  +1 cpmy_279  +1 cpmy_280  +1 dr_274  <= 1
 c_cpmy_275_4_May_1:  +1 cpmy_275  -1 dr_275  <= 0
 c_cpmy_275_4_May_2:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_276  +1 dr_275  <= 1
 c_cpmy_275_4_May_3:  +1 cpmy_277  +1 cpmy_278  +1 dr_275  <= 1
 c_cpmy_275_4_May_4:  +1 cpmy_279  +1 cpmy_280  +1 dr_275  <= 1
 c_cpmy_276_4_May_1:  +1 cpmy_276  -1 dr_276  <= 0
 c_cpmy_276_4_May_2:  +1 cpmy_273  +1 cpmy_274  +1 cpmy_275  +1 dr_276  <= 1
 c_cpmy_276_4_May_3:  +1 cpmy_277  +1 cpmy_278  +1 dr_276  <= 1
 c_cpmy_276_4_May_4:  +1 cpmy_279  +1 cpmy_280  +1 dr_276  <= 1
 c_cpmy_277_4_Jun_1:  +1 cpmy_277  -1 dr_277  <= 0
 c_cpmy_277_4_Jun_2:  +1 cpmy_278  +1 dr_277  <= 1
 c_cpmy_277_4_Jun_3:  +1 cpmy_279  +1 cpmy_280  +1 dr_277  <= 1
 c_cpmy_278_4_Jun_1:  +1 cpmy_278  -1 dr_278  <= 0
 c_cpmy_278_4_Jun_2:  +1 cpmy_277  +1 dr_278  <= 1
 c_cpmy_278_4_Jun_3:  +1 cpmy_279  +1 cpmy_280  +1 dr_278  <= 1
 c_cpmy_279_4_Jul_1:  +1 cpmy_279  -1 dr_279  <= 0
 c_cpmy_279_4_Jul_2:  +1 cpmy_280  +1 dr_279  <= 1
 c_cpmy_280_4_Jul_1:  +1 cpmy_280  -1 dr_280  <= 0
 c_cpmy_280_4_Jul_2:  +1 cpmy_279  +1 dr_280  <= 1

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
 cpmy_49
 cpmy_50
 cpmy_51
 cpmy_52
 cpmy_53
 cpmy_54
 cpmy_55
 cpmy_56
 cpmy_57
 cpmy_58
 cpmy_59
 cpmy_60
 cpmy_61
 cpmy_62
 cpmy_63
 cpmy_64
 cpmy_65
 cpmy_66
 cpmy_67
 cpmy_68
 cpmy_69
 cpmy_70
 cpmy_71
 cpmy_72
 cpmy_73
 cpmy_74
 cpmy_75
 cpmy_76
 cpmy_77
 cpmy_78
 cpmy_79
 cpmy_80
 cpmy_81
 cpmy_82
 cpmy_83
 cpmy_84
 cpmy_85
 cpmy_86
 cpmy_87
 cpmy_88
 cpmy_89
 cpmy_90
 cpmy_91
 cpmy_92
 cpmy_93
 cpmy_94
 cpmy_95
 cpmy_96
 cpmy_97
 cpmy_98
 cpmy_99
 cpmy_100
 cpmy_101
 cpmy_102
 cpmy_103
 cpmy_104
 cpmy_105
 cpmy_106
 cpmy_107
 cpmy_108
 cpmy_109
 cpmy_110
 cpmy_111
 cpmy_112
 cpmy_113
 cpmy_114
 cpmy_115
 cpmy_116
 cpmy_117
 cpmy_118
 cpmy_119
 cpmy_120
 cpmy_121
 cpmy_122
 cpmy_123
 cpmy_124
 cpmy_125
 cpmy_126
 cpmy_127
 cpmy_128
 cpmy_129
 cpmy_130
 cpmy_131
 cpmy_132
 cpmy_133
 cpmy_134
 cpmy_135
 cpmy_136
 cpmy_137
 cpmy_138
 cpmy_139
 cpmy_140
 cpmy_141
 cpmy_142
 cpmy_143
 cpmy_144
 cpmy_145
 cpmy_146
 cpmy_147
 cpmy_148
 cpmy_149
 cpmy_150
 cpmy_151
 cpmy_152
 cpmy_153
 cpmy_154
 cpmy_155
 cpmy_156
 cpmy_157
 cpmy_158
 cpmy_159
 cpmy_160
 cpmy_161
 cpmy_162
 cpmy_163
 cpmy_164
 cpmy_165
 cpmy_166
 cpmy_167
 cpmy_168
 cpmy_169
 cpmy_170
 cpmy_171
 cpmy_172
 cpmy_173
 cpmy_174
 cpmy_175
 cpmy_176
 cpmy_177
 cpmy_178
 cpmy_179
 cpmy_180
 cpmy_181
 cpmy_182
 cpmy_183
 cpmy_184
 cpmy_185
 cpmy_186
 cpmy_187
 cpmy_188
 cpmy_189
 cpmy_190
 cpmy_191
 cpmy_192
 cpmy_193
 cpmy_194
 cpmy_195
 cpmy_196
 cpmy_197
 cpmy_198
 cpmy_199
 cpmy_200
 cpmy_201
 cpmy_202
 cpmy_203
 cpmy_204
 cpmy_205
 cpmy_206
 cpmy_207
 cpmy_208
 cpmy_209
 cpmy_210
 cpmy_211
 cpmy_212
 cpmy_213
 cpmy_214
 cpmy_215
 cpmy_216
 cpmy_217
 cpmy_218
 cpmy_219
 cpmy_220
 cpmy_221
 cpmy_222
 cpmy_223
 cpmy_224
 cpmy_225
 cpmy_226
 cpmy_227
 cpmy_228
 cpmy_229
 cpmy_230
 cpmy_231
 cpmy_232
 cpmy_233
 cpmy_234
 cpmy_235
 cpmy_236
 cpmy_237
 cpmy_238
 cpmy_239
 cpmy_240
 cpmy_241
 cpmy_242
 cpmy_243
 cpmy_244
 cpmy_245
 cpmy_246
 cpmy_247
 cpmy_248
 cpmy_249
 cpmy_250
 cpmy_251
 cpmy_252
 cpmy_253
 cpmy_254
 cpmy_255
 cpmy_256
 cpmy_257
 cpmy_258
 cpmy_259
 cpmy_260
 cpmy_261
 cpmy_262
 cpmy_263
 cpmy_264
 cpmy_265
 cpmy_266
 cpmy_267
 cpmy_268
 cpmy_269
 cpmy_270
 cpmy_271
 cpmy_272
 cpmy_273
 cpmy_274
 cpmy_275
 cpmy_276
 cpmy_277
 cpmy_278
 cpmy_279
 cpmy_280
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
 uf_97
 uf_98
 uf_99
 uf_100
 uf_101
 uf_102
 uf_103
 uf_104
 uf_105
 uf_106
 uf_107
 uf_108
 uf_109
 uf_110
 uf_111
 uf_112
 uf_113
 uf_114
 uf_115
 uf_116
 uf_117
 uf_118
 uf_119
 uf_120
 uf_121
 uf_122
 uf_123
 uf_124
 uf_125
 uf_126
 uf_127
 uf_128
 uf_129
 uf_130
 uf_131
 uf_132
 uf_133
 uf_134
 uf_135
 uf_136
 uf_137
 uf_138
 uf_139
 uf_140
 uf_141
 uf_142
 uf_143
 uf_144
 uf_145
 uf_146
 uf_147
 uf_148
 uf_149
 uf_150
 uf_151
 uf_152
 uf_153
 uf_154
 uf_155
 uf_156
 uf_157
 uf_158
 uf_159
 uf_160
 uf_161
 uf_162
 uf_163
 uf_164
 uf_165
 uf_166
 uf_167
 uf_168
 uf_169
 uf_170
 uf_171
 uf_172
 uf_173
 uf_174
 uf_175
 uf_176
 uf_177
 uf_178
 uf_179
 uf_180
 uf_181
 uf_182
 uf_183
 uf_184
 uf_185
 uf_186
 uf_187
 uf_188
 uf_189
 uf_190
 uf_191
 uf_192
 uf_193
 uf_194
 uf_195
 uf_196
 uf_197
 uf_198
 uf_199
 uf_200
 uf_201
 uf_202
 uf_203
 uf_204
 uf_205
 uf_206
 uf_207
 uf_208
 uf_209
 uf_210
 uf_211
 uf_212
 uf_213
 uf_214
 uf_215
 uf_216
 uf_217
 uf_218
 uf_219
 uf_220
 uf_221
 uf_222
 uf_223
 uf_224
 uf_225
 uf_226
 uf_227
 uf_228
 uf_229
 uf_230
 uf_231
 uf_232
 uf_233
 uf_234
 uf_235
 uf_236
 uf_237
 uf_238
 uf_239
 uf_240
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
 dr_17
 dr_18
 dr_19
 dr_20
 dr_21
 dr_22
 dr_23
 dr_24
 dr_25
 dr_26
 dr_27
 dr_28
 dr_29
 dr_30
 dr_31
 dr_32
 dr_33
 dr_34
 dr_35
 dr_36
 dr_37
 dr_38
 dr_39
 dr_40
 dr_41
 dr_42
 dr_43
 dr_44
 dr_45
 dr_46
 dr_47
 dr_48
 dr_49
 dr_50
 dr_51
 dr_52
 dr_53
 dr_54
 dr_55
 dr_56
 dr_57
 dr_58
 dr_59
 dr_60
 dr_61
 dr_62
 dr_63
 dr_64
 dr_65
 dr_66
 dr_67
 dr_68
 dr_69
 dr_70
 dr_71
 dr_72
 dr_73
 dr_74
 dr_75
 dr_76
 dr_77
 dr_78
 dr_79
 dr_80
 dr_81
 dr_82
 dr_83
 dr_84
 dr_85
 dr_86
 dr_87
 dr_88
 dr_89
 dr_90
 dr_91
 dr_92
 dr_93
 dr_94
 dr_95
 dr_96
 dr_97
 dr_98
 dr_99
 dr_100
 dr_101
 dr_102
 dr_103
 dr_104
 dr_105
 dr_106
 dr_107
 dr_108
 dr_109
 dr_110
 dr_111
 dr_112
 dr_113
 dr_114
 dr_115
 dr_116
 dr_117
 dr_118
 dr_119
 dr_120
 dr_121
 dr_122
 dr_123
 dr_124
 dr_125
 dr_126
 dr_127
 dr_128
 dr_129
 dr_130
 dr_131
 dr_132
 dr_133
 dr_134
 dr_135
 dr_136
 dr_137
 dr_138
 dr_139
 dr_140
 dr_141
 dr_142
 dr_143
 dr_144
 dr_145
 dr_146
 dr_147
 dr_148
 dr_149
 dr_150
 dr_151
 dr_152
 dr_153
 dr_154
 dr_155
 dr_156
 dr_157
 dr_158
 dr_159
 dr_160
 dr_161
 dr_162
 dr_163
 dr_164
 dr_165
 dr_166
 dr_167
 dr_168
 dr_169
 dr_170
 dr_171
 dr_172
 dr_173
 dr_174
 dr_175
 dr_176
 dr_177
 dr_178
 dr_179
 dr_180
 dr_181
 dr_182
 dr_183
 dr_184
 dr_185
 dr_186
 dr_187
 dr_188
 dr_189
 dr_190
 dr_191
 dr_192
 dr_193
 dr_194
 dr_195
 dr_196
 dr_197
 dr_198
 dr_199
 dr_200
 dr_201
 dr_202
 dr_203
 dr_204
 dr_205
 dr_206
 dr_207
 dr_208
 dr_209
 dr_210
 dr_211
 dr_212
 dr_213
 dr_214
 dr_215
 dr_216
 dr_217
 dr_218
 dr_219
 dr_220
 dr_221
 dr_222
 dr_223
 dr_224
 dr_225
 dr_226
 dr_227
 dr_228
 dr_229
 dr_230
 dr_231
 dr_232
 dr_233
 dr_234
 dr_235
 dr_236
 dr_237
 dr_238
 dr_239
 dr_240
 dr_241
 dr_242
 dr_243
 dr_244
 dr_245
 dr_246
 dr_247
 dr_248
 dr_249
 dr_250
 dr_251
 dr_252
 dr_253
 dr_254
 dr_255
 dr_256
 dr_257
 dr_258
 dr_259
 dr_260
 dr_261
 dr_262
 dr_263
 dr_264
 dr_265
 dr_266
 dr_267
 dr_268
 dr_269
 dr_270
 dr_271
 dr_272
 dr_273
 dr_274
 dr_275
 dr_276
 dr_277
 dr_278
 dr_279
 dr_280

general
 ud_1
 ud_2
 ud_3
 ud_4
 ud_5
 ud_6
 ud_7
 ud_8
 ud_9
 ud_10
 ud_11
 ud_12
 ud_13
 ud_14
 ud_15
 ud_16

end

