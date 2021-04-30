\* Crop Rotation 2021 *\

minimize
Z:
 + 1000 ud_1_Be_1 + 1000 ud_2_Be_2 + 1000 ud_3_Be_3 + 1000 ud_4_Be_4

Subject to

 c_uf_1_1_1_Jan: + 1 uf_1_1_1_Jan  <= 1
 c_uf_2_1_2_Jan: + 1 uf_2_1_2_Jan - 1 uf_45_1_1_Dec  <= 0
 c_uf_3_1_3_Jan: + 1 uf_3_1_3_Jan - 1 uf_46_1_2_Dec  <= 0
 c_uf_4_1_4_Jan: + 1 uf_4_1_4_Jan - 1 uf_47_1_3_Dec  <= 0
 c_uf_5_1_1_Feb: - 1 uf_1_1_1_Jan + 1 uf_5_1_1_Feb  <= 0
 c_uf_6_1_2_Feb: - 1 uf_2_1_2_Jan + 1 uf_6_1_2_Feb  <= 0
 c_uf_7_1_3_Feb: - 1 uf_3_1_3_Jan + 1 uf_7_1_3_Feb  <= 0
 c_uf_8_1_4_Feb: - 1 uf_4_1_4_Jan + 1 uf_8_1_4_Feb  <= 0
 c_uf_9_1_1_Mar: - 1 uf_5_1_1_Feb + 1 uf_9_1_1_Mar  <= 0
 c_uf_10_1_2_Mar: - 1 uf_6_1_2_Feb + 1 uf_10_1_2_Mar  <= 0
 c_uf_11_1_3_Mar: - 1 uf_7_1_3_Feb + 1 uf_11_1_3_Mar  <= 0
 c_uf_12_1_4_Mar: - 1 uf_8_1_4_Feb + 1 uf_12_1_4_Mar  <= 0
 c_uf_13_1_1_Apr: + 1 cpmy_1_Be_1_1_Apr - 1 uf_9_1_1_Mar + 1 uf_13_1_1_Apr  <= 0
 c_uf_14_1_2_Apr: + 1 cpmy_3_Be_1_2_Apr - 1 uf_10_1_2_Mar + 1 uf_14_1_2_Apr  <= 0
 c_uf_15_1_3_Apr: + 1 cpmy_5_Be_1_3_Apr - 1 uf_11_1_3_Mar + 1 uf_15_1_3_Apr  <= 0
 c_uf_16_1_4_Apr: + 1 cpmy_7_Be_1_4_Apr - 1 uf_12_1_4_Mar + 1 uf_16_1_4_Apr  <= 0
 c_uf_17_1_1_May: + 1 cpmy_2_Be_1_1_May - 1 uf_13_1_1_Apr + 1 uf_17_1_1_May  <= 0
 c_uf_18_1_2_May: + 1 cpmy_4_Be_1_2_May - 1 uf_14_1_2_Apr + 1 uf_18_1_2_May  <= 0
 c_uf_19_1_3_May: + 1 cpmy_6_Be_1_3_May - 1 uf_15_1_3_Apr + 1 uf_19_1_3_May  <= 0
 c_uf_20_1_4_May: + 1 cpmy_8_Be_1_4_May - 1 uf_16_1_4_Apr + 1 uf_20_1_4_May  <= 0
 c_uf_21_1_1_Jun: - 1 cpmy_1_Be_1_1_Apr - 1 uf_17_1_1_May + 1 uf_21_1_1_Jun  <= 0
 c_uf_22_1_2_Jun: - 1 cpmy_3_Be_1_2_Apr - 1 uf_18_1_2_May + 1 uf_22_1_2_Jun  <= 0
 c_uf_23_1_3_Jun: - 1 cpmy_5_Be_1_3_Apr - 1 uf_19_1_3_May + 1 uf_23_1_3_Jun  <= 0
 c_uf_24_1_4_Jun: - 1 cpmy_7_Be_1_4_Apr - 1 uf_20_1_4_May + 1 uf_24_1_4_Jun  <= 0
 c_uf_25_1_1_Jul: - 1 cpmy_2_Be_1_1_May - 1 uf_21_1_1_Jun + 1 uf_25_1_1_Jul  <= 0
 c_uf_26_1_2_Jul: - 1 cpmy_4_Be_1_2_May - 1 uf_22_1_2_Jun + 1 uf_26_1_2_Jul  <= 0
 c_uf_27_1_3_Jul: - 1 cpmy_6_Be_1_3_May - 1 uf_23_1_3_Jun + 1 uf_27_1_3_Jul  <= 0
 c_uf_28_1_4_Jul: - 1 cpmy_8_Be_1_4_May - 1 uf_24_1_4_Jun + 1 uf_28_1_4_Jul  <= 0
 c_uf_29_1_1_Aug: - 1 uf_25_1_1_Jul + 1 uf_29_1_1_Aug  <= 0
 c_uf_30_1_2_Aug: - 1 uf_26_1_2_Jul + 1 uf_30_1_2_Aug  <= 0
 c_uf_31_1_3_Aug: - 1 uf_27_1_3_Jul + 1 uf_31_1_3_Aug  <= 0
 c_uf_32_1_4_Aug: - 1 uf_28_1_4_Jul + 1 uf_32_1_4_Aug  <= 0
 c_uf_33_1_1_Sep: - 1 uf_29_1_1_Aug + 1 uf_33_1_1_Sep  <= 0
 c_uf_34_1_2_Sep: - 1 uf_30_1_2_Aug + 1 uf_34_1_2_Sep  <= 0
 c_uf_35_1_3_Sep: - 1 uf_31_1_3_Aug + 1 uf_35_1_3_Sep  <= 0
 c_uf_36_1_4_Sep: - 1 uf_32_1_4_Aug + 1 uf_36_1_4_Sep  <= 0
 c_uf_37_1_1_Oct: - 1 uf_33_1_1_Sep + 1 uf_37_1_1_Oct  <= 0
 c_uf_38_1_2_Oct: - 1 uf_34_1_2_Sep + 1 uf_38_1_2_Oct  <= 0
 c_uf_39_1_3_Oct: - 1 uf_35_1_3_Sep + 1 uf_39_1_3_Oct  <= 0
 c_uf_40_1_4_Oct: - 1 uf_36_1_4_Sep + 1 uf_40_1_4_Oct  <= 0
 c_uf_41_1_1_Nov: - 1 uf_37_1_1_Oct + 1 uf_41_1_1_Nov  <= 0
 c_uf_42_1_2_Nov: - 1 uf_38_1_2_Oct + 1 uf_42_1_2_Nov  <= 0
 c_uf_43_1_3_Nov: - 1 uf_39_1_3_Oct + 1 uf_43_1_3_Nov  <= 0
 c_uf_44_1_4_Nov: - 1 uf_40_1_4_Oct + 1 uf_44_1_4_Nov  <= 0
 c_uf_45_1_1_Dec: - 1 uf_41_1_1_Nov + 1 uf_45_1_1_Dec  <= 0
 c_uf_46_1_2_Dec: - 1 uf_42_1_2_Nov + 1 uf_46_1_2_Dec  <= 0
 c_uf_47_1_3_Dec: - 1 uf_43_1_3_Nov + 1 uf_47_1_3_Dec  <= 0
 c_uf_48_1_4_Dec: - 1 uf_44_1_4_Nov + 1 uf_48_1_4_Dec  <= 0

 c_ud_1_Be_1: + 18.75 cpmy_1_Be_1_1_Apr + 18.75 cpmy_2_Be_1_1_May -1 ud_1_Be_1  >= 30
 c_ud_2_Be_2: + 18.75 cpmy_3_Be_1_2_Apr + 18.75 cpmy_4_Be_1_2_May -1 ud_2_Be_2  >= 30
 c_ud_3_Be_3: + 18.75 cpmy_5_Be_1_3_Apr + 18.75 cpmy_6_Be_1_3_May -1 ud_3_Be_3  >= 30
 c_ud_4_Be_4: + 18.75 cpmy_7_Be_1_4_Apr + 18.75 cpmy_8_Be_1_4_May -1 ud_4_Be_4  >= 30

binary
 cpmy_1_Be_1_1_Apr
 cpmy_2_Be_1_1_May
 cpmy_3_Be_1_2_Apr
 cpmy_4_Be_1_2_May
 cpmy_5_Be_1_3_Apr
 cpmy_6_Be_1_3_May
 cpmy_7_Be_1_4_Apr
 cpmy_8_Be_1_4_May
 uf_1_1_1_Jan
 uf_2_1_2_Jan
 uf_3_1_3_Jan
 uf_4_1_4_Jan
 uf_5_1_1_Feb
 uf_6_1_2_Feb
 uf_7_1_3_Feb
 uf_8_1_4_Feb
 uf_9_1_1_Mar
 uf_10_1_2_Mar
 uf_11_1_3_Mar
 uf_12_1_4_Mar
 uf_13_1_1_Apr
 uf_14_1_2_Apr
 uf_15_1_3_Apr
 uf_16_1_4_Apr
 uf_17_1_1_May
 uf_18_1_2_May
 uf_19_1_3_May
 uf_20_1_4_May
 uf_21_1_1_Jun
 uf_22_1_2_Jun
 uf_23_1_3_Jun
 uf_24_1_4_Jun
 uf_25_1_1_Jul
 uf_26_1_2_Jul
 uf_27_1_3_Jul
 uf_28_1_4_Jul
 uf_29_1_1_Aug
 uf_30_1_2_Aug
 uf_31_1_3_Aug
 uf_32_1_4_Aug
 uf_33_1_1_Sep
 uf_34_1_2_Sep
 uf_35_1_3_Sep
 uf_36_1_4_Sep
 uf_37_1_1_Oct
 uf_38_1_2_Oct
 uf_39_1_3_Oct
 uf_40_1_4_Oct
 uf_41_1_1_Nov
 uf_42_1_2_Nov
 uf_43_1_3_Nov
 uf_44_1_4_Nov
 uf_45_1_1_Dec
 uf_46_1_2_Dec
 uf_47_1_3_Dec
 uf_48_1_4_Dec

general
 ud_1_Be_1
 ud_2_Be_2
 ud_3_Be_3
 ud_4_Be_4

end
