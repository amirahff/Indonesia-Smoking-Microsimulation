###################
### CALIBRATION ###
###################

library(tidyverse)

load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result_M_18.24.Rda')
load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result_M_25.44.Rda')
load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result_M_45.64.Rda')
load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result_M_65plus.Rda')
load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result_F_65plus.Rda')
load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result_ori.Rda')
load('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/final_result.Rda')

calibration_result = data.frame()
calibration_result = rbind(calibration_result,final_result_ori)
calibration_result = rbind(calibration_result,final_result_M_18.24)
calibration_result = rbind(calibration_result,final_result_M_25.44)
calibration_result = rbind(calibration_result,final_result_M_45.64)
calibration_result = rbind(calibration_result,final_result_M_65plus)
calibration_result = rbind(calibration_result,final_result_F_65plus)
calibration_result = rbind(calibration_result,final_result)

calibration_result2 = calibration_result %>%
  filter(!which_res == 'all')

###########
### Ori ###
###########

load('p_11.Rda')
load('p_12.Rda')
load('p_21.Rda')
load('p_22.Rda')
load('p_31.Rda')
load('p_33.Rda')
load('p_dead.Rda')

#########################
### Final Calibration ###
#########################

p_33[5] = p_33[5] - (0.015 - p_31[5])
p_31[5] = 0.015

p_11[5] = p_11[5] - (0.01 - p_12[5])
p_12[5] = 0.01

p_33[7] = p_33[7] - (0.15 - p_31[7])
p_31[7] = 0.15

p_11[7] = p_11[7] - (0.0020 - p_12[7])
p_12[7] = 0.0015

p_33[8] = p_33[8] - (0.01 - p_31[8])
p_31[8] = 0.01

p_11[8] = p_11[8] - (0.0020 - p_12[8])
p_12[8] = 0.0020

p_33[9] = p_33[9] - (0.01 - p_31[9])
p_31[9] = 0.01

p_11[9] = p_11[9] - (0.0050 - p_12[9])
p_12[9] = 0.0050

p_33[10] = p_11[10] - (0.05 - p_12[10])
p_31[10] = 0.05

p_11[10] = p_11[10] - (0.015 - p_12[10])
p_12[10] = 0.015