library(dplyr)

load('ifls.Rda')
load('ifls_plot.Rda')

cont.table.ori = table(ifls_plot$smoking_state,ifls_plot$age_group,ifls_plot$sex)
r0 = prop.table(cont.table.ori, margin = c(2,3)) 

load('p_11.Rda')
load('p_12.Rda')
load('p_21.Rda')
load('p_22.Rda')
load('p_31.Rda')
load('p_33.Rda')
load('p_dead.Rda')

p_11_ori = p_11; p_12_ori = p_12
p_21_ori = p_21; p_22_ori = p_22
p_31_ori = p_31; p_33_ori = p_33

load('microsim.Rda')
load('get_mape.Rda')


###################
### CALIBRATION ###
###################

final_result_M_65plus = data.frame()

# Calibration p15 increase p_31 to 0.05

p_33[10] = p_33[10] - (0.05 - p_31[10])
p_31[10] = 0.05

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_31 0.05'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_31 0.05'
                                         , which_res = 'M_65plus'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p15 increase p_31 to 0.10

p_33[10] = p_33[10] - (0.10 - p_31[10])
p_31[10] = 0.10

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_31 0.1'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_31 0.1'
                                         , which_res = 'M_65plus'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p15 increase p_31 to 0.15

p_33[10] = p_33[10] - (0.15 - p_31[10])
p_31[10] = 0.15

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_31 0.15'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_31 0.15'
                                         , which_res = 'M_65plus'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p12 increase p_12 to 0.03

p_11[10] = p_11[10] - (0.03 - p_12[10])
p_12[10] = 0.03

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_12 0.03'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_12 0.03'
                                         , which_res = 'M_65plus'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

# Calibration p12 increase p_12 to 0.015

p_11[10] = p_11[10] - (0.015 - p_12[10])
p_12[10] = 0.015

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_12 0.015'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_12 0.015'
                                         , which_res = 'M_65plus'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

# Calibration p12 increase p_12 to 0.01

p_11[10] = p_11[10] - (0.01 - p_12[10])
p_12[10] = 0.01

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_12 0.01'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_65plus = rbind(final_result_M_65plus
                             ,data.frame(category = 'M_65plus p_12 0.01'
                                         , which_res = 'M_65plus'
                                         , df_rep_res))

save(final_result_M_65plus, file = 'final_result_M_65plus.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

