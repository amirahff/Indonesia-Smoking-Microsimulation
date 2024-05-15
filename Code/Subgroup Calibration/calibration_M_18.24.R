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

final_result_M_18.24 = data.frame()

# Calibration p12 increase p_31 to 10%

p_33[7] = p_33[7] - (0.10 - p_31[7])
p_31[7] = 0.10

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_31 0.1'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_31 0.1'
                                         , which_res = 'M_18.24'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p12 increase p_31 to 20%

p_33[7] = p_33[7] - (0.2 - p_31[7])
p_31[7] = 0.2

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_31 0.2'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_31 0.2'
                                         , which_res = 'M_18.24'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p12 increase p_31 to 15%

p_33[7] = p_33[7] - (0.15 - p_31[7])
p_31[7] = 0.15

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_31 0.15'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_31 0.15'
                                         , which_res = 'M_18.24'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p12 increase p_12 to 0.0020

p_11[7] = p_11[7] - (0.0020 - p_12[7])
p_12[7] = 0.0020

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_12 0.0020'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_12 0.0020'
                                         , which_res = 'M_18.24'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

# Calibration p12 increase p_12 to 0.0015

p_11[7] = p_11[7] - (0.0015 - p_12[7])
p_12[7] = 0.0015

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_12 0.0015'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_12 0.0015'
                                         , which_res = 'M_18.24'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

# Calibration p12 increase p_12 to 0.0030

p_11[7] = p_11[7] - (0.0030 - p_12[7])
p_12[7] = 0.0030

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_12 0.0030'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_18.24 = rbind(final_result_M_18.24
                             ,data.frame(category = 'M_18.24 p_12 0.0030'
                                         , which_res = 'M_18.24'
                                         , df_rep_res))

save(final_result_M_18.24, file = 'final_result_M_18.24.Rda')

p_12 = p_12_ori
p_11 = p_11_ori