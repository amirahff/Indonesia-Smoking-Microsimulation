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

final_result_M_45.64 = data.frame()

# Calibration p14 increase p_31 to 0.08

p_33[9] = p_33[9] - (0.08 - p_31[9])
p_31[9] = 0.08

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_31 0.08'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_31 0.08'
                                         , which_res = 'M_45.64'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p14 increase p_31 to 0.01

p_33[9] = p_33[9] - (0.01 - p_31[9])
p_31[9] = 0.01

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_31 0.01'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_31 0.01'
                                         , which_res = 'M_45.64'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p14 increase p_31 to 0.05

p_33[9] = p_33[9] - (0.05 - p_31[9])
p_31[9] = 0.05

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_31 0.05'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_31 0.05'
                                         , which_res = 'M_45.64'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

p_31 = p_31_ori
p_33 = p_33_ori

# Calibration p14 increase p_12 to 0.010

p_11[9] = p_11[9] - (0.010 - p_12[9])
p_12[9] = 0.010

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_12 0.010'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_12 0.010'
                                         , which_res = 'M_45.64'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

# Calibration p14 increase p_12 to 0.0050

p_11[9] = p_11[9] - (0.0050 - p_12[9])
p_12[9] = 0.0050

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_12 0.0050'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

# rep_res = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_12 0.0050'
                                         , which_res = 'M_45.64'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

p_12 = p_12_ori
p_11 = p_11_ori

# Calibration p14 increase p_12 to 0.0010

p_11[9] = p_11[9] - (0.0010 - p_12[9])
p_12[9] = 0.0010

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_12 0.0010'
                                         , which_res = 'all'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_M_45.64 = rbind(final_result_M_45.64
                             ,data.frame(category = 'M_45.64 p_12 0.0010'
                                         , which_res = 'M_45.64'
                                         , df_rep_res))

save(final_result_M_45.64, file = 'final_result_M_45.64.Rda')

p_12 = p_12_ori
p_11 = p_11_ori
