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

final_result_ori = data.frame()

rep_res = replicate(10, get_mape(N=10000, whichone='all'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_ori = rbind(final_result_ori
                              ,data.frame(category = 'no adjustment'
                                          , which_res = 'all'
                                          , df_rep_res))

save(final_result_ori, file = 'final_result_ori.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_18.24'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_ori = rbind(final_result_ori
                         ,data.frame(category = 'no adjustment'
                                     , which_res = 'M_18.24'
                                     , df_rep_res))

save(final_result_ori, file = 'final_result_ori.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_25.44'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_ori = rbind(final_result_ori
                         ,data.frame(category = 'no adjustment'
                                     , which_res = 'M_25.44'
                                     , df_rep_res))

save(final_result_ori, file = 'final_result_ori.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_45.64'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_ori = rbind(final_result_ori
                         ,data.frame(category = 'no adjustment'
                                     , which_res = 'M_45.64'
                                     , df_rep_res))

save(final_result_ori, file = 'final_result_ori.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='M_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_ori = rbind(final_result_ori
                         ,data.frame(category = 'no adjustment'
                                     , which_res = 'M_65plus'
                                     , df_rep_res))

save(final_result_ori, file = 'final_result_ori.Rda')

rep_res = replicate(10, get_mape(N=10000, whichone='F_65plus'))
df_rep_res = data.frame(mape = rep_res[1,], rmse = rep_res[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

final_result_ori = rbind(final_result_ori
                         ,data.frame(category = 'no adjustment'
                                     , which_res = 'F_65plus'
                                     , df_rep_res))

save(final_result_ori, file = 'final_result_ori.Rda')
