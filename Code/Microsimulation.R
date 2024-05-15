library(mcreplicate)

microsim = function(N = 10000, cycle = 10) {
  
  which_age_group = function(age) {
    age_group = dplyr::case_when(age <= 17 ~ 1
                                 ,age <= 24 ~ 2
                                 ,age <= 44 ~ 3
                                 ,age <= 64 ~ 4
                                 ,TRUE ~ 5)
    return(age_group)
  }
  
  sample_age = function(idx) {
    res = sample(list_of_age[[idx]],1) 
    return(res)
  }
  
  which_next_state = function(state,p_tmp,sex,age_group_ord) {
    if(is.na(state)) {res = NA_real_}
    else {
      idx = (sex*5)+age_group_ord
      if(state == 1) {
        res = dplyr::case_when(p_tmp <= p_11[idx] ~ 1
                               ,p_tmp <= p_11[idx]+p_12[idx] ~ 2
                               ,TRUE ~ NA_real_)
      }
      if(state == 2) {
        res = dplyr::case_when(p_tmp <= p_21[idx] ~ 1
                               ,p_tmp <= p_21[idx]+p_22[idx] ~ 2
                               ,TRUE ~ NA_real_)
      }
      if(state == 3) {
        res = dplyr::case_when(p_tmp <= p_31[idx] ~ 1
                               ,p_tmp <= p_33[idx]+p_33[idx] ~ 3
                               ,TRUE ~ NA_real_)
      }
    }
    return(res)
  }
  
  sex = rbinom(N,1,0.45)
  
  list_of_age = list(seq(15,17),seq(18,24),seq(25,44),seq(45,64),seq(65,80))
  
  age_group = sample(c(1,2,3,4,5),N, prob=c(0.0328,0.1192,0.5136,0.2661,0.0682)
                     , replace=TRUE)
  
  state_init_probs = list(c(0.000,0.0000,1),c(0.0018,0.0005,0.9977)
                          ,c(0.0107,0.0010,0.9883),c(0.0383,0.0041,0.9576)
                          ,c(0.1201,0.0120,0.8679),c(0.2756,0.0090,0.7154)
                          ,c(0.6662,0.0212,0.3126),c(0.7343,0.0231,0.2426)
                          ,c(0.7401,0.0562,0.2037),c(0.7377,0.1194,0.1429))
  
  sex_ord = c()
  age_group_ord = c()
  smoke_init_ord = c()
  for (i in 0:1) {
    age_sex = age_group[sex == i]
    for (j in 1:5) {
        n = length(age_sex[age_sex==j])
        smoke_init = sample(c(1,2,3),n
                            ,prob=state_init_probs[[(5*i)+j]]
                            ,replace=TRUE)
        
        sex_ord = append(sex_ord, rep(i,n))
        age_group_ord = append(age_group_ord, rep(j,n))
        smoke_init_ord = append(smoke_init_ord, smoke_init)
    }
  }
  
  age_ord = sapply(age_group_ord,sample_age)
  
  all_state = vector('list')
  all_state[[1]] = smoke_init_ord
  for(cycle in 1:7) {
    if(cycle==1) {state = smoke_init_ord} 
    else{state = next_state}
    age_ord = age_ord + 1
    age_group_ord = which_age_group(age_ord)
    
    p_tmp = runif(N,0,1)
    next_state = mapply(which_next_state, state, p_tmp, sex_ord, age_group_ord)
    all_state[[1+cycle]] = next_state
  }

  return(list(all_state,sex_ord,age_group_ord))
}



  get_mape = function(N = 10000, whichone = 'all') {
    
    all_res = microsim(N = N, cycle = 7)
  
  final_df = data.frame(sex = all_res[[2]]
                        , age_group = all_res[[3]]
                        , smoking_state = all_res[[1]][[8]])
  
  final_df = final_df %>%
    mutate(sex = ifelse(sex == 1,'Male','Female')
           ,age_group = case_when(age_group == 1 ~ '15 to 17 years old'
                                  ,age_group == 2 ~ '18 to 24 years old'
                                  ,age_group == 3 ~ '25 to 44 years old'
                                  ,age_group == 4 ~ '45 to 64 years old'
                                  ,age_group == 5 ~ '65 plus years old')
           ,smoking_state = case_when(smoking_state == 1 ~ 'Current Smoker'
                                      ,smoking_state == 2 ~ 'Former Smoker'
                                      ,smoking_state == 3 ~ 'Never Smoker'))
  
  cont.table = table(final_df$smoking_state,final_df$age_group,final_df$sex)
  r1 = prop.table(cont.table, margin = c(2,3))
  
  if (whichone == 'all') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[seq(1,22,3)]),0,(abs(r1-r0)/r0)[seq(1,22,3)])
    mape = mean(ape)
    rmse = sqrt(mean((r1 - r0)^2))
  }
  else if (whichone == 'M_18.24') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[13]),0,(abs(r1-r0)/r0)[13])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[13]))
  }
  else if (whichone == 'M_25.44') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[16]),0,(abs(r1-r0)/r0)[16])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[16]))
  }
  else if (whichone == 'M_45.64') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[19]),0,(abs(r1-r0)/r0)[19])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[19]))
  }
  else if (whichone == 'M_65plus') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[22]),0,(abs(r1-r0)/r0)[22])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[22]))
  }
  else if (whichone == 'F_18.24') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[1]),0,(abs(r1-r0)/r0)[1])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[1]))
  }
  else if (whichone == 'F_25.44') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[4]),0,(abs(r1-r0)/r0)[4])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[4]))
  }
  else if (whichone == 'F_45.64') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[7]),0,(abs(r1-r0)/r0)[7])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[7]))
  }
  else if (whichone == 'F_65plus') {
    ape = ifelse(is.na((abs(r1-r0)/r0)[10]),0,(abs(r1-r0)/r0)[10])
    mape = mean(ape)
    rmse = sqrt(mean(((r1 - r0)^2)[10]))
  }
  
  return(c(mape,rmse))
}

###
  
exx = mc_replicate(10, get_mape(N=10000, whichone='all'), mc.cores = 4)
df_exx = data.frame(mape = exx[1,], rmse = exx[2,]) %>%
  summarize(mean_mape = mean(mape)
            ,mean_rmse = mean(rmse))

###

set.seed(222)
all_res = microsim(N = 10000, cycle = 7)
final_df = data.frame(sex = all_res[[2]]
                      , age_group = all_res[[3]]
                      , smoking_state = all_res[[1]][[8]])

final_df = final_df %>%
  mutate(sex = ifelse(sex == 1,'Male','Female')
         ,age_group = case_when(age_group == 1 ~ '15 to 17 years old'
                                ,age_group == 2 ~ '18 to 24 years old'
                                ,age_group == 3 ~ '25 to 44 years old'
                                ,age_group == 4 ~ '45 to 64 years old'
                                ,age_group == 5 ~ '65 plus years old')
         ,smoking_state = case_when(smoking_state == 1 ~ 'Current Smoker'
                                    ,smoking_state == 2 ~ 'Former Smoker'
                                    ,smoking_state == 3 ~ 'Never Smoker'))

cont.table = table(final_df$smoking_state,final_df$age_group,final_df$sex)
r1 = prop.table(cont.table, margin = c(2,3))

round(r1,4)
round(r0,4)


save(final_result, file = 'final_result.Rda')
load('final_result.Rda')

save(microsim, file = 'microsim.Rda')
save(get_mape, file = 'get_mape.Rda')

save(calibration_result2, file = 'calibration_result2.Rda')
