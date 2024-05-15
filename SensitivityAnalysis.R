subgroups_prevalences2 = final_df %>%
  filter(!is.na(smoking_state)) %>%
  group_by(sex,age_group) %>%
  summarize(microsim_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
  ungroup() %>%
  # mutate(condition = 'Base') %>%
  mutate(condition = 'Calibrated') %>%
  select(condition, sex, age_group, microsim_prevalence)

population_prevalences2 = final_df %>%
  filter(!is.na(smoking_state)) %>%
  summarize(microsim_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
  # mutate(sex = 'All', age_group = 'All', condition = 'Base') %>%
  mutate(sex = 'All', age_group = 'All', condition = 'Calibrated') %>%
  select(condition, sex, age_group, microsim_prevalence)

subgroups_prevalences3 = ifls_plot %>%
  filter(!is.na(smoking_state), !is.na(sex)) %>%
  group_by(sex,age_group) %>%
  summarize(ifls_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
  ungroup() %>%
  # mutate(condition = 'Base') %>%
  mutate(condition = 'Calibrated') %>%
  select(condition, sex, age_group, ifls_prevalence)

population_prevalences3 = ifls_plot %>%
  filter(!is.na(smoking_state), !is.na(sex)) %>%
  summarize(ifls_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
  # mutate(sex = 'All', age_group = 'All', condition = 'Base') %>%
  mutate(sex = 'All', age_group = 'All', condition = 'Calibrated') %>%
  select(condition, sex, age_group, ifls_prevalence)

microsim_prevalence = rbind(population_prevalences2, subgroups_prevalences2)
ifls_prevalence = rbind(population_prevalences3, subgroups_prevalences3)
all_prevalence = left_join(microsim_prevalence, ifls_prevalence
                           , by = c('condition', 'sex', 'age_group'))

all_prevalence = all_prevalence %>%
  mutate(microsim_prevalence = round(microsim_prevalence,4)
         ,ifls_prevalence = round(ifls_prevalence,4)) %>%
  mutate(abs_error = abs(microsim_prevalence - ifls_prevalence))

all_prevalence = all_prevalence %>%
  mutate(microsim_prevalence = round(microsim_prevalence,4)
         ,ifls_prevalence = round(ifls_prevalence,4)) %>%
  mutate(squared_error = (microsim_prevalence - ifls_prevalence)^2) %>%
  summarize(rmse = sqrt(mean(squared_error)))

library(kableExtra)

all_prevalence %>%
  kableExtra::kbl(booktabs = T
                  , escape = T
                  , align = 'c'
                  , caption = 'Prevalence from Initial Microsimulation Result') %>%
  kableExtra::kable_classic(full_width = F
                            , html_font = 'Cambria'
                            , latex_options = 'HOLD_position')

# initial_prevalence = all_prevalence
# save(initial_prevalence, file = 'initial_prevalence.Rda')

# calibrated_prevalence = all_prevalence
# save(calibrated_prevalence, file = 'calibrated_prevalence.Rda')

####################################

subgroups_prevalences2 = final_df %>%
  filter(!is.na(smoking_state)) %>%
  group_by(sex,age_group) %>%
  summarize(current_smoker = sum(smoking_state=='Current Smoker')/n()
            , former_smoker = sum(smoking_state=='Former Smoker')/n()
            , never_smoker = sum(smoking_state=='Never Smoker')/n()) %>%
  ungroup() %>%
  mutate(condition = 'Base', data = 'Microsimulation') %>%
  select(data, condition, sex, age_group, current_smoker, former_smoker, never_smoker)

population_prevalences2 = final_df %>%
  filter(!is.na(smoking_state)) %>%
  summarize(current_smoker = sum(smoking_state=='Current Smoker')/n()
            , former_smoker = sum(smoking_state=='Former Smoker')/n()
            , never_smoker = sum(smoking_state=='Never Smoker')/n()) %>%
  mutate(sex = 'All', age_group = 'All', condition = 'Base', data = 'Microsimulation') %>%
  select(data, condition, sex, age_group, current_smoker, former_smoker, never_smoker)

subgroups_prevalences3 = ifls_plot %>%
  filter(!is.na(smoking_state), !is.na(sex)) %>%
  group_by(sex,age_group) %>%
  summarize(current_smoker = sum(smoking_state=='Current Smoker')/n()
            , former_smoker = sum(smoking_state=='Former Smoker')/n()
            , never_smoker = sum(smoking_state=='Never Smoker')/n()) %>%
  ungroup() %>%
  mutate(condition = 'Base', data = 'IFLS') %>%
  select(data, condition, sex, age_group, current_smoker, former_smoker, never_smoker)

population_prevalences3 = ifls_plot %>%
  filter(!is.na(smoking_state), !is.na(sex)) %>%
  summarize(current_smoker = sum(smoking_state=='Current Smoker')/n()
            , former_smoker = sum(smoking_state=='Former Smoker')/n()
            , never_smoker = sum(smoking_state=='Never Smoker')/n()) %>%
  mutate(sex = 'All', age_group = 'All', condition = 'Base', data = 'IFLS') %>%
  select(data, condition, sex, age_group, current_smoker, former_smoker, never_smoker)

microsim_prevalence = rbind(population_prevalences2, subgroups_prevalences2)
ifls_prevalence = rbind(population_prevalences3, subgroups_prevalences3)
all_prevalence = rbind(microsim_prevalence, ifls_prevalence)

all_prevalence = all_prevalence %>%
  pivot_longer(cols = c(current_smoker,former_smoker,never_smoker)
               , values_to = c("prevalence"), names_to = 'smoking_status') %>%
  filter(sex != 'All', age_group != 'All')

library(ggplot2)
ggplot(all_prevalence) +
  geom_bar(aes(x = age_group, y = prevalence, fill = data)
           , stat='identity', position = 'dodge') +
  facet_grid(smoking_status ~ sex) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        ,legend.position = 'bottom')
  

####################################

subgroups_prevalences = final_df %>%
  filter(!is.na(smoking_state)) %>%
  group_by(sex,age_group) %>%
  summarize(smoking_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
  ungroup() %>%
  mutate(condition = 'base') %>%
  select(condition, sex, age_group, smoking_prevalence)

population_prevalences = final_df %>%
  filter(!is.na(smoking_state)) %>%
  summarize(smoking_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
  mutate(sex = 'all', age_group = 'all', condition = 'base') %>%
  select(condition, sex, age_group, smoking_prevalence)

p_31_base = p_31
p_12_base = p_12
p_21_base = p_21
p_33_base = p_33
p_11_base = p_11
p_22_base = p_22

for (i in 1:10) {
  
  for (j in 1:2) {
    
    if(j == 1) {multiplier = 0.5}
    else{multiplier = 2}
    
    for (k in 1:3) {
      
      if(k == 1) {
        p_11[i] = p_11[i] - (p_12[i]*multiplier) 
        p_12[i] = p_12[i]*multiplier
        }
      else if(k == 2) {
        p_22[i] = p_22[i] - (p_21[i]*multiplier)
        p_21[i] = p_21[i]*multiplier
        }
      else if(k == 3) {
        p_33[i] = p_33[i] - (p_31[i]*multiplier)
        p_31[i] = p_31[i]*multiplier
        }
      
      set.seed(222)
      all_res_tmp = microsim(N = 10000, cycle = 7)
      final_df_tmp = data.frame(sex = all_res_tmp[[2]]
                                , age_group = all_res_tmp[[3]]
                                , smoking_state = all_res_tmp[[1]][[8]])

      subgroups_prevalences_tmp = final_df_tmp %>%
        filter(!is.na(smoking_state)) %>%
        filter(sex == ifelse(i<=5,0,1)
               , age_group == ifelse(i>5,i-5,i)) %>%
        mutate(sex = ifelse(sex == 1,'Male','Female')
               ,age_group = case_when(age_group == 1 ~ '15 to 17 years old'
                                      ,age_group == 2 ~ '18 to 24 years old'
                                      ,age_group == 3 ~ '25 to 44 years old'
                                      ,age_group == 4 ~ '45 to 64 years old'
                                      ,age_group == 5 ~ '65 plus years old')
               ,smoking_state = case_when(smoking_state == 1 ~ 'Current Smoker'
                                          ,smoking_state == 2 ~ 'Former Smoker'
                                          ,smoking_state == 3 ~ 'Never Smoker')) %>%
        group_by(sex,age_group) %>%
        summarize(smoking_prevalence = sum(smoking_state=='Current Smoker')/n()) %>%
        ungroup() %>%
        mutate(condition1 = case_when(i == 1 ~ 'F_15.17'
                                      ,i == 2 ~ 'F_18.24'
                                      ,i == 3 ~ 'F_25.44'
                                      ,i == 4 ~ 'F_45.64'
                                      ,i == 5 ~ 'F_65plus'
                                      ,i == 6 ~ 'M_15.17'
                                      ,i == 7 ~ 'M_18.24'
                                      ,i == 8 ~ 'M_25.44'
                                      ,i == 9 ~ 'M_45.64'
                                      ,i == 10 ~ 'M_65plus')
               ,condition2 = case_when(k == 1 ~ 'CS to FS'
                                       ,k == 2 ~ 'FS to CS'
                                       ,k == 3 ~ 'NS to CS')
               ,condition3 = case_when(j == 1 ~ '0.5x'
                                       ,j == 2 ~ '2x')) %>%
        mutate(condition = paste(condition1,condition2,condition3)) %>%
        select(condition, sex, age_group, smoking_prevalence)
      
      population_prevalences_tmp = final_df_tmp %>%
        filter(!is.na(smoking_state)) %>%
        summarize(smoking_prevalence = sum(smoking_state==1)/n()) %>%
        mutate(condition1 = case_when(i == 1 ~ 'F_15.17'
                                      ,i == 2 ~ 'F_18.24'
                                      ,i == 3 ~ 'F_25.44'
                                      ,i == 4 ~ 'F_45.64'
                                      ,i == 5 ~ 'F_65plus'
                                      ,i == 6 ~ 'M_15.17'
                                      ,i == 7 ~ 'M_18.24'
                                      ,i == 8 ~ 'M_25.44'
                                      ,i == 9 ~ 'M_45.64'
                                      ,i == 10 ~ 'M_65plus')
               ,condition2 = case_when(k == 1 ~ 'CS to FS'
                                       ,k == 2 ~ 'FS to CS'
                                       ,k == 3 ~ 'NS to CS')
               ,condition3 = case_when(j == 1 ~ '0.5x'
                                       ,j == 2 ~ '2x')) %>%
        mutate(condition = paste(condition1,condition2,condition3)) %>%
        mutate(sex = 'all', age_group = 'all') %>%
        select(condition, sex, age_group, smoking_prevalence)
      
      subgroups_prevalences = rbind(subgroups_prevalences
                                    , subgroups_prevalences_tmp)
      population_prevalences = rbind(population_prevalences
                                     , population_prevalences_tmp)
      
      p_12 = p_12_base
      p_21 = p_21_base
      p_31 = p_31_base
      p_11 = p_11_base
      p_22 = p_22_base
      p_33 = p_33_base
      
    }
  }
}

write.csv(subgroups_prevalences, file = 'subgroups_prevalences.csv')
write.csv(population_prevalences, file = 'population_prevalences.csv')

###########################

population_prevalences_new = read.csv('population_prevalences_edit.csv')

# Population
population_prevalences_new = population_prevalences_new %>%
  filter(parameter != 'base')

# original value of output
base.value <- 0.331877729

# get order of parameters according to size of intervals
# (I use this to define the ordering of the factors which I then use to define the positions in the plot)
order.parameters <- population_prevalences_new %>% arrange(UL_difference) %>%
  filter(UL_difference != 0) %>%
  mutate(parameter=factor(x=parameter, levels=parameter)) %>%
  select(parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.95

# get data frame in shape for ggplot and geom_rect
population_prevalences_plot <- population_prevalences_new %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value', lower_bound:upper_bound) %>%
  # just reordering columns
  select(parameter, type, output.value, UL_difference) %>%
  # create the columns for geom_rect
  mutate(parameter=factor(parameter, levels=order.parameters),
         ymin=pmin(output.value, base.value),
         ymax=pmax(output.value, base.value),
         xmin=as.numeric(parameter)-width/2,
         xmax=as.numeric(parameter)+width/2)

# create plot
# (use scale_x_continuous to change labels in y axis to name of parameters)
png(width = 1200, height = 1000)
ggplot() + 
  geom_rect(data = population_prevalences_plot, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), 
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 20)) + 
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters) +
  coord_flip()
dev.off()


#########################

subgroups_prevalences_new = read.csv('subgroups_prevalences_edit.csv')

categs = c('F_18.24','F_25.44','F_45.64','F_65plus'
           ,'M_18.24','M_25.44','M_45.64','M_65plus')

plotss =vector(mode = "list", length = 8)

for (cats in categs) {
  
  base.value = subgroups_prevalences_new[
    subgroups_prevalences_new$category == cats 
    &subgroups_prevalences_new$parameter == 'base','lower_bound']
  
  subgroups_prevalences_new2 = subgroups_prevalences_new %>%
    filter(parameter != 'base') %>%
    filter(category == cats)
  
  # get order of parameters according to size of intervals
  # (I use this to define the ordering of the factors which I then use to define the     positions in the plot)
  order.parameters <- subgroups_prevalences_new2 %>% arrange(UL_difference) %>%
    mutate(parameter=factor(x=parameter, levels=parameter)) %>%
    select(parameter) %>% unlist() %>% levels()
  
  # width of columns in plot (value between 0 and 1)
  width <- 0.95
  
  # get data frame in shape for ggplot and geom_rect
  subgroups_prevalences_plot <- subgroups_prevalences_new2 %>% 
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='output.value', lower_bound:upper_bound) %>%
    # just reordering columns
    select(parameter, type, output.value, UL_difference) %>%
    # create the columns for geom_rect
    mutate(parameter=factor(parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(parameter)-width/2,
           xmax=as.numeric(parameter)+width/2)
  
  # create plot
  # (use scale_x_continuous to change labels in y axis to name of parameters)
  # png(width = 1200, height = 1000)
  p_tmp=ggplot() + 
    geom_rect(data = subgroups_prevalences_plot, 
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
    theme_bw() + 
    theme(axis.title.y=element_blank(), legend.position = 'bottom',
          legend.title = element_blank()
          ,text = element_text(size = 20)) + 
    geom_hline(yintercept = base.value) +
    scale_x_continuous(breaks = c(1:length(order.parameters)), 
                       labels = order.parameters) +
    coord_flip()
  # dev.off()
  
  plotss = append(plotss,p_tmp)
  print(p_tmp)
}
  


