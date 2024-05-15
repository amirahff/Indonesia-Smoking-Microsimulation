library(tidyverse)

# Read Data
b3a_cov_4 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3a_cov.csv')
b3a_km_4 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh07_all_dta/b3b_km.csv')
b3a_cov_5 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3a_cov.csv')
b3a_km_5 = read_csv('/Users/amirahff/Documents/Brown Biostatistics/PHP 2670/hh14_all_dta/b3b_km.csv')

# Select relevant columns
b3a_cov_4_ess = b3a_cov_4 %>%
  dplyr::select(pidlink,sex,age)

b3a_cov_5_ess = b3a_cov_5 %>%
  dplyr::select(pidlink,sex,age)

b3a_km_4_ess = b3a_km_4 %>%
  dplyr::select(pidlink,km01a,km04)

b3a_km_5_ess = b3a_km_5 %>%
  dplyr::select(pidlink,km01a,km04)

# Combine 3a and 3b, filter complete dataset only
ifls4 = left_join(b3a_cov_4_ess, b3a_km_4_ess, by = 'pidlink')
ifls4_nona = ifls4 %>%
  filter(!(is.na(km01a) & is.na(km04))) %>%
  mutate(year = 2007)
ifls5 = left_join(b3a_cov_5_ess, b3a_km_5_ess, by = 'pidlink')
ifls5_nona = ifls5 %>%
  filter(!(is.na(km01a) & is.na(km04))) %>%
  mutate(year = 2014)

# Merge IFLS 4 and 5, filter complete dataset only
ifls_join = inner_join(ifls4_nona, ifls5_nona, by = 'pidlink')

ifls = rbind(ifls4_nona %>% filter(pidlink %in% ifls_join$pidlink)
             , ifls5_nona %>% filter(pidlink %in% ifls_join$pidlink))

ifls = ifls %>%
  mutate(age_group = case_when(age <= 17 ~ '15.17'
                               ,age <= 24 ~ '18.24'
                               ,age <= 44 ~ '25.44'
                               ,age <= 64 ~ '45.64'
                               ,TRUE ~ '65plus')
         ,smoke_state = case_when(km01a == 3 ~ 'NS'
                                  ,km04 == 1 ~ 'CS'
                                  ,km04 == 3 ~ 'FS')
         ,sex = ifelse(sex == 1,'M','F')
         ,pidlink = as.character(pidlink)) %>%
  mutate(state = case_when(smoke_state == 'CS' ~ 1
                           ,smoke_state == 'FS' ~ 2
                           ,smoke_state == 'NS' ~ 3)
         ,sex = as.factor(sex)
         ,age_group = as.factor(age_group)) %>%
  arrange(pidlink)

ifls_exclude = ifls %>%
  dplyr::select(pidlink, year, state) %>%
  pivot_wider(names_from = year, names_prefix = 'year', values_from = state) %>%
  filter((year2007 < 3 & year2014 == 3) | (year2007 == 3 & year2014 == 2)) 

ifls = ifls %>%
  filter(!pidlink %in% ifls_exclude$pidlink)
  
# check age group distribution
prop.table(table(ifls$age_group))

# check age distribution
ggplot(ifls) + 
  geom_histogram(aes(x=age))

ggplot(ifls[ifls$age_group=='15.17',]) + 
  geom_histogram(aes(x=age))

ggplot(ifls[ifls$age_group=='18.24',]) + 
  geom_histogram(aes(x=age))

ggplot(ifls[ifls$age_group=='25.44',]) + 
  geom_histogram(aes(x=age))

ggplot(ifls[ifls$age_group=='45.64',]) + 
  geom_histogram(aes(x=age))

ggplot(ifls[ifls$age_group=='65plus',]) + 
  geom_histogram(aes(x=age))
  
# Smoking proportion
ifls_plot = ifls %>%
  mutate(sex = ifelse(sex == 'M','Male','Female')
         ,age_group = case_when(age_group == '15.17' ~ '15 to 17 years old'
                                ,age_group == '18.24' ~ '18 to 24 years old'
                                ,age_group == '25.44' ~ '25 to 44 years old'
                                ,age_group == '45.64' ~ '45 to 64 years old'
                                ,age_group == '65plus' ~ '65 plus years old')
         ,smoking_state = case_when(state == 1 ~ 'Current Smoker'
                                    ,state == 2 ~ 'Former Smoker'
                                    ,state == 3 ~ 'Never Smoker')) %>%
  filter(!age_group == '15 to 17 years old', year == 2014)

cont.table.ori = table(ifls_plot$smoking_state,ifls_plot$age_group,ifls_plot$sex)
prop.table(cont.table.ori, margin = c(2,3))
r0 = prop.table(cont.table.ori, margin = c(2,3)) 

cont.table.all = table(ifls_plot$smoking_state,ifls_plot$age_group,ifls_plot$sex)

save(ifls, file = 'ifls.Rda')
save(ifls_plot, file = 'ifls_plot.Rda')

load('ifls.Rda')
load('ifls_plot.Rda')