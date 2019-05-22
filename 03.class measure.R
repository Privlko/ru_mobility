# use ru3, not ru2 --------------------------------------------------------

ru3 <- tbl_df(ru2)
ru3

# you have to go back and include the measures below in 01 ----------------

ru3 %>% count(sub)
ru3 %>% count(isco)
ru3 %>% count(sub_n)


# recoding isco to a 2-digit figure --------------------

ru3 <- ru3 %>% 
  mutate(isco08 = isco/100,
         isco08 = round(isco08, 0))


ru3 %>% count(isco08)
# converting measures to factors ------------------------------------------

ru3$econ <- factor(ru3$mainoc, levels = unique(ru3$mainoc))
ru3$super <- factor(ru3$sub, levels = unique(ru3$sub))


ru3 %>% 
  count(isco)

# labels and coding -------------------------------------------------------

ru3 <- ru3 %>% 
  mutate(econ = fct_recode(econ,
                           'Inactive' = '1',
                           'Inactive' = '2',
                           'Inactive' = '3',
                           'Inactive' = '4',
                           'Inactive' = '5',
                           'Inactive' = '6',
                           'Inactive' = '7',
                           'Unemployed' = '8',
                           'Unemployed' = '9',
                           'Self-employed' = '10',
                           'Self-employed' = '11',
                           'Employed' = '12',
                           'Employed' = '13'))


ru3 <- ru3 %>% 
  mutate(super = fct_recode(super,
                           'Yes' = '1',
                           'No' = '2'))

ru3 <- ru3 %>% 
  mutate(ten_employees = sub_n < 10 )



table(ru3$sub_n,ru3$ten_employees, useNA='ifany')


# fix tags ----------------------------------------------------------------

ru3$econ <- factor(ru3$econ, levels = c('Employed','Self-employed',
                                    'Unemployed', 'Inactive'))

ru3$super <- factor(ru3$super, levels = c('Yes','No'))




# esec categories ---------------------------------------------------------
ru3 %>% count(econ)

table(ru3$super,ru3$econ, useNA='ifany')

ru3 <- ru3 %>%
  mutate(econstat = case_when(econ=='Self-employed' & ten_employees==FALSE ~'Employer, large',
                              econ=='Self-employed' & ten_employees==TRUE ~ 'Employer, small',
                              econ=='Self-employed' & super == 'No' ~ 'Self-employed, no employees',
                              econ=='Employed' & super == 'Yes' ~ 'Supervisors, managers',
                              econ=='Employed' & super == 'No' ~ 'Regular workers, employees'))


ru3 %>% 
  count(econ, econstat)

ru3$econstat <- factor(ru3$econstat, levels = c('Regular workers, employees',
                                            'Supervisors, managers',
                                            'Self-employed, no employees',
                                            'Employer, small',
                                            'Employer, large'))



ru3 <- ru3 %>% 
  mutate(esec = case_when(econstat=='Employer, large' & 
                          (isco08 != 54| isco08 != 63 | !is.na(isco08))~ 'Large employers, higher mgrs/prof',
                          isco08 %in% c(1:3, 11, 20:21, 24, 26:27) ~'Large employers, higher mgrs/prof',
                          isco08 ==12 & (econstat=='Supervisors, managers'| 
                                        econstat=='Regular workers, employees')~'Large employers, higher mgrs/prof',
                          isco08 ==13 & (econstat=='Supervisors, managers')~'Large employers, higher mgrs/prof',
                          isco08 %in% c(12:14) & econstat=='Employer, large' ~'Large employers, higher mgrs/prof',
                          isco08 %in% c(22:23, 25, 31:33) ~ 'Lower mgrs/prof, higher superv',
                          isco08 %in% c(13,14) & econstat %in% c('Supervisors, managers', 'Regular workers, employees')~'Lower mgrs/prof, higher superv',
                          isco08 %in% c(30) & econstat=='Supervisors, managers' ~'Lower mgrs/prof, higher superv',
                          isco08 %in% c (34,35, 40:43) & econstat=='Supervisors, managers'~'Lower mgrs/prof, higher superv',
                          isco08 %in% c (30) & econstat == 'Regular workers, employees' ~ 'Intermediate occupations',
                          isco08 %in% c(34, 35, 40:44) & econstat == 'Regular workers, employees' ~ 'Intermediate occupations',
                          isco08 %in% c(54:52) & econstat %in% c('Self-employed, no employees','Employer, small','Employer, large', 'Supervisors, managers') ~'Intermediate occupations',
                          isco08 %in% c(12, 13, 14, 30, 34, 53, 70, 91, 94, 96) & econstat %in% c('Self-employed, no employees','Employer, small') ~ 'Small employers and self-emp (non-agriculture)',
                          isco08 %in% c(60, 62, 92, 93) & econstat %in% c('Self-employed, no employees','Employer, small') ~ 'Small employers and self-emp (agriculture)',
                          isco08 %in% c(63) & econstat %in% c('Employer, small', 'Supervisors, managers') ~ 'Small employers and self-emp (agriculture)',
                          isco08 %in% c(42, 44, 62, 70, 96) & econstat %in% c('Supervisors, managers') ~ 'Lower supervisors and technicians',
                          isco08 %in% c(42, 50:54) & econstat %in% c('Regular workers, employees') ~ 'Lower sales and service',
                          isco08 %in% c(60, 61, 62, 70:75) & econstat %in% c('Regular workers, employees') ~ 'Lower technical',
                          isco08 %in% c(80:83, 90:96) & econstat %in% c('Regular workers, employees') ~ 'Routine'))


ru3 %>% 
  count(esec)

ru3$esec <- factor(ru3$esec, levels = c("Large employers, higher mgrs/prof", 
                                    "Lower mgrs/prof, higher superv", 
                                    "Intermediate occupations",
                                    "Small employers and self-emp (non-agriculture)",
                                    "Small employers and self-emp (agriculture)",
                                    "Lower supervisors and technicians",
                                    "Lower sales and service","Lower technical",
                                    "Routine"))

levels(ru3$esec)

ru3 %>% 
  filter(!is.na(esec)) %>% 
  ggplot(aes(x=esec))+
  geom_bar(position='stack',
           aes(y=..prop.., group=gender, fill=gender))+
  coord_flip()

ru3 %>% 
count(esec)

ru3 %>% 
  filter(!is.na(esec),
         income<1000000) %>% 
  ggplot(aes(income, colour=esec, fill=esec))+
  geom_density(alpha=0.1)+
  scale_x_log10()

table(ru3$esec, ru3$isco08, useNA = 'always')

options(na.action="na.omit")

# simplified esec categories ---------------------------------------------------

ru3 <- ru3 %>% 
  mutate(esec_simple = fct_recode(esec,
                           'High' = "Large employers, higher mgrs/prof",
                           'High' = "Lower mgrs/prof, higher superv",
                           'Medium' = "Intermediate occupations",
                           'Medium' = "Small employers and self-emp (non-agriculture)",
                           'Medium' = "Small employers and self-emp (agriculture)",
                           'Low' = "Lower supervisors and technicians",
                           'Low' = "Lower sales and service","Lower technical",
                           'Low' = "Lower technical",
                           'Low' = "Routine"))


ru3 %>% 
  count(esec, esec_simple)

ru3 %>% 
  filter(!is.na(esec)) %>% 
  ggplot(aes(esec))+
  geom_bar(aes(y=..prop..,group=1))+
  coord_flip()+
  facet_wrap(~esec_simple)



ru3 %>% 
  filter(age<65,
         age>18,
         !is.na(mob_final),
         !is.na(esec_simple)) %>% 
  ggplot(aes(x=age, fill=factor(mob_final)))+
  geom_histogram(binwidth = 3, position = 'fill')+
  geom_hline(yintercept=0.5, alpha=0.3, size=2)+
  theme_minimal()+
  facet_wrap(~esec_simple)+
  labs(x='Age of respondent',
       y='Share',
       fill='Mobility type',
       title='Mobility by age across social class groups',
       subtitle='Upper class respondents experience more upper mobility across all ages, \nlower class groups experience more exit among older workers.',
       caption='Source: RLMS, rounds 20-24. \nPlot: @privlko')


ru3 %>% 
  filter(age<65,
         age>16,
         !is.na(ever_promoted),
         !is.na(esec_simple)) %>% 
  ggplot(aes(x=ever_promoted, y=wage))+
  geom_boxplot(alpha=0.5)+
  scale_y_log10(labels= scales::comma)+
  theme_minimal()+
  facet_wrap(~esec_simple)

ru3 %>% 
  filter(age<65,
         age>16,
         !is.na(ever_lateral),
         !is.na(esec_simple)) %>% 
  ggplot(aes(x=ever_lateral, y=wage))+
  geom_boxplot(alpha=0.5)+
  scale_y_log10(labels= scales::comma)+
  theme_minimal()+
  facet_wrap(~esec_simple)


ru3 %>% 
  filter(age<65,
         age>16,
         !is.na(esec_simple)) %>% 
  ggplot(aes(x=esec_simple, y=wage, fill=esec_simple))+
  geom_jitter(alpha=0.05, aes(colour=esec_simple))+
  geom_boxplot(alpha=0.5, aes(fill=esec_simple))+
  scale_y_log10(labels= scales::comma)+
  theme_minimal()



ru3 %>% 
  filter(age<65,
         age>16,
         !is.na(esec),
         wage>500) %>% 
  ggplot(aes(x=esec, y=wage, fill=esec))+
  geom_jitter(alpha=0.05, aes(colour=esec))+
  geom_boxplot(alpha=0.5, aes(fill=esec))+
  scale_y_log10(labels= scales::comma)+
  theme_minimal()+
  coord_flip()+
  theme(legend.position = "none") 
