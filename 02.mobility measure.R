
# load the data, use ru1!! -----------------------------------------------------------
ru1 <- tbl_df(ru1)
ru1
# need to revisit this below ----------------------------------------------



ru1 %>% 
  count(promotion, lateral)

ru1 %>% count(newjob)

ru1 %>% count(newjob, promotion, lateral)
# convert mobility measures to factors ------------------------------------

ru1$newjob <- factor(ru1$newjob, levels = unique(ru1$newjob))
ru1$promotion <- factor(ru1$promotion, levels = unique(ru1$promotion))
ru1$lateral <- factor(ru1$lateral, levels = unique(ru1$lateral))


ru1 %>% 
  count(lateral, promotion)

# change the labels or add some labels ------------------------------------

ru1 <- ru1 %>% 
  mutate(promotion = fct_recode(promotion,
                            'Yes' = '1',
                            'No' = '2'),
         lateral = fct_recode(lateral,
                              'Yes' = '1',
                              'No' = '2'),
         newjob = fct_recode(newjob,
                              'Profession and employer, the same' = '1',
                              'Profession- changed, employer- same' = '2',
                              'Profession- same, employer- changed' = '3',
                              'Profession- changed, employer- changed' = '4'),
         gender = fct_recode(factor(gender),
                             'Male' = '1',
                             'Female' = '2'))



ru1 %>% 
  count(newjob)


ru1$newjob <- factor(ru1$newjob, levels = c("Profession and employer, the same", 
                                            "Profession- changed, employer- same",
                                            "Profession- same, employer- changed",
                                            "Profession- changed, employer- changed"))

  levels(ru1$newjob)

  ru1 %>% 
    count(newjob)

##there's an issue with promotion and lateral moves, they only apply to 
#respondents in the same job or respondents with the same employer


ru1 <- ru1 %>% 
  mutate(promotion = case_when(is.na(promotion) & newjob %in% 
                                 c('Profession- same, employer- changed', 
                                    'Profession- changed, employer- changed') ~'Exit',
                         promotion == 'Yes' ~ 'Yes',
                         promotion == 'No'~ 'No'),
         lateral =case_when(is.na(lateral) & newjob %in% c('Profession- same, employer- changed', 
                                                            'Profession- changed, employer- changed') ~'Exit',
                            lateral == 'Yes' ~ 'Yes',
                            lateral == 'No'~ 'No'))



table(ru1$promotion, ru1$lateral, useNA ='always')


# final mobility measure --------------------------------------------------

ru1 <- ru1 %>% 
  mutate(mob_final = case_when(promotion=='Yes' ~ 'Promotion',
                               lateral == 'Yes' & promotion != 'Yes'~ 'Lateral',
                               promotion == 'No' & lateral =='No' ~ 'Same',
                               promotion =='Exit' | lateral == 'Exit' ~ 'Exit'))



table(ru1$mob_final)


##relevel
ru1$promotion <- factor(ru1$promotion, levels = c('No', 'Yes', 'Exit'))
ru1$lateral <- factor(ru1$lateral, levels = c('No', 'Yes', 'Exit'))
ru1$mob_final <- factor(ru1$mob_final, levels = c('Same', 'Promotion', 'Lateral', 'Exit'))

levels(ru1$promotion)
levels(ru1$lateral)
levels(ru1$mob_final)



# view and explore --------------------------------------------------------


ru1 %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=mob_final, fill=gender))+
  geom_bar(position = 'dodge', aes(y=..prop.., group=gender))+
  coord_flip()


ru1 %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=round, fill=mob_final))+
  geom_bar(position = 'fill', aes(group=mob_final))

ru1


# create a "balanced" group/panel for brief comparison --------------------
ru2 <- ru1 %>% 
  filter(n==5)

prop.table(table(ru1$mob_final,ru1$round),2)
prop.table(table(ru2$mob_final,ru2$round),2)


ru1 %>% 
  filter( !is.na(mob_final),
          income < 1000000) %>% 
  ggplot(aes(x=mob_final, fill=mob_final))+
  geom_boxplot(aes(y=income))+
  coord_flip()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)



# model -------------------------------------------------------------------

ru2 <- ru2 %>% 
  mutate(log_wage = log(wage)) %>% 
  filter(log_wage > 0)


m1 <- lm(data= ru2, log_wage ~ newjob)
m2 <- lm(data= ru2, log_wage ~ promotion+lateral)
m3 <- lm(data= ru2, log_wage ~ gender+mob_final)



coef(m1)
summary(m1)

coef(m2)
summary(m2)

coef(m3)
summary(m3)



table(ru1$newjob, ru1$promotion, ru1$lateral, useNA= 'always')

