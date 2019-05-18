
# load the data, don't use ru1!! -----------------------------------------------------------
ru2 <- tbl_df(ru1)
ru2
# need to revisit this below ----------------------------------------------



ru2 %>% 
  count(promotion, lateral)

ru2 %>% count(newjob)

ru2 %>% count(newjob, promotion, lateral)
# convert mobility measures to factors ------------------------------------

ru2$newjob <- factor(ru2$newjob, levels = unique(ru2$newjob))
ru2$promotion <- factor(ru2$promotion, levels = unique(ru2$promotion))
ru2$lateral <- factor(ru2$lateral, levels = unique(ru2$lateral))


ru2 %>% 
  count(lateral, promotion)

# change the labels or add some labels ------------------------------------

ru2 <- ru2 %>% 
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



ru2 %>% 
  count(newjob)


ru2$newjob <- factor(ru2$newjob, levels = c("Profession and employer, the same", 
                                            "Profession- changed, employer- same",
                                            "Profession- same, employer- changed",
                                            "Profession- changed, employer- changed"))

  levels(ru2$newjob)

  ru2 %>% 
    count(newjob)

##there's an issue with promotion and lateral moves, they only apply to 
#respondents in the same job or respondents with the same employer


ru2 <- ru2 %>% 
  mutate(promotion = case_when(is.na(promotion) & newjob %in% 
                                 c('Profession- same, employer- changed', 
                                    'Profession- changed, employer- changed') ~'Exit',
                         promotion == 'Yes' ~ 'Yes',
                         promotion == 'No'~ 'No'),
         lateral =case_when(is.na(lateral) & newjob %in% c('Profession- same, employer- changed', 
                                                            'Profession- changed, employer- changed') ~'Exit',
                            lateral == 'Yes' ~ 'Yes',
                            lateral == 'No'~ 'No'))



table(ru2$promotion, ru2$lateral, useNA ='always')


# final mobility measure --------------------------------------------------

ru2 <- ru2 %>% 
  mutate(mob_final = case_when(promotion=='Yes' ~ 'Promotion',
                               lateral == 'Yes' & promotion != 'Yes'~ 'Lateral',
                               promotion == 'No' & lateral =='No' ~ 'Same',
                               promotion =='Exit' | lateral == 'Exit' ~ 'Exit'))



table(ru2$mob_final)


##relevel
ru2$promotion <- factor(ru2$promotion, levels = c('No', 'Yes', 'Exit'))
ru2$lateral <- factor(ru2$lateral, levels = c('No', 'Yes', 'Exit'))
ru2$mob_final <- factor(ru2$mob_final, levels = c('Same', 'Promotion', 'Lateral', 'Exit'))

levels(ru2$promotion)
levels(ru2$lateral)
levels(ru2$mob_final)



# view and explore --------------------------------------------------------


ru2 %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=mob_final, fill=gender))+
  geom_bar(position = 'dodge', aes(y=..prop.., group=gender))+
  coord_flip()


ru2 %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=round, fill=mob_final))+
  geom_bar(position = 'fill', aes(group=mob_final))

ru2


# create a "balanced" group/panel for brief comparison --------------------
ru3 <- ru2 %>% 
  filter(n==5)

prop.table(table(ru2$mob_final,ru2$round),2)
prop.table(table(ru3$mob_final,ru3$round),2)


ru2 %>% 
  filter( !is.na(mob_final),
          income < 1000000) %>% 
  ggplot(aes(x=mob_final, fill=mob_final))+
  geom_boxplot(aes(y=income))+
  coord_flip()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)



# model -------------------------------------------------------------------

ru3 <- ru3 %>% 
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
ru2

ru2$mob_final <- factor(ru2$mob_final, levels = rev(levels(ru2$mob_final)))
levels(ru2$mob_final)

ru2 %>% 
  filter(age<65,
         age>18,
         !is.na(mob_final)) %>% 
  ggplot(aes(x=age, fill=factor(mob_final)))+
  geom_histogram(binwidth = 3, position = 'fill')+
  geom_hline(yintercept=0.5, alpha=0.3, size=2)+
  theme_minimal()+
  facet_wrap(~gender)



table(ru2$newjob, ru2$promotion, ru2$lateral, useNA= 'always')

