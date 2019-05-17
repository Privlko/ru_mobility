
# THIS FILE IS OUT OF DATE, UPDATE ----------------------------------------



library(tidyverse)
library(forcats)
library(ggplot2)
library(data.table)



# load the data -----------------------------------------------------------

x

y <-  x %>%
  select(round, idind, wage=ixwagelm,
         income=ixinclmo, marr_stat=ixmarist, gender=ixgender, 
         promotion=ixpromot, lateral=ixmovao, downward=ixmovlp,
         mob_type=ixnewjob) 

# need to revisit this below ----------------------------------------------



y %>% count(ixpromot, ixmovao)
y %>% count(ixnewjob)
y %>% count(ixnewjob, ixpromot, ixmovao)


# convert mobility measures to factors ------------------------------------

y$mob_type <- factor(y$ixnewjob, levels = unique(y$ixnewjob))
y$promotion <- factor(y$ixpromot, levels = unique(y$ixpromot))
y$lateral <- factor(y$ixmovao, levels = unique(y$ixmovao))


y %>% 
  count(lateral, promotion)


y %>% 
  count(ixmovao)

# change the labels or add some labels ------------------------------------

y <- y %>% 
  mutate(promotion = fct_recode(promotion,
                            'Yes' = '1',
                            'No' = '2'),
         lateral = fct_recode(lateral,
                              'Yes' = '1',
                              'No' = '2'),
         mob_type = fct_recode(mob_type,
                              'Profession and employer, the same' = '1',
                              'Profession- changed, employer- same' = '2',
                              'Profession- same, employer- changed' = '3',
                              'Profession- changed, employer- changed' = '4'),
         gender = fct_recode(factor(ixgender),
                             'Male' = '1',
                             'Female' = '2'))

y$mob_type <- factor(y$mob_type, levels = c("Profession and employer, the same", 
                                            "Profession- changed, employer- same",
                                            "Profession- same, employer- changed",
                                            "Profession- changed, employer- changed"))

##there's an issue with promotion and lateral moves, they only apply to 
#respondents in the same job or respondents with the same employer


y <- y %>% 
  mutate(promotion = case_when(is.na(promotion) & mob_type %in% c('Profession- same, employer- changed', 
                                                              'Profession- changed, employer- changed') ~'Exit',
                         promotion == 'Yes' ~ 'Yes',
                         promotion == 'No'~ 'No'),
         lateral =case_when(is.na(lateral) & mob_type %in% c('Profession- same, employer- changed', 
                                                              'Profession- changed, employer- changed') ~'Exit',
                            lateral == 'Yes' ~ 'Yes',
                            lateral == 'No'~ 'No'))



table(y$promotion, y$lateral, useNA ='always')


# final mobility measure --------------------------------------------------

y <- y %>% 
  mutate(mob_final = case_when(promotion=='Yes' ~ 'Promotion',
                               lateral == 'Yes' & promotion != 'Yes'~ 'Lateral',
                               promotion == 'No' & lateral =='No' ~ 'Same',
                               promotion =='Exit' | lateral == 'Exit' ~ 'Exit'))



table(y$mob_final)


##relevel
y$promotion <- factor(y$promotion, levels = c('No', 'Yes', 'Exit'))
y$lateral <- factor(y$lateral, levels = c('No', 'Yes', 'Exit'))
y$mob_final <- factor(y$mob_final, levels = c('Same', 'Promotion', 'Lateral', 'Exit'))

levels(y$promotion)
levels(y$lateral)
levels(y$mob_final)

y %>% 
  count(mob_type, prom1, lateral)

y %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=mob_final, fill=gender))+
  geom_bar(position = 'dodge', aes(y=..prop.., group=gender))+
  coord_flip()


y %>% 
  filter( !is.na(mob_final),
          ixinclmo < 1000000) %>% 
  ggplot(aes(x=mob_final, fill=mob_final))+
  geom_boxplot(aes(y=ixinclmo))+
  coord_flip()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)


y %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=mob_final, fill=mob_final))+
  geom_boxplot(aes(y=ixwagelm))+
  coord_flip()+
  scale_y_log10(labels = scales::comma)


# model -------------------------------------------------------------------

y <- y %>% 
  mutate(log_wage = log(ixwagelm)) %>% 
  filter(log_wage > 0)


m1 <- lm(data= y, log_wage ~ mob_type)
m2 <- lm(data= y, log_wage ~ promotion+lateral)
m3 <- lm(data= y, log_wage ~ promotion+lateral+mob_type)
m4 <- lm(data= y, log_wage ~ mob_final + gender)


coef(m1)
summary(m1)

coef(m2)
summary(m2)

coef(m3)
summary(m3)

summary(m4)

table(y$mob_type, y$promotion, y$lateral, useNA= 'always')

