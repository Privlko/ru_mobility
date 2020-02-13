library(forcats)
library(tidyverse)
library(broom)
library(gtools)
library(ggrepel)



# load the data, don't use ru1!! -----------------------------------------------------------
ru2 <- tbl_df(ru1)
ru2




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




ru2 %>% 
  count(promotion)

table(ru2$promotion, ru2$lateral, useNA ='always')



# final mobility measure --------------------------------------------------

ru2 <- ru2 %>% 
  mutate(mob_final = case_when(promotion=='Yes' ~ 'Promotion',
                               lateral == 'Yes' & promotion != 'Yes'~ 'Lateral',
                               promotion == 'No' & lateral =='No' ~ 'Same',
                               promotion =='Exit' | lateral == 'Exit' ~ 'Exit'))



table(ru2$mob_final)

prop.table(table(ru2$mob_final, ru2$gender),2)


##relevel
ru2$promotion <- factor(ru2$promotion, levels = c('No', 'Yes', 'Exit'))
ru2$lateral <- factor(ru2$lateral, levels = c('No', 'Yes', 'Exit'))
ru2$mob_final <- factor(ru2$mob_final, levels = c('Same', 'Promotion', 'Lateral', 'Exit'))

levels(ru2$promotion)
levels(ru2$lateral)
levels(ru2$mob_final)



# refactor measures for "ever" moved and "never" moved --------------------
ru2

ru2 <- ru2 %>% 
  mutate(ever_promoted= fct_recode(factor(ever_promoted),
                                   'Yes' = '1',
                                   'No' = '2'),
         ever_lateral=fct_recode(factor(ever_lateral),
                                  'Yes' = '1',
                                  'No' = '2'),
         ever_lowered=fct_recode(factor(ever_lowered),
                                 'Yes' = '1',
                                 'No' = '2'))

ru2$ever_promoted <- factor(ru2$ever_promoted, levels = c('No', 'Yes'))
ru2$ever_lateral <- factor(ru2$ever_lateral, levels = c('No', 'Yes'))
ru2$ever_lowered <- factor(ru2$ever_lowered, levels = c('No', 'Yes'))

# view and explore --------------------------------------------------------
ru2

theme_set(theme_bw())





# employer tenure ---------------------------------------------------------

ru2 %>% 
  count(year)


ru2 %>% 
  count(job_year)

ru2 <- ru2 %>% 
  filter(job_year < 2016) %>% 
  mutate(tenure = year - job_year) 


ru2 %>% 
  ggplot(aes(x=job_year))+
  geom_density()


# mobility rate by gender -------------------------------------------------

theme_set(theme_bw())



# using standard errors & confidence intervals---------------------------------------------------


sum1 <- ru2 %>%
filter(!is.na(mob_final)) %>% 
  group_by(gender, mob_final) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n),
         prop_dev =prop*(1-prop),
         se=sqrt(prop_dev/n),
         conf_up = prop + (se*1.96),
         conf_low = prop -(se*1.96))  
  
right_label <- sum1 %>%
  filter(gender=='Male')

left_label <- sum1 %>% 
  filter(gender=="Female")

sum1 %>% 
ggplot(aes(x=mob_final,
             y=prop,
             colour=gender))+
  geom_point(aes(size=0.3))+
  geom_errorbar(aes(ymin=conf_up,
                    ymax=conf_low),
                width=0.3,
                size=0.3)+
  labs(title = "Proportion of respondents citing mobility type by gender",
       subtitle= "Estimates contain 95% confidence intervals (Standard error*1.96)",
       x="Mobility type",
       y="Proportion of respondents",
       caption="RLMS rounds 2015 to 2011 \nAuthors own calculation")+
  guides(size=FALSE)+
  geom_text(data = right_label, aes(color = gender, label = round(prop, 3)),
                               size = 3, hjust = -.6) +
  geom_text(data = left_label, aes(color = gender, label = round(prop, 3)),
            size = 3, hjust = 1.8)


# using mobility and wages -----------------------------------------------

sum3 <- ru2 %>%
  filter(!is.na(mob_final),
         !is.na(wage)) %>% 
  group_by(gender, mob_final) %>% 
  summarise(wage_mean = mean(wage, na.rm=TRUE),
            wage_sd = sd(wage, na.rm = TRUE),
            n= n(),
            wage_se = wage_sd/sqrt(n)) 
 
sum3

sum3 %>% 
ggplot(aes(x=mob_final, 
             y= wage_mean,
             col=gender))+
  geom_line(aes(group=gender))+
  geom_point(aes(group=mob_final,
                 size=3))+
  geom_errorbar(aes(ymin=wage_mean-(wage_se*1.96),
                    ymax=wage_mean+(wage_se*1.96)),
                width=0.25)+
  scale_y_continuous(labels = scales::comma)+
  labs(y= "Average monthly wages in rubles",
       x="Mobility type",
       caption="RLMS rounds 2015 to 2011 \nAuthors own calculation")+
  guides(size=FALSE)+
  geom_text(data = sum3, aes(color = gender, label = round(wage_mean, 0)),
            size = 3, hjust = -.6)

  


q1 <- multinom(mob_final~ gender, data=ru2)

summary(q1)

knitr::kable(tidy(q1, exponentiate = TRUE), 2) 

tidy(q1, exponentiate = T) %>% 
  mutate(signif = stars.pval(p.value),
         p.value=round(p.value, 3),
         estimate=round(estimate, 3),
         std.error=round(std.error, 3))


##mobility over time
ru2 %>% 
  filter( !is.na(mob_final)) %>% 
  ggplot(aes(x=mob_final, 
             y=..prop..,
             group=1))+
  geom_bar()+
  facet_wrap(~gender)

##earnings by promotion and gender
ru2 %>% 
  filter(!is.na(ever_promoted)) %>% 
  ggplot(aes(x=factor(ever_promoted), y=wage, fill=factor(ever_promoted)))+
  geom_boxplot()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)+
  guides(fill=FALSE)


##earnings by mobility type and gender
ru2 %>% 
  filter(!is.na(mob_final)) %>% 
  ggplot(aes(x=factor(gender), y=wage, fill=factor(gender)))+
  geom_boxplot()+
  scale_y_log10(labels = scales::comma)+
  facet_grid(.~mob_final)+
  guides(fill=FALSE)

##mobility by age and gender
ru2 %>% 
  filter( !is.na(mob_final),
          age > 20,
          age < 66) %>%
  ggplot(aes(x=age, fill=mob_final))+
  geom_bar(binwidth = 5,
           position = 'fill', aes(group=mob_final))+
  facet_wrap(~gender)


save(ru2, file = "genderpaper.RData")

ru2 %>% 
  count(mob_final)

ru2 %>% 
  filter(!is.na(mob_final)) %>% 
  group_by(gender) %>% 
  count(mob_final) %>% 
  mutate(per = n/sum(n))


  

# create a "balanced" group/panel for brief comparison --------------------
ru3 <- ru2 %>% 
  filter(n==5)

prop.table(table(ru2$mob_final,ru2$round),2)


ru2 %>% 
  filter( !is.na(mob_final),
          income < 1000000) %>% 
  ggplot(aes(x=mob_final, fill=mob_final))+
  geom_boxplot(aes(y=income))+
  coord_flip()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)

##there's an issue here, you're looking
##across individuals here.

ru2 %>% 
  filter( !is.na(ever_promoted),
          income < 1000000) %>% 
  ggplot(aes(x=factor(ever_promoted), 
             fill=factor(ever_promoted)))+
  geom_boxplot(aes(y=income))+
  coord_flip()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)

ru2 %>% 
  count(round)

# model -------------------------------------------------------------------

ru3 <- ru3 %>% 
  mutate(log_wage = log(wage)) %>% 
  filter(log_wage > 0)



m1 <- lm(data= ru3, log_wage ~ newjob)
m2 <- lm(data= ru3, log_wage ~ promotion+lateral)
m3 <- lm(data= ru3, log_wage ~ gender+mob_final)



coef(m1)
summary(m1)


coef(m2)
summary(m2)

coef(m3)
summary(m3)


ru2


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


ggplot(ru3, aes(x=round, y=wage))+
  geom_jitter(alpha=0.5)+
  geom_boxplot(alpha=0.1,
               aes(group=round))+
   scale_y_log10(labels = scales::comma)
  



# measure for ever quit ---------------------------------------------------
ru2 <- tbl_df(ru2)


ru2 %>%
  mutate(ever_exit= if_else(mob_final=='Exit', 1, 0)) %>% 
  group_by(idind) %>% 
  mutate(ever_exit = max(ever_exit)) %>% 
  ggplot(aes(x=factor(ever_exit), y=wage, fill=factor(ever_exit)))+
  geom_boxplot()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(~gender)+
  guides(fill=FALSE)


ru2 %>% 
  mutate(ever_exit= if_else(mob_final=='Exit', 1, 0)) %>% 
  group_by(idind) %>% 
  mutate(ever_exit = max(ever_exit)) %>% 
  filter( !is.na(ever_exit),
          age > 20,
          age < 66) %>%
  ggplot(aes(x=age, fill=ever_exit))+
  geom_bar(binwidth = 5,
           position = 'fill', aes(group=ever_exit))+
  facet_wrap(~gender)

