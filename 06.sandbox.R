# sidenote: do not run, set to panel ------------------------------------------------------------
library(plm)
library(tidyverse)

ru3 %>% count(round)

ru3 %>% count(esec_simple)

ru3 %>% 
  group_by(idind) %>% 
  mutate(l.esec_simple = lag(esec_simple)) %>% 
  ungroup() %>% 
  select(idind, round, esec, esec_simple, l.esec_simple) %>% 
  count(esec_simple, l.esec_simple) %>% 
  mutate(tot=sum(n),
         per= n/tot)




View(ru4)
ru4  <- ru3 %>%
  mutate(logwage = log(wage)) %>% 
  filter(logwage > 0,
         !is.na(mob_final),
         !is.na(esec_simple),
         !is.na(age),
         !is.na(sub),
         !is.na(firm_size))%>%
  group_by(idind) %>% 
  mutate(n=n()) %>% 
  ungroup(idind) %>%
  filter(n>4) 

ru4 %>% 
  filter(idind<1000) %>% 
  ggplot(aes(x=idind, y= wage)) +
  geom_point(aes(colour=factor(round)))+
  geom_hline(yintercept=mean(ru4$wage), linetype='dashed', 
             color='darkred', size=2)


m1 <- lm(wage~ 1 ,data=ru4)

summary(m1)

broom::tidy(m1)

broom::augment(m1, ru4) %>%
  select(idind, round, .resid, wage) %>% 
  filter(idind<1000) %>%
  group_by(idind) %>% 
  mutate(lag_res= lag(.resid)) %>%
  ungroup() %>% 
  ggplot(aes(x=.resid, y=lag_res))+
  geom_point(aes(colour=factor(round)))
