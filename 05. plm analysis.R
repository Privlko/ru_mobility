# sidenote: do not run, set to panel ------------------------------------------------------------
library(plm)
library(tidyverse)
library(gtools)


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


ru3 %>% 
  filter(idind<100) %>% 
  ggplot(aes(x=idind, y= wage)) +
  geom_point(aes(colour=factor(round)))+
  geom_hline(yintercept=mean(ru4$wage), linetype='dashed', 
             color='darkred', size=2)+
  scale_y_log10(labels = scales::comma)

ru4 %>% 
  count(round)

ru3 %>% 
  count(round)


View(ru4)

ru3 <- ru3 %>% 
  filter(wage>0)
  
ru3 %>% count(n)
ru4 %>% count(n)
  
(p1 <- pdata.frame(ru3, c("idind","round"), drop.index = FALSE, row.names = TRUE))
(pbal <- pdata.frame(ru4, c("idind","round"), drop.index = FALSE, row.names = TRUE))



head(p1)
glimpse(p1)

View(p1)

m1 <- plm(log(wage) ~ mob_final + age, data = p1, model = "within")
m2 <- plm(log(wage) ~ mob_final + age, data = pbal, model = "within")


summary(m1)
summary(m2)

m3 <- plm(log(wage) ~ factor(marr_stat) + mob_final + age, data = p1, model = "within")
m4 <- plm(log(wage) ~ factor(marr_stat) + mob_final + age, data = pbal, model = "within")


summary(m3)
summary(m4)

m5 <- plm(log(wage) ~ factor(marr_stat) + mob_final + age + firm_size + round, data = p1, model = "within")
m6 <- plm(log(wage) ~ factor(marr_stat) + mob_final + age + firm_size + round, data = pbal, model = "within")


summary(m5)
summary(m6)


pHi <- p1 %>% 
  filter(esec_simple=='High') %>% 
  pdata.frame(c("idind","round"), drop.index = FALSE, row.names = TRUE)

  
mHi  <- plm(log(wage) ~ factor(marr_stat) + mob_final + age + firm_size + round,
             data=pHi, 
             model = "within")

  
  summary(mHi)

  
  pLo <- p1 %>% 
    filter(esec_simple=='Low') %>% 
    pdata.frame(c("idind","round"), drop.index = FALSE, row.names = TRUE)
  
  
  mLo  <- plm(log(wage) ~ factor(marr_stat) + mob_final + age + firm_size + round,
              data=pLo, 
              model = "within")
  
  
  summary(mLo)
    
View(p1)

pMid <- p1 %>% 
    filter(esec_simple=='Medium') %>% 
    pdata.frame(c("idind","round"), drop.index = FALSE, row.names = TRUE)
  

mMid  <- plm(log(wage) ~ factor(marr_stat) + mob_final + age + firm_size + round,
              data=pMid, 
              model = "within")
  
  
summary(mMid)


# presenting models -------------------------------------------------------



n1 <- broom::tidy(mHi) %>% 
  select(-statistic, -std.error) %>% 
  mutate(signif = stars.pval(p.value)) %>% 
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3))


n1 <- broom::tidy(m5) %>% 
  mutate(signif = stars.pval(p.value)) %>% 
  mutate(estimate = round(estimate, 3),
         p.value = round(p.value, 3)) %>%
  bind_cols(n1) %>% 
  select(term,
         estimate, 
         signif,
         estimate1,
         signif1) %>% 
  rename("Est (High ESEC)"= estimate1,
         "Sig (HighESEC)"= signif1)


n1  

