library(ggplot2)
library(tidyverse)
library(styler)
library(lintr)
library(survival)
library(broom)
library(gtools)


lintr::lint('C:/Users/Ivan.Privalko/Documents/ru_mobility/01.putting waves together')
lintr::lint("test.R")


ru_data %>% 
  count(round)

library(foreign)


write.dta(ru_data,  "C:/Users/Ivan.Privalko/Documents/ru_mobility/data1.dta")


## Not run: clogit(case ~ spontaneous + induced + strata(stratum), data=infert)

# A multinomial response recoded to use clogit
#  The revised data set has one copy per possible outcome level, with new
#  variable tocc = target occupation for this copy, and case = whether
#  that is the actual outcome for each subject.
# See the reference below for the data.


logan

resp <- levels(logan$occupation)

resp

n <- nrow(logan)

n

indx <- rep(1:n, length(resp))
indx

logan2 <- data.frame(logan[indx,],
                     id = indx,
                     tocc = factor(rep(resp, each=n)))



logan2

logan2$case <- (logan2$occupation == logan2$tocc)
logan2



clogit(case ~ tocc + tocc:education + strata(id), logan2)


ru_data <- readRDS("~/ru_mobility/ru_data.rds")

ru_data <- tbl_df(ru_data)

cl1 <- ru_data %>% 
  filter(mob_final=='Same' | mob_final=='Exit') %>% 
  mutate( quit = ifelse(mob_final =='Exit', 1, 0))


cl1M <- cl1 %>% 
  mutate( quit = ifelse(mob_final =='Exit', 1, 0)) %>% 
  filter(gender=='Male')

cl1F <- cl1 %>% 
  mutate( quit = ifelse(mob_final =='Exit', 1, 0)) %>% 
  filter(gender=='Female')


clogit(quit ~ age + marr + hours + strata(idind), cl1) %>% 
  tidy() %>% 
  mutate(signif = stars.pval(p.value),
         exp1=exp(estimate)) %>% 
  select(term, estimate, exp1, signif) 


clogit(quit ~ age + marr + hours + strata(idind), cl1M) %>% 
  tidy() %>% 
  mutate(signif = stars.pval(p.value)) %>% 
  select(term, estimate, signif) 

clogit(quit ~ age + marr + hours + strata(idind), cl1F) %>% 
  tidy() %>% 
  mutate(signif = stars.pval(p.value),
         exp1 = exp(estimate)) %>% 
  select(term, estimate, exp1, signif) 



clogit(quit ~ age + marr + hours + super + esec_simple + strata(idind), cl1) %>% 
  tidy() %>% 
  mutate(signif = stars.pval(p.value),
         exp1=exp(estimate)) %>% 
  select(term, estimate, exp1, signif) 













