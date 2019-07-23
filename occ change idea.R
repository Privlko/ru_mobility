
# package space -----------------------------------------------------------

library(tidyverse)
library(lme4)
library(ggplot2)
library(plm)
library(dplyr)

# load the data -----------------------------------------------------------

load('C:/Users/Ivan/Desktop/dir/papers/ru_mobility/data.Rda')

?plm


# quick plot --------------------------------------------------------------
ggplot(q2)+
  geom_bar(aes(x=mob, 
               y=..prop.., 
               group=1),
           position='dodge')+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1 , by=.1))+
  labs(title=my_title,
       subtitle = my_subtitle,
       caption= my_caption,
       y= '',
       x='Mobility Type')


# two digit and three digit isco's ----------------------------------------

q2<- q2 %>% 
  mutate(occ_three_digit = as.integer(occ/10),
         occ_two_digit = as.integer(occ_three_digit/10)) %>% 
  filter(occ < 9999,
         occ > 1000)
  

# declare data as panel ---------------------------------------------------

q2 <- pdata.frame(q2, 
                  index = c("id", "round"),
                  drop.index = FALSE)


ggplot(q2, aes(occ)) +
  geom_histogram()

# code the occupational mobility measures -----------------------------------------------------
q2$diff_4digit_occ <- diff(q2$occ, 1)
q2$diff_3digit_occ <- diff(q2$occ_three_digit, 1)
q2$diff_2digit_occ <- diff(q2$occ_two_digit, 1)


q2 <- q2 %>% 
  mutate(occ.change_4 = case_when(diff_4digit_occ ==0 ~ "No change",
                                  diff_4digit_occ > 0 ~ "Upward change",
                                  diff_4digit_occ < 0 ~ "Downward change")) %>% 
  filter(!is.na(diff_4digit_occ))

q2 <- q2 %>% 
  mutate(occ.change_2 = case_when(diff_2digit_occ ==0 ~ "No change",
                                  diff_2digit_occ > 0 ~ "Upward change",
                                  diff_2digit_occ < 0 ~ "Downward change")) %>% 
  filter(!is.na(diff_2digit_occ))



ggplot(q2, aes(mob)) + geom_bar(aes(fill = occ.change_2), position = "identity")

ggplot(q2, aes(x = mob, fill = occ.change_2)) +
  geom_bar()

tab_cnt <- table(q2$occ.change_2, q2$mob)
tab_cnt

prop.table(tab_cnt, 2) 

ggplot(q2, aes(x = mob, fill = occ.change_2)) +
  geom_bar(position = "fill") +
  ylab("proportion")+
  facet_wrap(~gender)

ggplot(q2, aes(x = occ.change_2, fill = mob)) +
  geom_bar(position = "fill") +
  ylab("proportion")



my_title <- 'Job mobility is uncommon among the panel, \nmost respondents do not list a job change in the last 12 months'
my_subtitle <- 'Although the data measures repeat observations, most observations report no job change.'
my_caption <- 'Source: RLMS \nPlot: @privlko'

