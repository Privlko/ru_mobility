library(tidyverse)
library(plm)

##writing a function


# year 2015 ---------------------------------------------------------------


load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2015x.RData")

ru15 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = ixsatisp,
         gender = ixgender, 
         region = regionx, 
         income = ixinclmo,
         respect = ixresprk,
         rank = ixeconrk,
         power = ixpowrnk,
         marr = ixmarist,
         promo = ixpromot,
         newjob= ixnewjob,
         occ=ixilpjb8,
         wage = ixwagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "99999996")) %>%
  mutate(promo =  as.numeric(promo == "1"))


(as.tibble(ru15))



# year 2014 ---------------------------------------------------------------


load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2014w.RData")


ru14 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = iwsatisp,
         gender = iwgender, 
         region = regionw, 
         income = iwinclmo,
         respect = iwresprk,
         rank = iweconrk,
         power = iwpowrnk,
         marr = iwmarist,
         promo = iwpromot,
         newjob= iwnewjob,
         occ = iwilopjb ,
         wage = iwwagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "5")) %>%
  mutate(promo =  as.numeric(promo == "1"))

(as.tibble(ru14))


# year 2013 ---------------------------------------------------------------

load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2013v.RData")

ru13 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = ivsatisp,
         gender = ivgender, 
         region = regionv, 
         income = ivinclmo,
         respect = ivresprk,
         rank = iveconrk,
         power = ivpowrnk,
         marr = ivmarist,
         promo = ivpromot,
         newjob= ivnewjob,
         occ = ivilopjb,
         wage = ivwagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "99999996")) %>%
  mutate(promo =  as.numeric(promo == "1"))


(as.tibble(ru13))



# year 2012 ---------------------------------------------------------------


load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2012u.RData")

ru12 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = iusatisp,
         gender = iugender, 
         region = regionu, 
         income = iuinclmo,
         respect = iuresprk,
         rank = iueconrk,
         power = iupowrnk,
         marr = iumarist,
         promo = iupromot,
         newjob= iunewjob,
         occ = iuilopjb,
         wage = iuwagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "99999996")) %>%
  mutate(promo =  as.numeric(promo == "1"))


(as.tibble(ru12))

# year 2011 ---------------------------------------------------------------

load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2011t.RData")

ru11 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = itsatisp,
         gender = itgender, 
         region = regiont, 
         income = itinclmo,
         respect = itresprk,
         rank = iteconrk,
         power = itpowrpk,
         marr = itmarist,
         promo = itpromot,
         newjob= itnewjob,
         occ = itilopjb,
         wage = itwagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "99999996")) %>%
  mutate(promo =  as.numeric(promo == "1"))


(as.tibble(ru11))
View(ru11)


# year 2010 ---------------------------------------------------------------

load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2010s.RData")


ru10 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = issatisp,
         gender = isgender, 
         region = regions, 
         income = isinclmo,
         respect = isresprk,
         rank = iseconrk,
         power = ispowrnk,
         marr = ismarist,
         promo = ispromot,
         newjob= isnewjob,
         occ = isilopjb,
         wage = iswagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "99999996")) %>%
  mutate(promo =  as.numeric(promo == "1"))


(as.tibble(ru10))
View(ru10)

# year 2009 ---------------------------------------------------------------

load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2009r.RData")


ru09 <- x %>% 
  select(round, 
         id = idind, 
         pay.sat = irsatisp,
         gender = irgender, 
         region = regionr, 
         income = irinclmo,
         respect = irresprk,
         rank = ireconrk,
         power = irpowrnk,
         marr = irmarsta,
         promo = irpromot,
         newjob= irnewjob,
         occ= irilopjb,
         wage = irwagelm) %>% 
  filter(income < 610000) %>% 
  filter(respect !=52) %>% 
  mutate(gender = fct_recode(as.factor(gender),
                             "M"="1",
                             "F"="2")) %>% 
  mutate(pay.sat = fct_recode(as.factor(pay.sat),
                              "Absolutely satisfied"="1",
                              "Satisfied"="2",
                              "Neither" = "3",
                              "Unsatisfied" = "4",
                              "Absolutely unsatisfied" = "5")) %>% 
  mutate(marr = fct_recode(as.factor(marr),
                           "never married" = "1",
                           "First" = "2",
                           "Second"="3",
                           "Divorced"="4",
                           "widowed"="5",
                           "separated"="6")) %>%
  mutate(newjob = fct_recode(as.factor(newjob),
                             "No change" = "1",
                             "Change prof, not place" = "2",
                             "Change place, not prof"="3",
                             "Change both"="4",
                             "Other" = "99999996")) %>%
  mutate(promo =  as.numeric(promo == "1"))


(as.tibble(ru09))

# bind rows ---------------------------------------------------------------

q1 <- rbind(ru15, ru14, ru13, ru12, ru11, ru10, ru09)

(as.tibble(q1))

q1 %>% 
  count(marr)

qq<-q1 %>% 
  count(occ)



q1$promo[is.na(q1$promo)] <- 0

q2 <- q1 %>% 
  mutate(mob = case_when(promo== 0  & (newjob == "No change" | newjob== "Change prof, not place") ~ "No change",
                         promo== 1 & (newjob == "No change" | newjob== "Change prof, not place") ~ "Promo",
                         promo== 0 & (newjob == "Change place, not prof") ~ "Quit")) %>% 
  filter(!is.na(mob))



q2 %>% 
  count(mob)

save(q2,file="C:/Users/Ivan/Desktop/dir/papers/ru_mobility/data.Rda")
load("C:/Users/Ivan/Desktop/dir/papers/ru_mobility/data.Rda")



# set to panel ------------------------------------------------------------

(p1 <- pdata.frame(q2, c("id","round"), row.names = TRUE))

##some clean up and variable coding?

ggplot(p1, aes(y=..prop..,
               x=mob,
               group=1)) + 
         geom_bar()+
  facet_wrap(~round)

View(p1)

?plm

p1$occ_diff <- diff(p1$occ)

##you're good to go
