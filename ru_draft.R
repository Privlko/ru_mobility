library(tidyverse)

load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2014w.RData")

View(x)
y2014 <- x %>%
  select(round, 
         region=regionw,
         satisj= iwsatisj,
         satisc=iwsatisc,
         satisp=iwsatisp,
         ind=iwpriind,
         job_year =iwjobsyr,
         job_month =iwjobsmo,
         job_hours =iwpwrkhr,
         job_min =iwpwrkmn,
         int_month=iwintmon,
         wage = iwwagelm,
         job_number_emp = iwpjemps,
         employer_gov = iwentgov,
         newjob = iwnewjob,
         promo = iwpromot,
         move = iwmovao,
         downward= iwmovlp,
         id = idind,
         gndr = iwgender,
         income = iwinclmo)%>% 
  mutate(log.wage = log(wage))

(as.tibble(y2014))


hist(y2014$wage, breaks = 40)
hist(y2014$log.wage, breaks = 40)

View(y2014)

?parse_date()

y2014$tenure <-  as.Date(paste(01,y2014$job_month, 
                       y2014$job_year,
                       sep = "."),
                       format = "%d.%m.%Y")



y2014$int_year <- as.numeric(!is.na(y2014$int_month))
y2014$int_year[y2014$int_year %in% 1] <- 2015 


y2014$interview <-  as.Date(paste(01,y2014$int_month, 
                               y2014$int_year,
                               sep = "."),
                         format = "%d.%m.%Y")


y2014 <- y2014 %>% 
  mutate(durr = interview - tenure)


as.tibble(y2014$durr)


hist(y2014$tenure, breaks = 40)
hist(as.numeric(y2014$durr), breaks = 40)           

y2014 <- y2014 %>% 
  mutate(durr_sq= as.numeric(durr)*as.numeric(durr))

m1 <- lm(log.wage~durr+durr_sq,
         data = y2014)
m1
summary(m1)


ggplot(y2014, aes(x=durr,
                  y=log.wage))+
  geom_point()+
  geom_smooth()
