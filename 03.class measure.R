# use ru3, not ru2 --------------------------------------------------------

ru3 <- tbl_df(ru2)


# you have to go back and include the measures below in 01 ----------------


library(tidyverse)
library(forcats)
library(ggplot2)



# load the data -----------------------------------------------------------

x

y <-  x %>%
  select(round, tid, idind,
         ixinclmo, ixmarist, ixgender,
         ixmainoc, ixilpjb8, ixpjemps,
         ixprisub, ixnpsub) 


y %>% count(ixnpsub)
y %>% count(ixilpjb8)
y %>% count(ixprisub)


# Social position and converting from isco08 to isco88 --------------------

y <- y %>% 
  mutate(isco08 = ixilpjb8/100, na.rm=TRUE,
         isco08 = round(isco08, 0))


y %>% count(isco08)
# converting measures to factors ------------------------------------------

y$econ <- factor(y$ixmainoc, levels = unique(y$ixmainoc))
y$super <- factor(y$ixprisub, levels = unique(y$ixprisub))


y %>% 
  count(ixmainoc)

# labels and coding -------------------------------------------------------

y <- y %>% 
  mutate(econ = fct_recode(econ,
                           'Inactive' = '1',
                           'Inactive' = '2',
                           'Inactive' = '3',
                           'Inactive' = '4',
                           'Inactive' = '5',
                           'Inactive' = '6',
                           'Inactive' = '7',
                           'Unemployed' = '8',
                           'Unemployed' = '9',
                           'Self-employed' = '10',
                           'Self-employed' = '11',
                           'Employed' = '12',
                           'Employed' = '13'))


y <- y %>% 
  mutate(super = fct_recode(super,
                           'Yes' = '1',
                           'No' = '2'))

y <- y %>% 
  mutate(ten_employees = ixnpsub < 10 )

table(y$ixnpsub,y$ten_employees)


# fix tags ----------------------------------------------------------------

y$econ <- factor(y$econ, levels = c('Employed','Self-employed',
                                    'Unemployed', 'Inactive'))

y$super <- factor(y$super, levels = c('Yes','No'))




# esec categories ---------------------------------------------------------
y %>% count(econ)

table(y$super,y$econ)

y <- y %>%
  mutate(econstat = case_when(econ=='Self-employed' & ten_employees==FALSE ~'Employer, large',
                              econ=='Self-employed' & ten_employees==TRUE ~ 'Employer, small',
                              econ=='Self-employed' & super == 'No' ~ 'Self-employed, no employees',
                              econ=='Employed' & super == 'Yes' ~ 'Supervisors, managers',
                              econ=='Employed' & super == 'No' ~ 'Regular workers, employees'))


y %>% 
  count(econ, econstat)

y$econstat <- factor(y$econstat, levels = c('Regular workers, employees',
                                            'Supervisors, managers',
                                            'Self-employed, no employees',
                                            'Employer, small',
                                            'Employer, large'))



y<- y%>% mutate(esec = case_when(econstat=='Employer, large' & 
                          (isco08 != 54| isco08 != 63 | !is.na(isco08))~ 'Large employers, higher mgrs/prof',
                          isco08 %in% c(1:3, 11, 20:21, 24, 26:27) ~'Large employers, higher mgrs/prof',
                          isco08 ==12 & (econstat=='Supervisors, managers'| 
                                        econstat=='Regular workers, employees')~'Large employers, higher mgrs/prof',
                          isco08 ==13 & (econstat=='Supervisors, managers')~'Large employers, higher mgrs/prof',
                          isco08 %in% c(12:14) & econstat=='Employer, large' ~'Large employers, higher mgrs/prof',
                          isco08 %in% c(22:23, 25, 31:33) ~ 'Lower mgrs/prof, higher superv',
                          isco08 %in% c(13,14) & econstat %in% c('Supervisors, managers', 'Regular workers, employees')~'Lower mgrs/prof, higher superv',
                          isco08 %in% c(30) & econstat=='Supervisors, managers' ~'Lower mgrs/prof, higher superv',
                          isco08 %in% c (34,35, 40:43) & econstat=='Supervisors, managers'~'Lower mgrs/prof, higher superv',
                          isco08 %in% c (30) & econstat == 'Regular workers, employees' ~ 'Intermediate occupations',
                          isco08 %in% c(34, 35, 40:44) & econstat == 'Regular workers, employees' ~ 'Intermediate occupations',
                          isco08 %in% c(54:52) & econstat %in% c('Self-employed, no employees','Employer, small','Employer, large', 'Supervisors, managers') ~'Intermediate occupations',
                          isco08 %in% c(12, 13, 14, 30, 34, 53, 70, 91, 94, 96) & econstat %in% c('Self-employed, no employees','Employer, small') ~ 'Small employers and self-emp (non-agriculture)',
                          isco08 %in% c(60, 62, 92, 93) & econstat %in% c('Self-employed, no employees','Employer, small') ~ 'Small employers and self-emp (agriculture)',
                          isco08 %in% c(63) & econstat %in% c('Employer, small', 'Supervisors, managers') ~ 'Small employers and self-emp (agriculture)',
                          isco08 %in% c(42, 44, 62, 70, 96) & econstat %in% c('Supervisors, managers') ~ 'Lower supervisors and technicians',
                          isco08 %in% c(42, 50:54) & econstat %in% c('Regular workers, employees') ~ 'Lower sales and service',
                          isco08 %in% c(60, 61, 62, 70:75) & econstat %in% c('Regular workers, employees') ~ 'Lower technical',
                          isco08 %in% c(80:83, 90:96) & econstat %in% c('Regular workers, employees') ~ 'Routine'))



y$esec <- factor(y$esec, levels = c("Large employers, higher mgrs/prof", 
                                    "Lower mgrs/prof, higher superv", 
                                    "Intermediate occupations",
                                    "Small employers and self-emp (non-agriculture)",
                                    "Small employers and self-emp (agriculture)",
                                    "Lower supervisors and technicians",
                                    "Lower sales and service","Lower technical",
                                    "Routine"))

levels(y$esec)

y %>% 
  filter(!is.na(esec)) %>% 
  ggplot(aes(x=esec))+
  geom_bar(position='stack',
           aes(y=..prop.., group=ixgender, fill=ixgender))+
  coord_flip()



y %>% 
  filter(!is.na(esec),
         ixinclmo<1000000) %>% 
  ggplot(aes(ixinclmo, colour=esec, fill=esec))+
  geom_density(alpha=0.1)+
  scale_x_log10()

table(y$esec, y$isco08, useNA = 'always')

y$ixinclmo[which(is.nan(y$ixinclmo))] = NA
y$ixinclmo[which(y$ixinclmo==Inf)] = NA

options(na.action="na.omit")

y$log_inc <- log(y$ixinclmo) 

y <- y %>% 
  filter(log_inc>0)

m1<- lm(data = y, log_inc~ esec)


summary(m1)


# simplified esec categories ---------------------------------------------------

y <- y %>% 
  mutate(esec_simple = fct_recode(esec,
                           'High' = "Large employers, higher mgrs/prof",
                           'High' = "Lower mgrs/prof, higher superv",
                           'Medium' = "Intermediate occupations",
                           'Medium' = "Small employers and self-emp (non-agriculture)",
                           'Medium' = "Small employers and self-emp (agriculture)",
                           'Low' = "Lower supervisors and technicians",
                           'Low' = "Lower sales and service","Lower technical",
                           'Low' = "Lower technical",
                           'Low' = "Routine"))


y %>% 
  count(esec, esec_simple)

y %>% 
  filter(!is.na(esec)) %>% 
  ggplot(aes(esec))+
  geom_bar(aes(y=..prop..,group=1))+
  coord_flip()+
  facet_wrap(~esec_simple)

