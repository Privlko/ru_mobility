library(tidyverse)


##caution; there's an issues with occupational measures.

##start with 2015
##do not touch

load("C:/Users/Ivan/Desktop/dir/data/rlms/adult2015x.RData")

ix <-  x %>%
  select(round, idind, wage = ixwagelm,
         income=ixinclmo, marr_stat=ixmarist, 
         gender=ixgender, promotion=ixpromot, 
         lateral = ixmovao, lower=ixmovlp, 
         newjob = ixnewjob, age = ixage,
         mainoc=ixmainoc, isco=ixilpjb8, firm_size=ixpjemps,
         sub=ixprisub, sub_n=ixnpsub) 


##open 2014

iw <-  x %>%
  select(round, idind, wage = iwwagelm,
         income=iwinclmo, marr_stat=iwmarist, 
         gender=iwgender, promotion=iwpromot, 
         lateral = iwmovao, lower=iwmovlp, 
         newjob = iwnewjob, age = iwage,
         mainoc= iwmainoc, isco=iwilpjb8, firm_size=iwpjemps,
         sub=iwprisub, sub_n=iwnpsub) 

##open 2013 

iv <- x %>% 
  select(round, idind, wage = ivwagelm,
             income=ivinclmo, marr_stat=ivmarist, 
             gender=ivgender, promotion=ivpromot, 
             lateral = ivmovao, lower=ivmovlp, 
         newjob = ivnewjob, age = ivage,
         mainoc=ivmainoc, isco=ivilpjb8, firm_size=ivpjemps,
         sub=ivprisub, sub_n=ivnpsub) 



### open 2012 

iu <- x %>% 
  select(round, idind, wage = iuwagelm,
         income=iuinclmo, marr_stat=iumarist, 
         gender=iugender, promotion=iupromot, 
         lateral = iumovao, lower=iumovlp, 
         newjob = iunewjob, age = iuage,
         mainoc=iumainoc, isco=iuilopjb, firm_size=iupjemps,
         sub=iuprisub, sub_n=iunpsub) 


### open 2011
it <- x %>% 
  select(round, idind, wage = itwagelm,
         income=itinclmo, marr_stat=itmarist, 
         gender=itgender, promotion=itpromot, 
         lateral = itmovao, lower=itmovlp, 
         newjob = itnewjob, age = itage,
         mainoc=itmainoc, isco=itilopjb, firm_size=itpjemps,
         sub=itprisub, sub_n=itnpsub) 


ix <- tbl_df(ix)
ix
iw <- tbl_df(iw)
iw
iv <- tbl_df(iv)
iv
iu <- tbl_df(iu)
iu
it <- tbl_df(it)
it


ru1 <- bind_rows(it, iu, iv, iw, ix)



ru1 %>% 
  count(round)

ru1 %>% 
  count(idind)

ru1 %>% 
  count(promotion)

ru1 %>% 
  count(isco)


ru1 <- ru1 %>% arrange(idind, round)



# create a measure for total number of waves experienced ------------------

ru1 <- ru1 %>% 
  group_by(idind) %>% 
  mutate(n=n())


# create a measure for ever promoted, lateral, or lower --------------------------------------

ru1 <- ru1 %>% 
  group_by(idind) %>% 
  mutate(ever_promoted = min(promotion, na.rm=TRUE),
         ever_lateral = min(lateral, na.rm=TRUE),
         ever_lowered = min(lower, na.rm=TRUE),
         never_moved = max(newjob, na.rm = TRUE))

#check
ru1 %>% 
  filter(ever_promoted==1) %>% 
  select(idind, round, promotion, ever_promoted)

