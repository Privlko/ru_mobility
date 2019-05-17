library(tidyverse)
library(data.table)


##start with 2015
##do not touch
load("~/Russia/data/adult2015x.RData")


ix <-  x %>%
  select(round, idind, wage = ixwagelm,
         income=ixinclmo, marr_stat=ixmarist, 
         gender=ixgender, promotion=ixpromot, 
         lateral = ixmovao, lower=ixmovlp, newjob = ixnewjob) 


##open 2014

iw <-  x %>%
  select(round, idind, wage = iwwagelm,
         income=iwinclmo, marr_stat=iwmarist, 
         gender=iwgender, promotion=iwpromot, 
         lateral = iwmovao, lower=iwmovlp, newjob = iwnewjob) 

##open 2013 

iv <- x %>% 
  select(round, idind, wage = ivwagelm,
             income=ivinclmo, marr_stat=ivmarist, 
             gender=ivgender, promotion=ivpromot, 
             lateral = ivmovao, lower=ivmovlp, newjob = ivnewjob) 



### open 2012 

iu <- x %>% 
  select(round, idind, wage = iuwagelm,
         income=iuinclmo, marr_stat=iumarist, 
         gender=iugender, promotion=iupromot, 
         lateral = iumovao, lower=iumovlp, newjob = iunewjob) 


ix <- tbl_df(ix)
ix
iw <- tbl_df(iw)
iw
iv <- tbl_df(iv)
iv
iu <- tbl_df(iu)
iu


ru1 <- bind_rows(ix, iw, iv, iu)



ru1 %>% 
  count(round)

ru1 %>% 
  count(idind)

ru1 %>% 
  count(promotion)

ru1 %>% 
  group_by(idind) %>% 
  tally() %>% 
  arrange(desc(n))

ru1 %>% arrange(idind, round)



