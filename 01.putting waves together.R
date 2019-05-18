library(tidyverse)


##start with 2015
##do not touch



ix <-  x %>%
  select(round, idind, wage = ixwagelm,
         income=ixinclmo, marr_stat=ixmarist, 
         gender=ixgender, promotion=ixpromot, 
         lateral = ixmovao, lower=ixmovlp, 
         newjob = ixnewjob, age = ixage) 


##open 2014

iw <-  x %>%
  select(round, idind, wage = iwwagelm,
         income=iwinclmo, marr_stat=iwmarist, 
         gender=iwgender, promotion=iwpromot, 
         lateral = iwmovao, lower=iwmovlp, 
         newjob = iwnewjob, age = iwage) 

##open 2013 

iv <- x %>% 
  select(round, idind, wage = ivwagelm,
             income=ivinclmo, marr_stat=ivmarist, 
             gender=ivgender, promotion=ivpromot, 
             lateral = ivmovao, lower=ivmovlp, newjob = ivnewjob, age = ivage) 



### open 2012 

iu <- x %>% 
  select(round, idind, wage = iuwagelm,
         income=iuinclmo, marr_stat=iumarist, 
         gender=iugender, promotion=iupromot, 
         lateral = iumovao, lower=iumovlp, newjob = iunewjob, age = iuage) 

### open 2011
it <- x %>% 
  select(round, idind, wage = itwagelm,
         income=itinclmo, marr_stat=itmarist, 
         gender=itgender, promotion=itpromot, 
         lateral = itmovao, lower=itmovlp, newjob = itnewjob, age = itage) 



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



ru1 <- ru1 %>% arrange(idind, round)
View(ru1)

ru1



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
         never_moved = min(newjob, na.rm = TRUE))

#check
ru1 %>% 
  filter(ever_promoted==1) %>% 
  select(idind, round, promotion, ever_promoted)

# set to panel ------------------------------------------------------------

(p1 <- pdata.frame(q2, c("id","round"), drop.index = FALSE, row.names = TRUE))







