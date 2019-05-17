# data table ideas --------------------------------------------------------

set.seed(45L)

DT <- data.table(ru1)

DT[3:15,]

DT[wage==6000]
DT[wage %in% c(600, 1000, 2000)]

DT[, wage]
DT[,.(wage)]
DT[wage %in% c(6000),.(wage, newjob)]
DT

DT[,.(wage, newjob)]
DT[, .(sum(wage, na.rm=T), sd(wage, na.rm=T))]

DT[, .(big_total =sum(wage, na.rm=T), var_thing = sd(wage, na.rm=T))]
DT[, .(wage, var_thing = sd(wage, na.rm=TRUE))]


DT[,{print(promotion) 
  plot(wage) 
  NULL}]


###running commands by j groups, extremely useful

##how can we give a value of 1, to all of those who have EVER 
## been promoted? 

DT[, .(group_mean = mean(wage, na.rm=TRUE))]
DT[, .(group_mean = mean(wage, na.rm=TRUE)), by=idind]
DT[, .(ever_promoted = min(promotion)), by=idind ]
##this might be it, i just have to get it back 
##into the old format, but repeating values are recycled

DT[, .(wage_mean=mean(wage)), by=.(idind)]
DT[, .(wage_mean=mean(wage, na.rm=T)), by=.(promotion-1)]
DT[, .(wage_mean=mean(wage, na.rm=T)), by=.(promotion, gender)]

DT[, .(wage_mean=mean(wage, na.rm=T)), by=.(cut = sign(promotion-1))]


DT[200:250, .(wage_mean=mean(wage, na.rm=T)), by=.(cut = promotion)]


DT[, 
   .(count = .N, 
     average_wage=mean(wage,na.rm = T)), 
   by=.(promotion, gender)]



##that issue with getting back to the previous state of DT
## might be solved using := very useful tool
##i know how to make the column, i just have to get back to 
## the origInAL DT

DT[, .(ever_promoted = min(promotion)), by=idind ]

DT[, log_wage :=round(log(wage),2)]

DT[, ever_promoted := min(promotion), by=idind]
DT[, ever_lateral := min(lateral), by=idind]
DT[, ever_lower := min(lower), by=idind]


DT
DT[ever_promoted==1]
##success!!!!

DT[,.(mean(wage,na.rm=T)), by=promotion]

DT[,.(mean_wage=mean(wage,na.rm=T), count = .N), by=ever_promoted]
DT[,.(mean_wage=mean(wage,na.rm=T), count = .N), by=ever_lateral]
DT[,.(mean_wage=mean(wage,na.rm=T), count = .N), by=ever_lower]

DT[, tall_ie := .N, by=idind]
DT

DT[tall_ie==4, .(mean_wage=mean(wage,na.rm=T), count = .N), by=ever_promoted]
DT[tall_ie==4, .(mean_wage=mean(wage,na.rm=T), count = .N), by=ever_lateral]
DT[tall_ie==4, .(mean_wage=mean(wage,na.rm=T), count = .N), by=ever_lower]


DT[tall_ie==4]
