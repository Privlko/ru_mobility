use "C:\Users\Ivan.Privalko\Documents\ru_mobility\data1.dta", clear

drop if job_year == 2016 | job_year==2077

tab job_year,miss

xtset idind round
xtdescribe

tab mob_final
tab mob_final,nolabel


keep if mob_final==1 | mob_final==4

tab mob_final, gen(quit)

tab quit*

tab quit1

tab quit2


clogit quit2 c.age i.marr  c.hours , group(idind) or

clogit quit2 c.age i.marr  c.hours i.super i.esec_simple , group(idind) or
clogit quit2 c.age i.marr  c.hours i.super i.esec_simple if gender==1, group(idind) or
clogit quit2 c.age i.marr  c.hours i.super i.esec_simple if gender==2, group(idind) or


xtdescribe if e(sample)
margins marr, predict() at(age=(20(5)35))
marginsplot


tab firm_size

margins, predict() at(age=(20(5) 40))
marginsplot

xttab quit2

xtdescribe
tab hours, nolabel

recode hours (max=38)

tab hours
