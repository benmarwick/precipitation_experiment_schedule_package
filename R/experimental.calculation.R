

## calculations of the number of days with set amounts of
## precipitation in the rainfall experiment
## first source in our functions
# source("../Rscripts/precipitation.functions.R")
## required packages
library("bbmle")
##library("multicore")
## read in data

## Before running this code, ensure that the working directory is Experimental.Schedules.  The
## .Rscripts (containing precipitation.functions.R) directory, and other folders
## named after the fieldsites ('Cardoso', 'FrenchGuiana' etc) should be inside ../

## these fieldsite folders can have any number of files that you need,
## but should have one and only one file of rainfall data, named
## something beginning "Ppt*". It should be correctly formatted:
## years as columns, 60 rows (representing consecutive days)

### THERE SHOULD BE NO ROW NUMBERS in the input file. BE CAREFUL!
rainfall(Times=50,site="PuertoRico")
## this will print a file named after the fieldsite, AND the computer
## on which it was run!

## print the schedule
sched.print(sim.data="PuertoRico.sim.spider01.zulu.Rdata",site="PuertoRico")

## correct Argentina
#schedule_corrector(sim.data="Argentina.sim.spider01.zulu.Rdata",site="Argentina",remove_zero_median=TRUE,final_constant=TRUE)

## run diagnostics
diagnostic.plots(sim.data="PuertoRico.sim.spider01.zulu.Rdata",site="PuertoRico")

## note two VERY IMPORTANT THINGS:
## 1. there is no check that the temporal blocks will be unique to a
## site.  Thankfully this is very unlikely.  However, you should
## probably check manually just to be sure.
## 2. temporal blocks are *** RECALCULATED ** once a schedule is
## printed!  PLEASE DO NOT EVER run graph.print() after a schedule is
## emailed to someone!

## this makes the parameter comparison graphs.
## please do make sure that you have updated its internal list of the
## sites, if you have added a new one.

site.parameter.comparison()

