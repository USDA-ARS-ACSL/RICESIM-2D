# Script to automate comparisons of 2DSOIL output with g plant-1
# Not sure exactly what I want to do yet
library(dplyr)
library(lubridate)
library(tidyverse)

#initials info
eomult = 0.5
poprow = 3
row_spacing = 20
p_dens = 15
popslab = poprow/100 * eomult #plants / slab
#read in comma delimited model simulation output files

#!Ricesim
pcname = "//arsmdbe34423275/E/RICESIM-2D/" #work office pc
#pcname = "c://users/david.fleisher/source/repos/" #work laptop


read_g01 <- read.table(paste0(pcname,"RICESIM-2D/testsim/test.g01"),header = TRUE, sep = ',')
#read_g03 <- read.table(paste0(pcname,"RICESIM-2D/testsim/test.g03"),header = TRUE, sep = ',')
#read_g04 <- read.table(paste0(pcname,"RICESIM-2D/testsim/test.g04"),header = TRUE, sep = ',')
#read_gasex <- read.table(paste0(pcname,"RICESIM-2D/testsim/gasexchange.crp"),header = TRUE, sep = ',')

#plots for analysis with g01 trials

## N fraction in each organ
test = read_g01
test$cum_Ncorrect = test$cumN + 0.000625 #correct for initial N content at emergence
ggplot(test, aes(das,leafN)) +
  geom_point(color = 'red')+
  geom_point(aes(das,stemN),color = 'blue') +
  geom_point(aes(das,rootN),color = 'green')+
  geom_point(aes(das,storeN),color = 'brown')

## total N content in plant and cumulative N uptake

use = test[test$das<40,]
ggplot(test, aes(das,Nplant)) +
  geom_point(color = 'red') +
  geom_point(aes(das,cum_Ncorrect), color = 'blue')


# biomass data
ggplot(test, aes(das,LAI)) +
  geom_point(color = 'green')
ggplot(test, aes(das,grnlf_)) +
  geom_point(color = 'green')

ggplot(test, aes(DVS,LAI)) +
  geom_point(color = 'green')
ggplot(test, aes(DVS,grnlf_)) +
  geom_point(color = 'green')



ggplot(test, aes(das, tot_dw))+
  geom_point(color = 'red') +
  geom_point(aes(das,grain_), color = 'blue') +
  geom_point(aes(das,grnlf_), color = 'green') +
  geom_point(aes(das,stem_d), color = 'brown') +
  geom_point(aes(das,stemre), color = 'yellow')+
  geom_point(aes(das,dead_d), color = 'black')


ggplot(test, aes(DVS, tot_dw))+
  geom_point(color = 'red') +
  geom_point(aes(DVS), color = 'blue') +
  geom_point(aes(DVS,grnlf_), color = 'green') +
  geom_point(aes(DVS,stem_d), color = 'brown') +
  geom_point(aes(DVS,stemre), color = 'yellow')+
  geom_point(aes(DVS,dead_d), color = 'black')
#compare root mass from 2DSOIL conversion with g01

g4_3 <- merge(x=read_g04, y=read_g03, by = c("Date_time","X","Y"), all.x = TRUE)

g4_3$rootdm = (g4_3$RMassM + g4_3$RMassY)*g4_3$Area #RMass is mature or young root desnsity g cm-2 at each node;;;Note that RDenY is cm root per cm2 at aeah node

g4_3_root <- g4_3 %>% group_by(Date_time) %>%
  summarize(
    rootdm = sum(rootdm)/popslab #g plant at beginning of the day
  ) %>% mutate(
    doy = Date_time - 36526, # crude way of getting day of year in 2020, couldn't figure out conversion
    hr = as.integer((doy - floor(doy))* 24+1.1),
    doy = as.integer(doy)
  )



#SPUDSIM
#initials info
eomult = 0.5
poprow = 4.875
row_spacing = 75
p_dens = 6.5
popslab = poprow/100 * eomult #plants / slab
#read in comma delimited model simulation output files

#!Ricesim
pcname = "//arsmdbe34423275/E/SPUDSIM v2-br1-resp/release_version/" #work office pc
#pcname = "c://users/david.fleisher/source/repos/" #work laptop


read_g01 <- read.table(paste0(pcname,"paulmanfarms_NEpotato_release/paulmanfarms_NE.g01"),header = TRUE, sep = ',')

et_mm = with(read_g01, read_g01[date %in% c('08/01/2022'),])
sum(et_mm$Tr.Act)


et_mm


