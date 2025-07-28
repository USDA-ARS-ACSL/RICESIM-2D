# simple curve fitting for tauzet relationship between normalized ler and leaf water potential
# units are cm d-1 versus MPa respectively
# data from Cutler	J.	M.	K.	W.	Shahan	and	P.	L.	Steponkus	(1980).	Influence of Water Deficits and Osmotic Adjustment on Leaf Elongation in Rice1.	Crop	Science	20(3):	314-318.

#Based on results, found sf = 2.87 and phyf = -0.499 gave best fit


#read in space delimited text file ontained from above and digitized using www.graphreader.com
read_data <- read.table("//arsmdbe34423275/E/RICESIM-2D/RICESIM-2D/improvements/leaf extension vs psi/pooled_ler_psi.txt",header = TRUE)

#processing
proc <- read_data
##need to normalize LER for each variety x drought x condition first

library(tidyverse)

#find max value within each group
max <- proc %>% 
  group_by(variety, drought, condition) %>%
  slice_max(LER)
max$mLER = max$LER

proc2 <- merge(proc, max, by.x = c("variety","drought","condition"), by.y = c("variety","drought","condition"))
proc2 <- proc2 %>%
  select(-LER.y, -Psi.y)

#normalize by max LER for each grouping
proc2$n_LER = proc2$LER / proc2$mLER #normalize gs data over full range

#curve fit by pooling all cultivars within a given drought and condition group

#1 fast and preconditioned
datafp = proc2[proc2$condition =='pre' & proc2$drought == 'fast',]
datafp <- datafp %>%
  select(-variety, -drought, -condition, -mLER, -LER.x)
datafp <- datafp[order(-datafp$Psi.x),]


model = nls(n_LER ~ (1+exp(sf*phyf))/(1+exp(sf*(phyf-Psi.x))), data = datafp, start = list(sf=2,phyf=-0.3))

print(model)

pred <- predict(model, datafp$Psi.x)
plot(datafp$Psi.x, datafp$n_LER, pch = 20, col = 'darkgray', main = 'pre; fast', xlab ="psi-leaf", ylab = "norm-ler")
lines(datafp$Psi.x, pred,lwd = 1, col = 'blue')

#2 fast and control
datafp = proc2[proc2$condition =='control' & proc2$drought == 'fast',]
datafp <- datafp %>%
  select(-variety, -drought, -condition, -mLER, -LER.x)
datafp <- datafp[order(-datafp$Psi.x),]


model = nls(n_LER ~ (1+exp(sf*phyf))/(1+exp(sf*(phyf-Psi.x))), data = datafp, start = list(sf=4,phyf=-0.3))

print(model)

pred <- predict(model, datafp$Psi.x)
plot(datafp$Psi.x, datafp$n_LER, pch = 20, col = 'darkgray', main = 'control; fast', xlab ="psi-leaf", ylab = "norm-ler")
lines(datafp$Psi.x, pred,lwd = 1, col = 'blue')


#3) slow, pre
datafp = proc2[proc2$condition =='pre' & proc2$drought == 'slow',]
datafp <- datafp %>%
  select(-variety, -drought, -condition, -mLER, -LER.x)
datafp <- datafp[order(-datafp$Psi.x),]


model = nls(n_LER ~ (1+exp(sf*phyf))/(1+exp(sf*(phyf-Psi.x))), data = datafp, start = list(sf=10,phyf=-0.4))

print(model)

pred <- predict(model, datafp$Psi.x)
plot(datafp$Psi.x, datafp$n_LER, pch = 20, col = 'darkgray', main = 'pre; slow', xlab ="psi-leaf", ylab = "norm-ler")
lines(datafp$Psi.x, pred,lwd = 1, col = 'blue')

#4) slow, control
datafp = proc2[proc2$condition =='control' & proc2$drought == 'slow',]
datafp <- datafp %>%
  filter(variety == "rikuta") %>%
  select(-variety, -drought, -condition, -mLER, -LER.x)
datafp <- datafp[order(-datafp$Psi.x),]


model = nls(n_LER ~ (1+exp(sf*phyf))/(1+exp(sf*(phyf-Psi.x))), data = datafp, start = list(sf=5,phyf=-0.1))

print(model)

pred <- predict(model, datafp$Psi.x)
plot(datafp$Psi.x, datafp$n_LER, pch = 20, col = 'darkgray', main = 'control; slow', xlab ="psi-leaf", ylab = "norm-ler")
lines(datafp$Psi.x, pred,lwd = 1, col = 'blue')

