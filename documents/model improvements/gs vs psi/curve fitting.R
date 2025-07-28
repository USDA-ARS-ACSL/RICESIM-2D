# simple curve fitting for tauzet relationship between normalized gs and leaf water potential
# units are mmol m-2 s-1 versus MPa respectively
# data from Zhang, Q., W. Tang, Z. Xiong, S. Peng and Y. Li (2023). "Stomatal conductance in rice leaves and panicles responds differently to abscisic acid and soil drought." J Exp Bot 74(5): 1551-1563.

#Based on results, found sf = 2.87 and phyf = -0.499 gave best fit


#read in csv file ontained from above and digitized using www.graphreader.com
read_data <- read.csv("E://RICESIM-2D//RICESIM-2D//improvements//gs vs psi//gs_psi.csv")

#processing
proc <- read_data
proc$PSI = proc$PSI * -1 #MPA was originally absolute value
proc$n_gs = proc$gs / max(proc$gs) #normalize gs data over full range

#curve fit
model = nls(n_gs ~ (1+exp(sf*phyf))/(1+exp(sf*(phyf-PSI))), data = proc, start = list(sf=2,phyf = -0.3))
print(model)
pred <- predict(model, proc$PSI)

plot(proc$PSI, proc$n_gs, pch = 20, col = 'darkgray', main = 'fit results', xlab ="psi-leaf", ylab = "norm-g_s")
lines(proc$PSI, pred,lwd = 3, col = 'blue')
