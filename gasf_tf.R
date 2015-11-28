# method of creating a transfer function using simulated data

#load data and functions and libraries------------
library(tidyr)
library(magrittr)
library(dplyr)
library(multitaper)
library(MASS) # need for pseudoinverse
library(ggplot2)

source("spec_est.R")

gas_data <- as.data.frame(read.table("gasf.txt"))
colnames(gas_data) <- c("gas_change", "CO2")

# spectral estimates ---------

spec_gas <- spec_est(gas_data$gas_change, 4, 7)
spec_co2 <- spec_est(gas_data$CO2, 4, 7)

# create transfer function ---------

freq <- seq(1, nrow(spec_gas), 2)

tf <- vector(mode = 'numeric', length = length(freq))

F_gas <- matrix(ncol = length(freq), nrow = ncol(spec_gas))
F_co2 <- matrix(ncol = length(freq), nrow = ncol(spec_co2))

for(j in 1:length(freq)){
  
  F_gas[,j] <- t(spec_gas[freq[j],]) #take row corresponding to jth chosen frequency
  F_co2[,j] <- t(spec_co2[freq[j],]) #take row corresponding to jth chosen frequency
 
}

for(i in 1:length(freq)){
  tf[i] <- ginv(F_gas[,i]) %*% F_co2[,i]
}


# create data frames of tf magnitude and phase ----------------
p=1/2*length(tf)
mag <- abs(tf)
phase <- atan2(Im(tf), Re(tf))

tf <- data.frame(freq = seq(0,0.5,0.5/p), mag = mag[1:(p+1)], phase = phase[1:(p+1)])
tf <- tf %>% gather(type, val, mag:phase)

# plot transfer functions --------------

ggplot(data = tf, aes(x = freq, y = val)) +
  facet_grid(type~., scale = "free_y") +
  geom_line() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = "FALSE", colour = "red")



