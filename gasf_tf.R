# method of creating a transfer function using simulated data

#load data and functions and libraries------------
library(tidyr)
library(magrittr)
library(dplyr)
library(multitaper)
library(MASS) # need for pseudoinverse
library(ggplot2)

source("spec_est.R")
source("get_tf.R")

gas_data <- as.data.frame(read.table("gasf.txt"))
colnames(gas_data) <- c("gas_change", "CO2")

# spectral estimates ---------

block_N <- floor(nrow(gas_data)/10)

spec_gas <- spec_est(gas_data$gas_change, 4, 7, block_N)
spec_co2 <- spec_est(gas_data$CO2, 4, 7, block_N)

# create transfer function ---------

freq <- seq(1, nrow(spec_gas), 2)
tf <- get_tf(spec_gas, spec_co2, freq)

# plot transfer functions --------------

ggplot(data = tf, aes(x = freq, y = val)) +
  facet_grid(type~., scale = "free_y") +
  geom_line() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = "FALSE", colour = "red")



