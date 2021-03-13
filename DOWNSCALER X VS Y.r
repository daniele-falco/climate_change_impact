
options(repr.plot.width = 8, repr.plot.height = 4)

library(dplyr)
library(rgdal)
library(ggplot2)
library(gdxtools)

source('load_data.r')

## Downscaler creation

models = list()
new_data=read.table('years.vs.regions.smoothing.txt',header=T)
for (region in colnames(coverages)) {
    local.temp = new_data[, region]
    models[[region]] = lm(local.temp ~ global.mean.temp.year)
}

## Plot: fitted vs real temperature

reg = "nde"
ggplot(years.vs.regions) +
    labs(title = paste(reg, "- fitted vs real temperature"), x = "Year", y = "Temperature (K)") +
    geom_point(aes(x = years, y = years.vs.regions[, reg]), alpha = 0.2,col='black') +
  geom_line(aes(x = years, y = new_data[, reg]), alpha = 0.2,col='blue')+
    geom_line(aes(x = years, y = models[[reg]]$fitted.values),col='red')
ggsave("Plots_stat/plot_dws_lin.png", width = 6, height = 3)

# Plot: linear regression
ggplot() +
    labs(title = paste(reg, "- linear regression"), x = "Global temperature (K)", y = "Local temperature (K)") +
    geom_segment(aes(x = global.mean.temp.year, xend = global.mean.temp.year,
                     y =  new_data[[reg]], yend = models[[reg]]$fitted.values), col="grey") +
    geom_point(aes(x = global.mean.temp.year, y = new_data[[reg]])) +
    geom_line(aes(x = global.mean.temp.year, y = models[[reg]]$fitted.values), col="red")
ggsave(paste("Plots_stat/regr_", reg, ".png", sep = ""), width = 6, height = 3)

