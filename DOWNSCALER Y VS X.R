
options(repr.plot.width = 8, repr.plot.height = 4)

library(dplyr)
library(rgdal)
library(ggplot2)

source('load_data.r')

inter=1:57
alpha=1:57
new_data=read.table('years.vs.regions.smoothing.txt',header=T)
temp_glob=global.mean.temp.year
for (i in 1:57){ #for every region
  temp_loc=new_data[,i]
  mod1=lm(temp_glob ~ temp_loc)
  inter[i]=mod1$coefficients[1] #select intercept
  alpha[i]=mod1$coefficients[2]#and slope
}
xfin=new_data[,29]*alpha[29]+inter[29]
y=(temp_glob-inter[29])/(alpha[29])

reg = "nde"
ggplot(years.vs.regions) +
  labs(title = paste(reg, "- fitted vs real temperature"), x = "Year", y = "Temperature (K)") +
  geom_point(aes(x = years, y = years.vs.regions[, reg]), alpha = 0.2,col='black') +
  geom_line(aes(x = years, y = new_data[, reg]), alpha = 0.2) +
  geom_line(aes(x = years, y = y),col='red')
ggsave("Plots_stat/plot_dws_lin_inv.png", width = 6, height = 3)

# Plot: linear regression

ggplot() +
  labs(title = paste(reg, "- linear regression"), x = "Global temperature (K)", y = "Local temperature (K)") +
  geom_segment(aes(x= global.mean.temp.year , xend = xfin  ,
                   y =  new_data[[reg]], yend =new_data[[reg]]), col="grey")+ 
  geom_point(aes(x = global.mean.temp.year, y = new_data[[reg]])) +
  geom_line(aes(x = global.mean.temp.year, y = y), col="red")
ggsave(paste("Plots_stat/regr_inv_", reg, ".png", sep = ""), width = 6, height = 3)
