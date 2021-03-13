
options(repr.plot.width = 8, repr.plot.height = 4)

library(dplyr)
library(rgdal)
library(ggplot2)

source('load_data.r')
new_data=read.table('years.vs.regions.smoothing.txt',header=T)
b_pcr=1:57
a_pcr=c_pcr=1:57
temp_glob=global.mean.temp.year
mglob=mean(temp_glob)
for (i in 1:57){
  temp_loc=new_data[,i]
  DD=cbind(temp_loc, temp_glob )
  pcDD=princomp(DD, scores=T)
  a_pcr[i]= -abs(pcDD$loadings[2,1])
  b_pcr[i]= abs(pcDD$loadings[1,1])
  mloc=mean(temp_loc)
  c_pcr[i]=mloc+a_pcr[i]*mglob/b_pcr[i]
}
y=(-(a_pcr[29]*temp_glob)/b_pcr[29])+c_pcr[29]
aa=a_pcr[29]/b_pcr[29]
temp_loc=as.vector(new_data[29])
xnew=1:60
ynew=1:60
for (ii in 1:1){
  cc=temp_glob[ii]-((1/aa)*temp_loc[ii,1])
  xnew[ii]=(cc-c_pcr[29])/(aa-(1/aa))
  ynew[ii]=aa*xnew[ii]+c_pcr[29]
}


reg = "nde"
ggplot(years.vs.regions) +
  labs(title = paste(reg, "- fitted vs real temperature"), x = "Year", y = "Temperature (K)") +
  geom_point(aes(x = years, y = years.vs.regions[, reg]), alpha = 0.2) +
  geom_line(aes(x = years, y = y),col='red')+
  geom_line(aes(x = years, y = new_data[, reg]), alpha = 0.2)
  
ggsave("Plots_stat/plot_dws_lin.png", width = 6, height = 3)

# Plot: linear regression
#xf=0.3476408*(temp_loc+2.472001*temp_glob-c_pcr[29])
#yf=0.4045306*xf+c_pcr[29]
alpha=1/((-(a_pcr[29])/b_pcr[29])-b_pcr[29]/a_pcr[29])
xf=alpha*(temp_loc-(b_pcr[29]/a_pcr[29])*temp_glob-c_pcr[29])
yf=(-(a_pcr[29])/b_pcr[29])*xf+c_pcr[29]
xf=xf$nde
yf=yf$nde
ggplot() +
  labs(title = paste(reg, "- linear regression"), x = "Global temperature (K)", y = "Local temperature (K)") +
  geom_segment(aes(x = global.mean.temp.year, xend = xf,
                   y =  new_data[[reg]], yend = yf), col="grey") +
  geom_point(aes(x = global.mean.temp.year, y = new_data[[reg]])) +
  geom_line(aes(x = c(287.4,global.mean.temp.year,289.1), y = c((-(a_pcr[29])/b_pcr[29])*287.4+c_pcr[29],y,(-(a_pcr[29])/b_pcr[29])*289.1+c_pcr[29])), col="red")

ggsave(paste("Plots_stat/regr_", reg, ".png", sep = ""), width = 6, height = 3)
