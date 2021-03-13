library('MASS')
library('caTools')
library('raster')

library('raster')
library('reshape')
library('ggplot2')
library('ggmap')
library('rgdal')
library('maptools')
library('dplyr')


library('edrGraphicalTools')
library('fda')
library('fdakma')
library('tidyverse')
library('rgdal')

data = stack('Dataset/tas_Amon_GISS-E2-H-CC_historical_r1i1p1_195101-201012.nc')
data = rotate(data)

df = as.data.frame(data)
areas = as.vector(raster::area(data))

global.mean.temp = as.vector(as.vector(areas) %*% as.matrix(df) / sum(areas))

coverages = read.csv("Outputs/matrice_copertura_regioni.csv")
coverages$X = NULL
# Rimuovi colonne tutte NA
coverages = coverages[, colSums(!is.na(coverages)) == nrow(coverages)]

months.vs.countries = as.data.frame(t(data.matrix(df)) %*% data.matrix(coverages))
months.vs.countries$year = as.numeric(substring(dimnames(months.vs.countries)[[1]], 2, 5))

years.vs.countries = aggregate(months.vs.countries, by=list(year=months.vs.countries$year), FUN=mean)
glob_temp_year= aggregate(global.mean.temp, by=list(year=months.vs.countries$year), FUN=mean)
set.seed(150550)
ni=4
nf=15
err_tot=matrix(0,(nf-ni+1),3)
err_tot_mean=1:(nf-ni+1)

#ii=7
for (ii in ni:nf){
years.vs.regions=read.table('years.vs.regions.txt')
m=5
t = 1:60
basis = create.bspline.basis(c(0, 60), nbasis = ii)
f.data = smooth.basis(y = as.matrix(years.vs.regions), fdParobj = basis) 
new_data= eval.fd(t, f.data$fd, Lfd = 0)





dati=years.vs.countries
head(dati)
#dati1=dati[,2:58]
dati1=as.data.frame(new_data)
temp_glob=glob_temp_year[,2]
anni=dati[,1]


funlin_xy=function(){
  i_train=1:57
  a_train=1:57
  for (i in 1:57){ #for every region
    temp_loc_train=training[,i]
    mod1=lm(temp_loc_train ~ temp_train)
    i_train[i]=mod1$coefficients[1] #select intercept
    a_train[i]=mod1$coefficients[2]#and slope
  }
  temp_anno1=temp_glob[j]  #select a global temperature referred to a random year
  xlin=(a_train*temp_anno1+(i_train)) 
  xlin
}

funlin_yx=function(){
  i_train=1:57
  a_train=1:57
  temp_anno1=temp_glob[j]
  for (i in 1:57){ #for every region
    temp_loc_train=training[,i]
    mod1=lm(temp_train ~ temp_loc_train)
    i_train[i]=mod1$coefficients[1] #select intercept
    a_train[i]=mod1$coefficients[2]#and slope
  }
  
  xlin=(temp_anno1-i_train)/(a_train)
  xlin
}

funpca=function(){
  b_pcr=1:57
  a_pcr=c_pcr=1:57
  mglob=mean(temp_train)
  for (i in 1:57){
    temp_loc_train=training[,i]
    DD=cbind(temp_loc_train, temp_train )
    pcDD=princomp(DD, scores=T)
    a_pcr[i]= -abs(pcDD$loadings[2,1])
    b_pcr[i]= abs(pcDD$loadings[1,1])
    mloc=mean(temp_loc_train)
    c_pcr[i]=mloc+a_pcr[i]*mglob/b_pcr[i]
  }
  temp_anno1=temp_glob[j] 
  y=(-(a_pcr*temp_anno1)/b_pcr)+c_pcr
  y
  
}

#prepare vectors of error of cross validation
errxy=1:50
erryx=1:50
errpca=1:50

for(i in 1:50){ #repeat the cross validation 50 times
  num=sample(1:60,1)#select a datum (leave one out)
  training=dati1[-num,] #new training set
  #test=dati1[num,]#test set
  test=dati[num,2:58]
  j=num
  
  temp_train=temp_glob[-num] 
  xxy=funlin_xy() #compute the estimate of the datum left out using all dawnscaler we have
  xyx=funlin_yx()
  xpcr=funpca()
  
  
  errxy[i]=rowMeans((xxy-test)) #compute the error for every downscaler
  erryx[i]=rowMeans((xyx-test))
  errpca[i]=rowMeans((xpcr-test))
  
}

err=cbind(errxy, erryx, errpca)
err

#for (dws in c("errxy", "erryx", "errpca")) {
#  ggplot() +
#    geom_point(aes(x = 1:50, y = err[, dws])) +
#    geom_line(aes(x = -1:52, y = 0), color = 'red') +
#    labs(x = "Index", y = "Error")
#  xlim(0, 51)
#  ggsave(paste("Plots_stat/dws_", dws, ".png", sep = ""), width = 6, height = 3)
#}



errmedio=colMeans(abs(err)) #compute, for every downscaler, the mean of the errors.
errmedio
err_tot[ii-ni+1,]=errmedio
err_tot_mean[ii-ni+1]=mean(errmedio)
}