data=read.table('anova.txt',header=T)
library(mvtnorm)
library(rgl)
library(car)
library(MASS)


##########STEP 1: A PARTIRE DALLA TEMPERATURA MEDIA (ANNI 1981-2010)
##########CREO 4 ZONE CLIMATICHE
#UTILIZZO UN HIERARCHICAL CLUSTERING

iris4=data$mean_new #faccio il clustering sulla temperatura 1981-2010
iris.e <- dist(iris4, method='euclidean')
iris.es <- hclust(iris.e, method='average') #average linkage



#creo 4 gruppi
cluster.es <- cutree(iris.es, k=4)
i1 <- as.vector(which(cluster.es==1))
i2 <- as.vector(which(cluster.es==2))
i3 <- as.vector(which(cluster.es==3))
i4 <- as.vector(which(cluster.es==4))
uno=iris4[i1]
due=iris4[i2]
tre=iris4[i3]
quattro=iris4[i4]

plot(iris4)
points(iris4[i1], col='red', pch=19)
points(iris4[i2], col='green', pch=19)
points(iris4[i3], col='blue', pch=19)
points(iris4[i4], col='orange', pch=19)

latitudine=data$y
latitudini1=data$y[i1] #salvo le coordinate dei 4 gruppi trovati
latitudini2=data$y[i2]
latitudini3=data$y[i3]
latitudini4=data$y[i4]
longitudine=data$x
longitudine1=data$x[i1]
longitudine2=data$x[i2]
longitudine3=data$x[i3]
longitudine4=data$x[i4]

#par(mfrow=c(1,4))
#plot(longitudine1,latitudini1)
#plot(longitudine2,latitudini2)
#plot(longitudine3,latitudini3)
#plot(longitudine4,latitudini4)

x11()
plot(longitudine,latitudine)
points(longitudine1,latitudini1, col='red', pch=19) #artico, "mare" intorno al polo sud, himalaya
points(longitudine2,latitudini2, col='green', pch=19)#antardide e groenlandia
points(longitudine3,latitudini3, col='blue', pch=19)#zona temperata nord+sud) e ande
points(longitudine4,latitudini4, col='orange', pch=19)# fascia equatoriale



############STEP 2. Ora ho 4 zone climatiche. Cerco di capire se il riscaldamento in queste zone
###è costante o se alcune zone si scaldano di più. Utilizzo un permutation test


#ANOVA (guardo le differenze di temperatura tra 1951-1980 e 1981-2010. Cambia qualcosa tra i gruppi trovati?)
diff1=data$diff[i1]
diff2=data$diff[i2]
diff3=data$diff[i3]
diff4=data$diff[i4]


diff=c(diff1,diff2,diff3,diff4)
index_inizio=c(1,(1+length(i1)),(1+length(i1)+length(i2)),(1+length(i1)+length(i2)+length(i3)))
index_fine=c(length(i1),(length(i1)+length(i2)),(length(i1)+length(i2)+length(i3)),(length(i1)+length(i2)+length(i3)+length(i4)))
nome_1=rep('uno',length(i1))
nome_2=rep('due',length(i2))
nome_3=rep('tre',length(i3))
nome_4=rep('quattro',length(i4))
fascia=c(nome_1,nome_2,nome_3,nome_4)
fit <- aov(diff ~ fascia)
T0 <- summary(fit)[[1]][1,4]

B <- 1000 # Number of permutations
T_stat <- numeric(B) 
n <- dim(data)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  diff_perm <- diff[permutation]
  fit_perm <- aov(diff_perm ~ fascia)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}
p_val <- sum(T_stat>=T0)/B
p_val #p-val 0, c'è differenza

#CERCHIAMO DI CAPIRE IN QUALI FASCE C'è DIFFERENZA, per ogni coppia possibile faccio il test mu_i=mu_j
pval=matrix(-1,4,4)
nn=c(length(i1),length(i2),length(i3),length(i4))

for (i in 1:3){
  for (j in (i+1):4){
    print(paste("fascia",i,"-","fascia",j))
    n1=nn[i]
    n2=nn[j]
    n=n1+n2
    x1=diff[index_inizio[i]:index_fine[i]]
    x2=diff[index_inizio[j]:index_fine[j]]
    x_pooled <- c(x1,x2)
    T0 <- abs(mean(x1) - mean(x2))
    B <- 10000 # Number of permutations
    T_stat <- numeric(B) # Vector where we will store the values of T*
    
    # To estimate the p-value we use a loop
    # Inside the loop, we do the following:
    # 1. choose a random permutation of data (with the command sample)
    # 2. calculate and save the test statistic obtained with the permuted data
    for(perm in 1:B){
      # permutation:
      permutation <- sample(1:n)
      x_perm <- x_pooled[permutation]
      x1_perm <- x_perm[1:n1]
      x2_perm <- x_perm[(n1+1):n]
      # test statistic:
      T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
    }
    
    pval[i,j]=sum(T_stat>=T0)/B
    pval[i,j]
  }
}
pval# guardare solo la parte superiore della matrice. Sono tutti zeri-->c'è differenza in ogni gruppo