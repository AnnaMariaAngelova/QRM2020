#Install packages

#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("VineCopula")
#install.packages("copula")

#Load libraries

library(magrittr) 
library(dplyr)
library(copula)
library(VineCopula)

lha_ <- read.csv("C:/Users/Asus/Desktop/QRM/First Assignment/QRM2020/01_Data/lha_ads.csv", header=T)
lha <- lha_ads %>% select(2)
ads <-lha_ads %>% select(3)

#Check correlation to confirm we get same results as Python and import went correctly
cor(lha,ads,method='spearman')

u <- pobs(as.matrix(cbind(lha,ads)))[,1]
v <- pobs(as.matrix(cbind(lha,ads)))[,2]
selectedCopula <- BiCopSelect(u,v,familyset=NA)
selectedCopula
?BiCopSelect() 

#Let's try to fit the suggested model using the copula package and double check the parameters fitting.

#Fit t-copula to test
t.cop <- tCopula(dim=2)
set.seed(500)
m     <- pobs(as.matrix(cbind(lha,ads)))
fit   <- fitCopula(t.cop,m,method='ml')
coef(fit)

#Let's take a look at the density of the copula we have just estimated

rho <- coef(fit)[1]
df  <- coef(fit)[2]
persp(tCopula(dim=2,rho,df=df),dCopula)

#Now we only need to build the copula and sample from the random samples.

z <- rCopula(20000,tCopula(dim=2,rho,df=df))
plot(z[,1],z[,2],pch='.',col='blue')
cor(z,method='spearman')

#Now we will model the marginals bt assuming normally distributed returns for simplicity.

lha_mu  <- mean(lha$LHA_return, na.rm = TRUE)
lha_sd  <- sd(lha$LHA_return, na.rm = TRUE)
ads_mu  <- mean(ads$ADS_return, na.rm = TRUE)
ads_sd  <- sd(ads$ADS_return, na.rm = TRUE)

copula_dist <- mvdc(copula=tCopula(rho,dim=2,df=df), margins=c("norm","norm"),
                    paramMargins=list(list(mean=lha_mu, sd=lha_sd),
                                      list(mean=ads_mu, sd=ads_sd)))
sim <- rMvdc(5000,copula_dist)

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(1, 1))

plot(lha$LHA_return,ads$ADS_return,main='Returns')
points(sim[,1],sim[,2],col='red')
legend('bottomright',c('Observed','Simulated'),col=c('black','red'),pch=21)

#Testing Alternative Copulas

#Fit Gaussian copula
normal <- normalCopula(dim = 2)
fit_normal   <- fitCopula(normal,m,method='ml')
coef(fit_normal)
rho_fit_normal <- coef(fit_normal)[1]
persp(normalCopula(dim=2,rho),dCopula)

z <- rCopula(20000,normalCopula(dim=2,rho))
plot(z[,1],z[,2],pch='.',col='blue')
cor(z,method='spearman')


#Fit Gumbel copula
gumbel <- gumbelCopula(dim = 2)
fit_gumbel <- fitCopula(gumbel, m, method = 'ml')
coef(fit_gumbel)

#Fit Frank copula
frank <- frankCopula(dim = 2)
fit_frank <- fitCopula(frank, m, method = 'ml')
coef(fit_frank)

#Goodness of fit
BiCopGofTest(u, v, family = 1) #normal
BiCopGofTest(u, v, family = 4) #gumbel
BiCopGofTest(u, v, family = 5) #frank
BiCopGofTest(u, v, family = 2) #student-t (we expect good goodness of fit only here, the rest should be <0.05)




