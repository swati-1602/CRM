library(ExtDist)## for laplace
library(ggplot2)
library(cowplot)
library(MASS)




#sample_size<-seq(10,50,10)
sample_size<-seq(50,1000,50)



## define absolute loss function
L<-function(x){
  L1<-abs(x)
  return(mean(L1))
}

## define huber loss function
huber_loss <- function(x,w=1.35) {
  loss <- ifelse(abs(x) <= w, 0.5 * x^2, w* (abs(x) - 0.5 * w))
  return(mean(loss))
}

## Define the log cosh loss function
Logc_loss <- function( x) {
  loss <-log(cosh(x))
  return(mean(loss))
}


## Clad parameter estimation using  control function approach

f_1<-function(beta,rho,n){
  
  
  #instrumental variable 
  z<-runif(n) 
  
  #define reduce form error term
  e_1<-rnorm(n)
  
  ##Endogenous regressor
  x_2<-1*z+e_1
  
  
  ## error term
  #error<-abs(beta[1]+x_1*beta[2]+x_2*beta[3]+rho*e_1)*rnorm(n,0,1)
  #error<-rnorm(n)
  #error<-rLaplace(n)
  error<-rt(n,3)
  
  ### define correaltaion
  e<-rho*e_1+error
  
  
  ### define exogenous variable
  x_1<-rnorm(n) 
  
 
  
  ## define latent variable
  y_star<-beta[1]+x_1*beta[2]+x_2*beta[3]+e
  
  ## define latent variable
  y<-pmax(0,y_star)
  
  ### first stage regression
  res<-residuals(lm(x_2~z))
  
  
  # Count left-censored observations
  left_censored_count<-sum(y_star < 0)
  censoring<-(left_censored_count/n)*100
  
  
  ### Optimize function
  f1<-function(theta){
    Loss<-L(y-pmax(0,theta[1]+theta[2]*x_1+theta[3]*x_2+theta[4]*res))
  }
  v<-c(optim(c(1,1,1,1),f1)$par,censoring)
  return(v)
  
}
f_1(c(1,2,3),0.5,50) 

## WME parameter estimation using  control function approach

f_2<-function(beta,rho,n){
  #w<- 0.01, 0.05, 0.1, 0.5,1
  
  #instrumental variable
  z<-runif(n)
  
  #define reduce form error term
  e_1<-rnorm(n)
  
  ##Endogenous regressor
  x_2<-1*z+e_1 
  
  ## error term
  #error<-abs(beta[1]+x_1*beta[2]+x_2*beta[3]+rho*e_1)*rnorm(n)
  #error<-rnorm(n)
  #error<-rLaplace(n)
  error<-rt(n,3)
  
  ### define correaltaion
  e<-rho*e_1+error
  
  ### define exogenous variable
  x_1<-rnorm(n) 
  
  
  
  ### first stage regression
  res<-residuals(lm(x_2~z))
  
  ## define latent variable
  y_star<-beta[1]+x_1*beta[2]+x_2*beta[3]+e 
  
  
  ## define  censored response variable
  y<-pmax(0,y_star)
  
  
  # Count left-censored observations
  left_censored_count <- sum(y_star < 0)
  censoring<-(left_censored_count/n)*100
  
  
  ### Optimize function
  f2<-function(theta){
    Loss<-huber_loss(y-pmax(1.35,theta[1]+theta[2]*x_1+theta[3]*x_2+theta[4]*res))
  }
  
  v<-c(optim(c(1,1,1,1),f2)$par,censoring)
  return(v)
  
}
f_2(c(1,2,3),0.5,50)

##Log cosh  Parameter estimation using the control function approach
L_4<- function(beta, rho, n) {
  
  # Instrumental variable
  
  z <- runif(n) 
  
  #reduce form  Error term
  e_1 <- rnorm(n) 
  
  ## Endogenous regressor
  x_2 <- 1 * z + e_1 
  
  ## Error term
  #error<-abs(beta[1] + x_1*beta[2] + x_2*beta[3]+rho*e_1)*rnorm(n)
  #error <- rnorm(n) 
  #error<-rLaplace(n)
  error<-rt(n,3)
  
  ### Define correlation
  e <-rho * e_1 + error 
  
  ### Define exogenous variable
  x_1<-rnorm(n)

  
  ## Define latent variable
  y_star <- beta[1] + x_1 * beta[2] + x_2 * beta[3] + e 
  
  ## Define  censored response variable
  y_i<- pmax(0, y_star) 
  
  ### First stage regression
  res <- residuals(lm(x_2 ~ z)) 
  
  
  # Count left-censored observations
  left_censored_count <- sum(y_star < 0)
  censoring <- (left_censored_count / n) * 100
  
  ### Optimize function
  L4 <- function(theta) {
    Logc_loss(y_i-pmax(0,theta[1] + theta[2] * x_1 + theta[3] * x_2 + theta[4] * res))
  }
  
  v<- c(optim(c(1, 1, 1, 1), L4)$par, censoring)
  return(v)
}

L_4(c(1, 2, 3), 0.5, 1000)



## Monte Carlo study
## number of replications
r<-2000

## MSE for parameters
beta_hat1<-c()
Bias_c1<-c()
Bias_c2<-c()
Bias_c3<-c()
Bias_rho1<-c()
MSE_c1<-c()
MSE_c2<-c()
MSE_c3<-c()
MSE_rho1<-c()
cens_count<-c()



for (N in sample_size)
{
  beta<-c(1,2,3)
  rho<-0.5
  beta_hat1<-sapply(1:r, function(l)f_1(beta,rho,N)[-c(5)])
  MSE_c1<-append(MSE_c1, mean(( beta_hat1[1,]- beta[1])^2), after = length(MSE_c1))
  MSE_c2<-append(MSE_c2, mean(( beta_hat1[2,]- beta[2])^2), after = length(MSE_c2))
  MSE_c3<-append(MSE_c3, mean(( beta_hat1[3,]- beta[3])^2), after = length(MSE_c3))
  MSE_rho1<-append(MSE_rho1, mean(( beta_hat1[4,]- rho)^2), after = length(MSE_rho1))
  Bias_c1<-append(Bias_c1, mean( beta_hat1[1,]- beta[1]), after = length(Bias_c1))
  Bias_c2<-append(Bias_c2, mean( beta_hat1[2,]- beta[2]), after = length(Bias_c2))
  Bias_c3<-append(Bias_c3, mean( beta_hat1[3,]- beta[3]), after = length(Bias_c3))
  Bias_rho1<-append(Bias_rho1, mean( beta_hat1[4,]- rho), after = length(Bias_rho1))
  cens_count<-append(cens_count, f_1(beta,rho,N)[-c(1,2,3,4)], after = length(cens_count))
  
}


round(MSE_c1[c(1,2,10,20)],digits=4)
round(MSE_c2[c(1,2,10,20)],digits=4)
round(MSE_c3[c(1,2,10,20)],digits=4)
round(MSE_rho1[c(1,2,10,20)],digits=4)
round(Bias_c1[c(1,2,10,20)],digits=4)
round(Bias_c2[c(1,2,10,20)],digits=4)
round(Bias_c3[c(1,2,10,20)],digits=4)
round(Bias_rho1[c(1,2,10,20)],digits=4)
cens_count



## MSE for parameters of huber loss
beta_hat11<-c()
Bias_h1<-c()
Bias_h2<-c()
Bias_h3<-c()
Bias_rho2<-c()
MSE_h1<-c()
MSE_h2<-c()
MSE_h3<-c()
MSE_rho2<-c()
cens_count1<-c()


for (N in sample_size)
{
  beta<-c(1,2,3)
  rho<-0.5
  beta_hat11<-sapply(1:r, function(l)f_2(beta,rho,N)[-c(5)])
  MSE_h1<-append(MSE_h1, mean(( beta_hat11[1,]- beta[1])^2), after = length(MSE_h1))
  MSE_h2<-append(MSE_h2, mean(( beta_hat11[2,]- beta[2])^2), after = length(MSE_h2))
  MSE_h3<-append(MSE_h3, mean(( beta_hat11[3,]- beta[3])^2), after = length(MSE_h3))
  MSE_rho2<-append(MSE_rho2, mean(( beta_hat11[4,]- rho)^2), after = length(MSE_rho2))
  Bias_h1<-append(Bias_h1, mean( beta_hat11[1,]- beta[1]), after = length(Bias_h1))
  Bias_h2<-append(Bias_h2, mean( beta_hat11[2,]- beta[2]), after = length(Bias_h2))
  Bias_h3<-append(Bias_h3, mean( beta_hat11[3,]- beta[3]), after = length(Bias_h3))
  Bias_rho2<-append(Bias_rho2, mean( beta_hat11[4,]- rho), after = length(Bias_rho2))
  cens_count1<-append(cens_count1,f_2(beta,rho,N)[-c(1,2,3,4)] , after = length(cens_count1))
}


round(MSE_h1[c(1,2,10,20)],digits=4)
round(MSE_h2[c(1,2,10,20)],digits=4)
round(MSE_h3[c(1,2,10,20)],digits=4)
round(MSE_rho2[c(1,2,10,20)],digits=4)
round(Bias_h1[c(1,2,10,20)],digits=4)
round(Bias_h2[c(1,2,10,20)],digits=4)
round(Bias_h3[c(1,2,10,20)],digits=4)
round(Bias_rho2[c(1,2,10,20)],digits=4)
cens_count1



## MSE for parameters of Log-cosh loss
beta_hatl1<-c()
MSE1<-c()
MSE2<-c()
MSE3<-c()
MSErho<-c()
Bias1<-c()
Bias2<-c()
Bias3<-c()
Biasrho<-c()
cens2<-c()



for (N in sample_size)
{
  beta<-c(1,2,3)
  rho<-0.5
  beta_hatl1<-sapply(1:r, function(l)L_4(beta,rho,N)[-c(5)])
  MSE1<-append(MSE1, mean(( beta_hatl1[1,]- beta[1])^2), after = length(MSE1))
  MSE2<-append(MSE2, mean(( beta_hatl1[2,]- beta[2])^2), after = length(MSE2))
  MSE3<-append(MSE3, mean(( beta_hatl1[3,]- beta[3])^2), after = length(MSE3))
  MSErho<-append(MSErho, mean(( beta_hatl1[4,]- rho)^2), after = length(MSErho))
  Bias1<-append(Bias1, mean( beta_hatl1[1,]- beta[1]), after = length(Bias1))
  Bias2<-append(Bias2, mean( beta_hatl1[2,]- beta[2]), after = length(Bias2))
  Bias3<-append(Bias3, mean( beta_hatl1[3,]- beta[3]), after = length(Bias3))
  Biasrho<-append(Biasrho, mean( beta_hatl1[4,]- rho), after = length(Biasrho))
  cens2<-append(cens2, L_4(beta,rho,N)[-c(1,2,3,4)], after = length(cens2))
  
}


round(MSE1[c(1,2,10,20)],digits=4)
round(MSE2[c(1,2,10,20)],digits=4)
round(MSE3[c(1,2,10,20)],digits=4)
round(MSErho[c(1,2,10,20)],digits=4)
round(Bias1[c(1,2,10,20)],digits=4)
round(Bias2[c(1,2,10,20)],digits=4)
round(Bias3[c(1,2,10,20)],digits=4)
round(Biasrho[c(1,2,10,20)],digits=4)
cens2



###combine plots for mse of beta and rho
#df<-data.frame(sample_size,MSE_c1)
#M1<-ggplot(df, aes(x=sample_size,y=MSE_c1))+ geom_line(color = "blue")+ geom_point(color = "red") +labs(title = "Beta_0")  + xlab("Sample Size") + 
# ylab("MSE") +geom_hline(yintercept =0,linetype="dashed") 


#df<-data.frame(sample_size,MSE_c2)
#M2<-ggplot(df, aes(x=sample_size,y=MSE_c2))+ geom_line(color = "red")+ geom_point(color = "blue") +labs(title = "Beta_1")  + xlab("Sample Size") + 
#ylab("MSE")+geom_hline(yintercept =0,linetype="dashed") 

#df<-data.frame(sample_size,MSE_c3)
#M3<-ggplot(df, aes(x=sample_size,y=MSE_c3))+ geom_line(color = "orange")+ geom_point(color = "blue") +labs(title = "Beta_2")  + xlab("Sample Size") + 
#ylab("MSE") +geom_hline(yintercept =0,linetype="dashed")                

##df<-data.frame(sample_size,MSE_rho1)
#M4<-ggplot(df, aes(x=sample_size,y=MSE_rho1))+ geom_line(color = "violet")+ geom_point(color = "blue") +labs(title = "Rho") + xlab("Sample Size") + 
#ylab("MSE")+geom_hline(yintercept =0,linetype="dashed") 

#plot_grid(M1, M2,M3, M4,nrow=2, ncol = 2)




### plot for mse of estimate beta_0  for different estimator
plot(sample_size,MSE_c1,type = "o",col="red",lty=1,xlab = "Sample Size",ylab = "MSE")
lines(sample_size,MSE_h1, col="green4",type="o",pch=22,)
lines(sample_size,MSE1, col="skyblue4",type="o")
abline(h=0,col="black",type = "o")
legend("topright",c("CLAD","WME","CLCE"), col=c("red","green4","skyblue4"),
       lty=1)

### plot for mse of estimate beta_1  for different estimator
plot(sample_size,MSE_c2,type = "o",col="red",lty=1,xlab = "Sample Size",ylab = "MSE")
lines(sample_size,MSE_h2, col="green4",type="o", pch=22)
lines(sample_size,MSE2, col="skyblue4",type="o")
abline(h=0,col="black",type = "o")
legend("topright",c("CLAD","WME","CLCE"),col=c("red", "green4", "skyblue4"),
       lty=1)

### plot for mse of estimate beta_2  for different estimator
plot(sample_size,MSE_c3,type = "o",col="red",lty=1,xlab = "Sample Size",ylab = "MSE")
lines(sample_size,MSE_h3, col="green4",type="o", pch=22)
lines(sample_size,MSE3, col="skyblue4",type="o")
abline(h=0,col="black",type = "o")
legend("topright",c("CLAD","WME","CLCE"), col=c("red", "green4", "skyblue4"),
       lty=1)
plot(sample_size,MSE_rho1,type = "o",col="red",lty=1,xlab = "Sample Size",ylab = "MSE")
lines(sample_size,MSE_rho2, col="green4",type="o", pch=22)
lines(sample_size,MSErho, col="skyblue4",type="o")
abline(h=0,col="black",type = "o")
legend("topright",c("CLAD","WME","CLCE"), col=c("red", "green4", "skyblue4"),
       lty=1)


