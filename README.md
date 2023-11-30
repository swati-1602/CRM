library(ExtDist)## for laplace
library(ggplot2)
library(cowplot)
library(MASS)



#sample_size<-seq(10,50,10)
sample_size<-seq(50,1000,50)



## define loss function
L<-function(x){
  L1<- sum(abs(x))
  return(L1)
}


##parameter estimation using  control function approach

  f_1<-function(beta,rho,n){
     
    
  #instrumental variable 
    z<-runif(n) 
   
   ##Endogenous regressor
   x_2<-1*z+e[2,] 
   
   #define reduce form error term
   e_1<-rnorm(n)
   
   
   ## error term
   #error<-abs(beta[1]+x_1*beta[2]+x_2*beta[3]+rho*e_1)*rnorm(n,0,1)
   error<-rnorm(n)
   #error<-rLaplace(n)
   #error<-rt(n,n)
   
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





###combine plots for mse of beta and rho
plot(sample_size,MSE_c1,col=c('red','blue'),type='o',pch=19,lwd=2,cex=1.5,xlab="Sample size",ylab="MSE")
abline(h=0,lty=2)

plot(sample_size,MSE_c2,col=c('green','blue'),type='o',pch=19,lwd=2,cex=1.5,xlab="Sample size",ylab="MSE")
abline(h=0,lty=2)


plot(sample_size,MSE_c3,col=c('orange','blue'),type='o',pch=19,lwd=2,cex=1.5,xlab="Sample size",ylab="MSE")
abline(h=0,lty=2)

plot(sample_size,MSE_rho1,col=c('violet','blue'),type='o',pch=19,lwd=2,cex=1.5,xlab="Sample size",ylab="MSE")
abline(h=0,lty=2)



