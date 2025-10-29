library(wooldridge)
data<-mroz
dat<-data[,-c(1,seq(7,10,1),seq(12,18,1),21)]
index<-which(mroz$hours == 0)
dat$hours[index] = 0

### censored proportion count
censored_count<-length(index)
censored_p<-(censored_count/753)*100


### first stage regression
dat$res<-residuals(lm(nwifeinc~age+educ+exper+expersq+kidslt6+kidsge6+huseduc,data=dat))


###define absolute loss function 
AL<-function(beta,data){
  
  L<-sum(abs(dat$hours-pmax(0,beta[1]+beta[2]*dat$age+beta[3]*dat$educ+beta[4]*dat$exper+beta[5]*dat$expersq+beta[6]*dat$kidslt6+beta[7]*dat$kidsge6+beta[8]*dat$nwifeinc+beta[9]*dat$res)))
}

est<-optim(c(rep(0.001,9)),AL,data=dat)$par


## Bootstrap samples

B<-seq(100,500,100)

mse_1<-c()
mse_2<-c()
mse_3<-c()
mse_4<-c()
mse_5<-c()
mse_6<-c()
mse_7<-c()
mse_8<-c()
mse_9<-c()

for (b in B) 
{
  bootstrap_sample <- function(data) {
    n <- nrow(data)
    indices <- sample(1:n, replace = TRUE)
    return(data[indices, ])
  }
  
  ## Generate B = 1000 bootstrap samples and store in a list.
  
  Boot_samples<- lapply(1:b, function(ii)bootstrap_sample(dat))
  
  
  es_t<-c()
  ## Bootstrap MSE of tobit parameters 
  for (i in 1:b) 
  {
    bs_i<-Boot_samples[[i]]
    
    ###first stage regression
    bs_i$res<-residuals(lm(nwifeinc~age+educ+exper+expersq+kidslt6+kidsge6+huseduc,,data=bs_i))
    
    ###define absolute loss function 
    AL<-function(beta){
      
      L<-sum(abs(bs_i$hours-pmax(0,beta[1]+beta[2]*bs_i$age+beta[3]*bs_i$educ+beta[4]*bs_i$exper+beta[5]*bs_i$expersq++beta[6]*bs_i$kidslt6+beta[7]*bs_i$kidsge6+beta[8]*bs_i$nwifeinc+beta[9]*bs_i$res)))
      return(L)
    }
    es_t[[i]]<-optim(c(rep(0.001,9)),AL)$par
    
  }
  
  ## Bootstrap MSE
  v1<-sapply(1:b, function(ii)es_t[[ii]][1])
  v2<-sapply(1:b, function(ii)es_t[[ii]][2])
  v3<-sapply(1:b, function(ii)es_t[[ii]][3])
  v4<-sapply(1:b, function(ii)es_t[[ii]][4])
  v5<-sapply(1:b, function(ii)es_t[[ii]][5])
  v6<-sapply(1:b, function(ii)es_t[[ii]][6])
  v7<-sapply(1:b, function(ii)es_t[[ii]][7])
  v8<-sapply(1:b, function(ii)es_t[[ii]][8])
  v9<-sapply(1:b, function(ii)es_t[[ii]][9])
  
  
  ## MSE 
  mse_1<-append(mse_1,mean((v1 - est[1])^2),after = length(mse_1))
  mse_2<-append(mse_2,mean((v2 - est[2])^2),after = length(mse_2))
  mse_3<-append(mse_3,mean((v3 - est[3])^2),after = length(mse_3))
  mse_4<-append(mse_4,mean((v4 - est[4])^2),after = length(mse_4))
  mse_5<-append(mse_5,mean((v5 - est[5])^2),after = length(mse_5))
  mse_6<-append(mse_6,mean((v6 - est[6])^2),after = length(mse_6))
  mse_7<-append(mse_7,mean((v7 - est[7])^2),after = length(mse_7))
  mse_8<-append(mse_8,mean((v8 - est[8])^2),after = length(mse_8))
  mse_9<-append(mse_9,mean((v9 - est[9])^2),after = length(mse_9))
  
}
mse_1
mse_2
mse_3
mse_4
mse_5
mse_6
mse_7
mse_8
mse_9

plot(B,mse_1,type = "l")
plot(B,mse_2,type = "l")
plot(B,mse_3,type = "l")
plot(B,mse_4,type = "l")
plot(B,mse_5,type = "l")
plot(B,mse_6,type = "l")
plot(B,mse_7,type = "l")
plot(B,mse_8,type = "l")
plot(B,mse_9,type = "l")


