#Libraries
library(tidyr)
library(dplyr)
library(np)
library(class)
library(caret)
library(splines)
library(mgcv)
library(sandwich)
library(lmtest)
library(xtable)

#Load Data
TFAdata <- read.delim("~/Documents/Booth/Year 1/Spring/Econometrics III/Problem Sets/Problem Set 4/TFAdata.txt")

#Set data to factors
TFAdata$marr <- factor(TFAdata$marr)
TFAdata$twoearn <- factor(TFAdata$twoearn)
TFAdata$e401 <- factor(TFAdata$e401)
TFAdata <- TFAdata[,1:10]
TFAdata <- TFAdata[,-8]
TFAdata <- TFAdata[,-6]

#Subset data
os <- sample_n(TFAdata, 1915) #out of sample data
is <- anti_join(TFAdata, os) #in sample data

#Subset is data
is_marr_two <- subset(is, marr == 1 & twoearn == 1)
is_marr_one <- subset(is, marr == 1 & twoearn == 0)
is_notmarr <- subset(is, marr == 0 & twoearn == 1) #likely data error
is_notmarr_one <- subset(is, marr == 0 & twoearn == 0)

#Subset os data
os_marr_two <- subset(os, marr == 1 & twoearn == 1)
os_marr_one <- subset(os, marr == 1 & twoearn == 0)
os_notmarr <- subset(os, marr == 0 & twoearn == 1) #likely data error
os_notmarr_one <- subset(os, marr == 0 & twoearn == 0)

#In sample objects
Y <- as.vector(is[,1])
X <- as.matrix(cbind(is$age,is$inc,is$fsize,
                         is$educ,is$marr,is$twoearn))

#Out of sample objects
Yos <- as.vector(os[,1])
Xos <- as.matrix(cbind(os$age,os$inc,os$fsize,os$educ,os$marr,os$twoearn))

#Part (a)

#Univariate LOOVC
Y <- as.matrix(is_marr_one[,1])
X <- as.matrix(is_marr_one[,2:5])
n = nrow(is_marr_one)

# n: sample size
h_seq = seq(from=0.1,to=10, by=0.01)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = ksmooth(x=X_tr,y=Y_tr,kernel = "normal",bandwidth=h_using,x.points = X_val)
    #Y_val_predict = npreg(X_tr, Y_tr, bws = h_using,bandwidth.comput = FALSE)
    print(Y_val_predict$y)
    CV_err[i] = (Y_val - Y_val_predict$y[1:4])^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
}

plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
     xlab="Smoothing bandwidth", ylab="LOOCV prediction error")

which(CV_err_h == min(CV_err_h))

h_seq[which(CV_err_h == min(CV_err_h))]

#Multivariate
k_is_marr_one <- npregbw(is_marr_one$net_tfa ~ is_marr_one$age + is_marr_one$inc + 
                    is_marr_one$fsize + is_marr_one$educ,
                  ckertype = 'uniform')
reg_is_marr_one <- npreg(k_is_marr_one)
summary(reg_is_marr_one)

k_is_marr_two <- npregbw(is_marr_two$net_tfa ~ is_marr_two$age + is_marr_two$inc + 
                           is_marr_two$fsize + is_marr_two$educ,
                         ckertype = 'uniform')
reg_is_marr_two <- npreg(k_is_marr_two)
summary(reg_is_marr_two)


k_is_notmarr_one <- npregbw(is_notmarr_one$net_tfa ~ is_notmarr_one$age + is_notmarr_one$inc + 
                           is_notmarr_one$fsize + is_notmarr_one$educ,
                         ckertype = 'uniform')
reg_is_notmarr_on <- npreg(k_is_notmarr_one)
summary(reg_is_notmarr_on)

#part b

#marr = 1 & twoearn = 0
Y <- as.matrix(is_marr_one[,1])
X <- as.matrix(is_marr_one[,2:5])
n = nrow(is_marr_one)

# n: sample size
h_seq = seq(from=1,to=10, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))
is_marr_one_predict = FNN::knn.reg(X,y = Y,k = k)
summary(is_marr_one_predict$pred)

plot1 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
     xlab="KNN", ylab="LOOCV prediction error")

#marr = 1 & twoearn = 1
Y <- as.matrix(is_marr_two[,1])
X <- as.matrix(is_marr_two[,2:5])
n = nrow(is_marr_two)

# n: sample size
h_seq = seq(from=1,to=10, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))
is_marr_two_predict = FNN::knn.reg(X,y = Y,k = k)
summary(is_marr_two_predict$pred)

plot2 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")


#marr = 0 & twoearn = 0
Y <- as.matrix(is_notmarr_one[,1])
X <- as.matrix(is_notmarr_one[,2:5])
n = nrow(is_notmarr_one)

# n: sample size
h_seq = seq(from=1,to=10, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))
is_notmarr_one_predict = FNN::knn.reg(X,y = Y[,-2],k = k)
summary(is_notmarr_one_predict$pred)

plot3 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")

#all
Y <- as.matrix(is[,1])
X <- as.matrix(is[,2:7])
n = nrow(is)

# n: sample size
h_seq = seq(from=1,to=10, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=5
is_predict = FNN::knn.reg(X,y = Y,k = k)
summary(is_predict$pred)

plot <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")


#Part c

#marr = 1 & twoearn = 0
Y <- as.matrix(is_marr_one[,1])
X <- as.matrix(is_marr_one[,2:5])
n = nrow(is_marr_one)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_marr_one_val = is_marr_one[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_marr_one_tr = is_marr_one[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_marr_one_tr$net_tfa ~ bs(is_marr_one_tr$age + is_marr_one_tr$inc +
                                             is_marr_one_tr$fsize + is_marr_one_tr$educ, 
                                           degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h)) #k=4
is_marr_one_predict = gam(is_marr_one$net_tfa ~ bs(is_marr_one$age + is_marr_one$inc +
                                                     is_marr_one$fsize + is_marr_one$educ, 
                                                   degree = k))
summary(is_marr_one_predict)

plot4 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#marr = 1 & twoearn = 1
Y <- as.matrix(is_marr_two[,1])
X <- as.matrix(is_marr_two[,2:5])
n = nrow(is_marr_two)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_marr_two_val = is_marr_two[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_marr_two_tr = is_marr_two[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_marr_two_tr$net_tfa ~ bs(is_marr_two_tr$age + is_marr_two_tr$inc +
                                             is_marr_two_tr$fsize + is_marr_two_tr$educ, 
                                           degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h)) #k=1
is_marr_two_predict = gam(is_marr_two$net_tfa ~ bs(is_marr_two$age + is_marr_two$inc +
                                                     is_marr_two$fsize + is_marr_two$educ, 
                                                   degree = k))
summary(is_marr_two_predict)

plot5 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#marr = 0 & twoearn = 0
Y <- as.matrix(is_notmarr_one[,1])
X <- as.matrix(is_notmarr_one[,2:5])
n = nrow(is_notmarr_one)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_notmarr_one_val = is_notmarr_one[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_notmarr_one_tr = is_notmarr_one[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_notmarr_one_tr$net_tfa ~ bs(is_notmarr_one_tr$age + is_notmarr_one_tr$inc +
                                             is_notmarr_one_tr$fsize + is_notmarr_one_tr$educ, 
                                           degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=1
is_notmarr_one_predict = gam(is_notmarr_one$net_tfa ~ bs(is_notmarr_one$age + is_notmarr_one$inc +
                                                     is_notmarr_one$fsize + is_notmarr_one$educ, 
                                                   degree = k))
summary(is_notmarr_one_predict)

plot6 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#all
Y <- as.matrix(is[,1])
X <- as.matrix(is[,2:7])
n = nrow(is)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_val = is[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_tr = is[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_tr$net_tfa ~ bs(is_tr$age + is_tr$inc + is_tr$fsize + is_tr$educ 
                                  + is_tr$marr + is_tr$twoearn, 
                                              degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=1
is_predict = gam(is$net_tfa ~ bs(is$age + is$inc + is$fsize + is$educ 
                                    + is$marr + is$twoearn, 
                                    degree = k))
summary(is_predict)

plot6 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#Part d
linear_est <- lm(net_tfa ~ age + inc + fsize + educ + marr + twoearn,
                 data = is)
robust_linear_est <- coeftest(linear_est, vcov = vcovHC, type = 'HC1')

stargazer::stargazer(robust_linear_est)

#Part e

#Kernel Regression

k_os_marr_one <- npregbw(os_marr_one$net_tfa ~ os_marr_one$age + os_marr_one$inc + 
                           os_marr_one$fsize + os_marr_one$educ,
                         ckertype = 'uniform')
reg_os_marr_one <- npreg(k_os_marr_one)
summary(reg_os_marr_one)

Kernel_MSE_os_marr_one <- reg_os_marr_one$MSE
Kernel_MAE_os_marr_one <- reg_os_marr_one$MAE

k_os_marr_two <- npregbw(os_marr_two$net_tfa ~ os_marr_two$age + os_marr_two$inc + 
                           os_marr_two$fsize + os_marr_two$educ,
                         ckertype = 'uniform')
reg_os_marr_two <- npreg(k_os_marr_two)
summary(reg_os_marr_two)

Kernel_MSE_os_marr_two <- reg_os_marr_two$MSE
Kernel_MAE_os_marr_two <- reg_os_marr_two$MAE

k_os_notmarr_one <- npregbw(os_notmarr_one$net_tfa ~ os_notmarr_one$age + os_notmarr_one$inc + 
                              os_notmarr_one$fsize + os_notmarr_one$educ,
                            ckertype = 'uniform')
reg_os_notmarr_one <- npreg(k_os_notmarr_one)
summary(reg_os_notmarr_one)

Kernel_MSE_os_notmarr_one <- reg_os_notmarr_one$MSE
Kernel_MAE_os_notmarr_one <- reg_os_notmarr_one$MAE

#KNN

#k=7
Y_os_marr_one <- as.matrix(os_marr_one[,1])
X_os_marr_one <- as.matrix(os_marr_one[,2:5])
os_marr_one_predict = FNN::knn.reg(X_os_marr_one,y = Y_os_marr_one ,k = 7)
summary(os_marr_one_predict$pred)

KNN_MSE_os_marr_one <- mean((os_marr_one_predict$pred-Y_os_marr_one)^2)
KNN_MAE_os_marr_one <- MAE(os_marr_one_predict$pred,Y_os_marr_one)

#k=8
Y_os_marr_two <- as.matrix(os_marr_two[,1])
X_os_marr_two <- as.matrix(os_marr_two[,2:5])
os_marr_two_predict = FNN::knn.reg(X_os_marr_two,y = Y_os_marr_two,k = 8)
summary(os_marr_two_predict$pred)

KNN_MSE_os_marr_two <- mean((os_marr_two_predict$pred-Y_os_marr_two)^2)
KNN_MAE_os_marr_two <- MAE(os_marr_two_predict$pred,Y_os_marr_two)

#k=2
Y_os_notmarr_one <- as.matrix(os_notmarr_one[,1])
X_os_notmarr_one <- as.matrix(os_notmarr_one[,2:5])
os_notmarr_one_predict = FNN::knn.reg(X_os_notmarr_one,y = Y_os_notmarr_one[,-2],k = 2)
summary(os_notmarr_one_predict$pred)

KNN_MSE_os_notmarr_one <- mean((os_notmarr_one_predict$pred-Y_os_notmarr_one)^2)
KNN_MAE_os_notmarr_one <- MAE(os_notmarr_one_predict$pred,Y_os_notmarr_one)

#k=5
Y_os <- as.matrix(os[,1])
X_os <- as.matrix(os[,2:7])
os_predict = FNN::knn.reg(X_os,y = Y_os,k = 5)
summary(os_predict$pred)

KNN_MSE_os <- mean((os_predict$pred-Y_os)^2)
KNN_MAE_os <- MAE(os_predict$pred,Y_os)

#Series

#k=4
os_marr_one_predict = gam(os_marr_one$net_tfa ~ bs(os_marr_one$age + os_marr_one$inc +
                                                     os_marr_one$fsize + os_marr_one$educ, 
                                                   degree = 4))
summary(os_marr_one_predict)

Series_MSE_os_marr_one <- mean((os_marr_one_predict$fitted.values-Y_os_marr_one)^2)
Series_MAE_os_marr_one <- MAE(os_marr_one_predict$fitted.values,Y_os_marr_one)

#k=1
os_marr_two_predict = gam(os_marr_two$net_tfa ~ bs(os_marr_two$age + os_marr_two$inc +
                                                     os_marr_two$fsize + os_marr_two$educ, 
                                                   degree = 1))
summary(os_marr_two_predict)

Series_MSE_os_marr_two <- mean((os_marr_two_predict$fitted.values-Y_os_marr_two)^2)
Series_MAE_os_marr_two <- MAE(os_marr_two_predict$fitted.values,Y_os_marr_two)

#k=1
os_notmarr_one_predict = gam(os_notmarr_one$net_tfa ~ bs(os_notmarr_one$age + os_notmarr_one$inc +
                                                           os_notmarr_one$fsize + os_notmarr_one$educ, 
                                                         degree = 1))
summary(os_notmarr_one_predict)

Series_MSE_os_notmarr_one <- mean((os_notmarr_one_predict$fitted.values-Y_os_notmarr_one)^2)
Series_MAE_os_notmarr_one <- MAE(os_notmarr_one_predict$fitted.values,Y_os_notmarr_one)

#k=1
os_predict = gam(os$net_tfa ~ bs(os$age + os$inc + os$fsize + os$educ 
                                 + os$marr + os$twoearn, 
                                 degree = 1))
summary(os_predict)

Series_MSE_os <- mean((os_predict$fitted.values-Y_os)^2)
Series_MAE_os <- MAE(os_predict$fitted.values,Y_os)

#OLS

os_linear_est <- lm(net_tfa ~ age + inc + fsize + educ + marr + twoearn,
                 data = os)
os_robust_linear_est <- coeftest(linear_est, vcov = vcovHC, type = 'HC1')
print(robust_linear_est)

OLS_MSE_os <-mean((os_linear_est$fitted.values-os[,1])^2)
OLS_MAE_os <-MAE(os_linear_est$fitted.values,os[,1])

#Part f

#Subset is data
is_marr_two2 <- subset(is, marr == 1 & twoearn == 1)
is_marr_one2 <- subset(is, marr == 1 & twoearn == 0)
is_notmarr_one2 <- subset(is, marr == 0 & twoearn == 0)

#Subset os data
os_marr_two2 <- subset(os, marr == 1 & twoearn == 1)
os_marr_one2 <- subset(os, marr == 1 & twoearn == 0)
os_notmarr_one2 <- subset(os, marr == 0 & twoearn == 0)

#Multivariate
k2_is_marr_one <- npregbw(is_marr_one2$e401 ~ is_marr_one2$age + is_marr_one2$inc + 
                           is_marr_one2$fsize + is_marr_one2$educ,
                         ckertype = 'uniform')
reg2_is_marr_one <- npreg(k2_is_marr_one)
summary(reg2_is_marr_one)

k2_is_marr_two <- npregbw(is_marr_two2$e401 ~ is_marr_two2$age + is_marr_two2$inc + 
                           is_marr_two2$fsize + is_marr_two2$educ,
                         ckertype = 'uniform')
reg2_is_marr_two <- npreg(k2_is_marr_two)
summary(reg2_is_marr_two)


k2_is_notmarr_one <- npregbw(is_notmarr_one2$e401 ~ is_notmarr_one2$age + is_notmarr_one2$inc + 
                              is_notmarr_one2$fsize + is_notmarr_one2$educ,
                            ckertype = 'uniform')
reg2_is_notmarr_on <- npreg(k2_is_notmarr_one)
summary(reg2_is_notmarr_on)

#Part g

#marr = 1 & twoearn = 0
Y <- as.matrix(is_marr_one2[,8])
X <- as.matrix(is_marr_one2[,2:5])
n = nrow(is_marr_one2)

# n: sample size
h_seq = seq(from=1,to=15, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=10
is_marr_one_predict2 = FNN::knn.reg(X,y = Y,k = k)
summary(is_marr_one_predict2$pred)

plot7 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")

#marr = 1 & twoearn = 1
Y <- as.matrix(is_marr_two2[,8])
X <- as.matrix(is_marr_two2[,2:5])
n = nrow(is_marr_two2)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=17
is_marr_two_predict2 = FNN::knn.reg(X,y = Y,k = k)
summary(is_marr_two_predict2$pred)

plot8 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")


#marr = 0 & twoearn = 0
Y <- as.matrix(is_notmarr_one2[,8])
X <- as.matrix(is_notmarr_one2[,2:5])
n = nrow(is_notmarr_one2)

# n: sample size
h_seq = seq(from=1,to=10, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=3
is_notmarr_one_predict2 = FNN::knn.reg(X,y = Y[,-2],k = k)
summary(is_notmarr_one_predict2$pred)

plot9 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")

#all
Y <- as.matrix(is[,8])
X <- as.matrix(is[,2:7])
n = nrow(is_marr_one2)

# n: sample size
h_seq = seq(from=1,to=15, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i,]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i,]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = FNN::knn.reg(X_tr, y= Y_tr, k = h_using)
    w <- (Y_val - Y_val_predict$pred)^2
    CV_err[i] = (Y_val - Y_val_predict$pred)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=15
is_predict2 = FNN::knn.reg(X,y = Y,k = k)
summary(is_predict2$pred)

plot7 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="KNN", ylab="LOOCV prediction error")

#Part h

#marr = 1 & twoearn = 0
Y <- as.matrix(is_marr_one[,8])
X <- as.matrix(is_marr_one[,2:5])
n = nrow(is_marr_one)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_marr_one_val = is_marr_one[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_marr_one_tr = is_marr_one[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_marr_one_tr$e401 ~ bs(is_marr_one_tr$age + is_marr_one_tr$inc +
                                             is_marr_one_tr$fsize + is_marr_one_tr$educ, 
                                           degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h)) #k=14
is_marr_one_predict2 = gam(is_marr_one$e401 ~ bs(is_marr_one$age + is_marr_one$inc +
                                                     is_marr_one$fsize + is_marr_one$educ, 
                                                   degree = k))
summary(is_marr_one_predict2)
t <- is_notmarr_one_predict2$pred >1
sum(t)
plot4 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#marr = 1 & twoearn = 1
Y <- as.matrix(is_marr_two[,8])
X <- as.matrix(is_marr_two[,2:5])
n = nrow(is_marr_two)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_marr_two_val = is_marr_two[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_marr_two_tr = is_marr_two[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_marr_two_tr$e401 ~ bs(is_marr_two_tr$age + is_marr_two_tr$inc +
                                             is_marr_two_tr$fsize + is_marr_two_tr$educ, 
                                           degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h)) #k=1
is_marr_two_predict = gam(is_marr_two$e401 ~ bs(is_marr_two$age + is_marr_two$inc +
                                                     is_marr_two$fsize + is_marr_two$educ, 
                                                   degree = k))
summary(is_marr_two_predict)

plot5 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#marr = 0 & twoearn = 0
Y <- as.matrix(is_notmarr_one[,8])
X <- as.matrix(is_notmarr_one[,2:5])
n = nrow(is_notmarr_one)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_notmarr_one_val = is_notmarr_one[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_notmarr_one_tr = is_notmarr_one[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_notmarr_one_tr$e401 ~ bs(is_notmarr_one_tr$age + is_notmarr_one_tr$inc +
                                                is_notmarr_one_tr$fsize + is_notmarr_one_tr$educ, 
                                              degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=1
is_notmarr_one_predict = gam(is_notmarr_one$e401 ~ bs(is_notmarr_one$age + is_notmarr_one$inc +
                                                           is_notmarr_one$fsize + is_notmarr_one$educ, 
                                                         degree = k))
summary(is_notmarr_one_predict)

plot6 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#all
Y <- as.matrix(is[,8])
X <- as.matrix(is[,2:7])
n = nrow(is)

# n: sample size
h_seq = seq(from=1,to=20, by=1)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    is_val = is[i,]
    #X_val = X[i,]
    #Y_val = Y[i]
    # validation set
    is_tr = is[-i,]
    #X_tr = X[-i,]
    #Y_tr = Y[-i]
    # training set
    fit <- gam(is_tr$e401 ~ bs(is_tr$age + is_tr$inc + is_tr$fsize + is_tr$educ
                               + is_tr$marr + is_tr$twoearn, 
                                           degree = h_using))
    #Y_val_predict = predict(fit, newdata = as.data.frame(X_tr), se = T)
    CV_err[i] = (Y_val - fit$fitted.values)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err, na.rm = TRUE)
} 

k <- as.numeric(which.min(CV_err_h))#k=14
is_predict2 = gam(is$e401 ~ bs(is$age + is$inc + is$fsize + is$educ
                                             + is$marr + is$twoearn, 
                                             degree = k))
summary(is_predict2)

plot6 <- plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
              xlab="Cubic Spline", ylab="LOOCV prediction error")

#Part i 
logit_data <- is

logit_data$marr <- factor(logit_data$marr)
logit_data$twoearn <- factor(logit_data$twoearn)
logit_data$e401 <- factor(logit_data$e401)

logit_predict <- glm(e401 ~ age + inc + fsize + educ + marr + twoearn
                     ,family=binomial(link='logit'), data=logit_data)
summary(logit_predict)
stargazer::stargazer(logit_predict)

#Part j

#create MCR function
class <- function(x){
  if(x>= .5){
    1}else{0}
}


k2_os_marr_one2 <- npregbw(os_marr_one2$e401 ~ os_marr_one2$age + os_marr_one2$inc + 
                            os_marr_one2$fsize + os_marr_one2$educ,
                          ckertype = 'uniform')
reg2_os_marr_one <- npreg(k2_os_marr_one2)
summary(reg2_os_marr_one)

Kernel_MSE_os_marr_one2 <- reg2_os_marr_one$MSE
Kernel_MCR_os_marr_one2 <- mean((reg2_os_marr_one$mean - os_marr_one2$e401 >= .5))


k2_os_marr_two <- npregbw(os_marr_two2$e401 ~ os_marr_two2$age + os_marr_two2$inc + 
                            os_marr_two2$fsize + os_marr_two2$educ,
                          ckertype = 'uniform')
reg2_os_marr_two <- npreg(k2_os_marr_two)
summary(reg2_os_marr_two)

Kernel_MSE_os_marr_two2 <- reg2_os_marr_two$MSE
Kernel_MCR_os_marr_two2 <- mean((reg2_os_marr_two$mean - os_marr_two2$e401 >= .5))

k2_os_notmarr_one <- npregbw(os_notmarr_one2$e401 ~ os_notmarr_one2$age + os_notmarr_one2$inc + 
                               os_notmarr_one2$fsize + os_notmarr_one2$educ,
                             ckertype = 'uniform')
reg2_os_notmarr_one <- npreg(k2_os_notmarr_one)
summary(reg2_os_notmarr_one)

Kernel_MSE_os_notmarr_one2 <- reg2_os_notmarr_one$MSE
Kernel_MCR_os_notmarr_one2 <- mean((reg2_os_notmarr_one$mean - os_notmarr_one2$e401 >= .5))

#KNN

#k=10
Y_os_marr_one2 <- as.matrix(is_marr_one2[,8])
X <- as.matrix(is_marr_one2[,2:5])
os_marr_one_predict2 = FNN::knn.reg(X,y = Y_os_marr_one2,k = 10)

KNN_MSE_os_marr_one2 <- mean((os_marr_one_predict2$pred-Y_os_marr_one2)^2)
KNN_MCR_os_marr_one2 <- mean((os_marr_one_predict2$pred -Y_os_marr_one2>= .5))

#k=17
Y_os_marr_two2 <- as.matrix(is_marr_two2[,8])
X <- as.matrix(is_marr_two2[,2:5])
os_marr_two_predict2 = FNN::knn.reg(X,y = Y_os_marr_two2,k = 17)
summary(is_marr_two_predict2$pred)

KNN_MSE_os_marr_two2 <- mean((os_marr_two_predict2$pred-Y_os_marr_two2)^2)
KNN_MCR_os_marr_two2 <- mean((os_marr_two_predict2$pred -Y_os_marr_two2>= .5))


#k=3
Y_os_notmarr_one2 <- as.matrix(is_notmarr_one2[,8])
X <- as.matrix(is_notmarr_one2[,2:5])
os_notmarr_one_predict2 = FNN::knn.reg(X,y = Y_os_notmarr_one2[,-2],k = 3)

KNN_MSE_os_notmarr_one2 <- mean((os_notmarr_one_predict2$pred-Y_os_notmarr_one2)^2)
KNN_MCR_os_notmarr_one2 <- mean((os_notmarr_one_predict2$pred - Y_os_notmarr_one2 >= .5))

#k=15
Y <- as.matrix(os[,8])
X <- as.matrix(os[,2:7])
os_predict2 = FNN::knn.reg(X,y = Y,k = 15)

KNN_MSE_os2 <- mean((os_predict2$pred-Y)^2)
KNN_MCR_os2 <- mean((os_predict2$pred >= .5))

#Series

#k=14
Y <- as.matrix(os_marr_one[,8])
X <- as.matrix(os_marr_one[,2:5])

os_marr_one_predict2 = gam(os_marr_one$e401 ~ bs(os_marr_one$age + os_marr_one$inc +
                                                   os_marr_one$fsize + os_marr_one$educ, 
                                                 degree = 14))

Series_MSE_os_marr_one2 <- mean((Y-os_marr_one_predict2$fitted.values)^2)

t <- os_marr_one_predict2$fitted.values
s <- lapply(t,class)
s <- as.numeric(unlist(s))

Series_MCR_os_marr_one2 <- mean((s - Y >= .5))

#k=1
Y <- as.matrix(os_marr_two[,8])
X <- as.matrix(os_marr_two[,2:5])

os_marr_two_predict2 = gam(os_marr_two$e401 ~ bs(os_marr_two$age + os_marr_two$inc +
                                                  os_marr_two$fsize + os_marr_two$educ, 
                                                degree = 1))

Series_MSE_os_marr_two2 <- mean((Y-os_marr_two_predict2$fitted.values)^2)

t <- os_marr_two_predict2$fitted.values
s <- lapply(t,class)
s <- as.numeric(unlist(s))

Series_MCR_os_marr_two2 <- mean((s - Y >= .5))

#k=1
Y <- as.matrix(os_notmarr_one[,8])
X <- as.matrix(os_notmarr_one[,2:5])
os_notmarr_one_predict2 = gam(os_notmarr_one$e401 ~ bs(os_notmarr_one$age + os_notmarr_one$inc +
                                                        os_notmarr_one$fsize + os_notmarr_one$educ, 
                                                      degree = 1))
Series_MSE_os_notmarr_one2 <- mean((Y-os_notmarr_one_predict2$fitted.values)^2)

t <- os_notmarr_one_predict2$fitted.values
s <- lapply(t,class)
s <- as.numeric(unlist(s))

Series_MCR_os_notmarr_one2 <- mean((s - Y >= .5))

#k=14
Y <- as.matrix(os[,8])
X <- as.matrix(os[,2:7])
os_predict2 = gam(os$e401 ~ bs(os$age + os$inc + os$fsize + os$educ
                               + os$marr + os$twoearn, 
                               degree = 14))
Series_MSE_os2 <- mean((Y-os_predict2$pred)^2)

t <- os_predict2$pred
s <- lapply(t,class)
s <- as.numeric(unlist(s))

Series_MCR_os2 <- mean((os_predict2$pred -Y>= .5))

#logit

os_logit<- os

os_logit$marr <- factor(os_logit$marr)
os_logit$twoearn <- factor(os_logit$twoearn)
#os_logit$e401 <- factor(os_logit$e401)
Y <- os_logit[,8]

logit_predict_os <- glm(e401 ~ age + inc + fsize + educ + marr + twoearn
                     ,family=binomial(link='logit'), data=os_logit)

logit_MSE_os2 <- mean((Y - logit_predict_os$residuals)^2)

#logit_MCR_os2 <- mean((logit_predict_os$fitted - Y>= .5))
t< - logit_predict_os$fitted
s <- lapply(t,class)
s <- as.numeric(unlist(s))
logit_MCR_os2 <- mean((s-Y)^2)
