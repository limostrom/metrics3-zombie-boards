library(ivreg,dplyr,ggplot2,tidyr,ggpubr,stargazer)
#Simulation Parameters
set.seed(12345)
N <- 100
T <- 6
M <- 1000
y <- matrix(NA, N,T+1)
u <- matrix(NA, N,T)
p <- matrix(c(0,0.5,0.95))
p_j <- matrix(NA,N,5)


p_fe <- matrix(NA,M,3)
colnames(p_fe) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
p_fd <- matrix(NA,M,3)
colnames(p_fd) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
p_hk <- matrix(NA,M,3)
colnames(p_hk) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
sump_jk <- matrix(NA, M, 3)
colnames(sump_jk) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
p_jk <- matrix(NA,M,3)
colnames(p_jk) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')

bias_fe <- matrix(0,3,1)
bias_fd <- matrix(0,3,1)
bias_hk <- matrix(0,3,1)
bias_jk <- matrix(0,3,1)

rmse_fe <- matrix(0,3,1)
rmse_fd <- matrix(0,3,1)
rmse_hk <- matrix(0,3,1)
rmse_jk <- matrix(0,3,1)

#Simulation
for (l in 1:3) {
  for (j in 1:M){
    
    #initialize variables
    u <- matrix(NA, N, T)
    alpha <- rnorm(N, mean = 0, sd = 1)
    y[ ,1] <- rnorm(N, mean = alpha/(1-p[l]), sd = 1/(1-p[l]^2))
    for (i in 1:T) {
      u[ ,i] <- rnorm(N, mean = 0, sd = 1)
    }
    
    #create y matrix
    for (i in 1:T){
      y[ ,i+1] = p[l]*y[ ,i] + alpha + u[ , i]
    }
    
    #create dependent and independent variable
    yy <- y[,2:(T+1)]
    ylag <- y[,1:T]
    
    #demean
    demean_y <- yy-rowMeans(yy)
    demean_lag <- ylag-rowMeans(ylag)
    #Estimation, time for bidness
    
    #fixed effects estimator
    Y_fe <- t(matrix(demean_y, nrow = 1))
    lag_fe <- t(matrix(demean_lag, nrow = 1))
    fe <- lm(Y_fe ~ lag_fe)
    p_fe[j,l] <- fe$coefficients[2]
    
    #first-differences estimator w/ IV
    y_fd <- matrix(NA,N,T-2)
    lag_fd <- matrix(NA,N,T-2)
    inst <- matrix(NA,N,T-2)
    for (k in 1:(T-2)) {
      y_fd[ ,k] <- y[ ,k+2] - y[ ,k+1]
      lag_fd[ ,k] <- y[ ,k+1] - y[ ,k]
      inst[ ,k] <- y[ ,k]
    }
    Y_fd <- t(matrix(y_fd, nrow = 1))
    Lagged_fd <- t(matrix(lag_fd, nrow = 1))
    Instrument <- t(matrix(inst, nrow = 1))
    fd <- ivreg(Y_fd ~ Lagged_fd|Instrument)
    p_fd[j,l] <- fd$coefficients[2] 
    
    #Hahn-Kuersteiner
    p_hk[j,l] <- (1+1/T)*p_fe[j,l]+1/T
    
    #jackknife
    x <- matrix(0, T, 3)
    b <- matrix(0, M, 3)
    for (u in 1:T) {
      #t-1 matrix
      t <- y[,-u]
      #estimation
      yj <- t[,2:(T)]
      yjlag <- t[,1:(T-1)]
      demean_yj <- yj-rowMeans(yj)
      demean_lagj <- yjlag-rowMeans(yjlag)
      Yj_fe <- t(matrix(demean_yj, nrow = 1))
      lagj_fe <- t(matrix(demean_lagj, nrow = 1))
      fej <- lm(Yj_fe ~ lagj_fe)
      z <- fej$coefficients[2]
      x[u,l] <- z
    }
    #sum of rho-t values
    b <- colSums(x)
    b <- matrix(b, nrow = 1, ncol = 3)
    #estimate bias corrected rho
    p_jk[j,l] <- (T*p_fe[j,l])-((T-1)/T)*(b[,l])
  }
  #compute bias
  bias_fe[l] <- mean(p_fe[ ,l])-p[l]
  bias_fd[l] <- mean(p_fd[ ,l])-p[l]
  bias_hk[l] <- mean(p_hk[ ,l])-p[l]
  bias_jk[l] <- mean(p_jk[ ,l])-p[l]
  #compute root-MSE
  rmse_fe[l] <- sqrt(mean((p_fe[,l]-p[l])^2))
  rmse_fd[l] <- sqrt(mean((p_fd[,l]-p[l])^2))
  rmse_hk[l] <- sqrt(mean((p_hk[,l]-p[l])^2))
  rmse_jk[l] <- sqrt(mean((p_jk[,l]-p[l])^2))
}

hist1_fe <- qplot(p_fe[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_fe <- qplot(p_fe[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_fe <- qplot(p_fe[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

hist1_fd <- qplot(p_fd[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_fd <- qplot(p_fd[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_fd <- qplot(p_fd[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

hist1_hk <- qplot(p_hk[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_hk <- qplot(p_hk[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_hk <- qplot(p_hk[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

hist1_jk <- qplot(p_jk[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_jk <- qplot(p_jk[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_jk <- qplot(p_jk[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

ggarrange(hist1_fe, hist2_fe, hist3_fe, hist1_fd, hist2_fd, hist3_fd,hist1_hk, 
          hist2_hk, hist3_hk, hist1_jk, hist2_jk, hist3_jk,
          labels = c("FE(1)", "FE(2)", "FE(3)", "FD(1)", "FD(2)", "FD(3)","HK(1)"
                     , "HK(2)",  "HK(3)", "JK(1)", "JK(2)","JK(3)"),
          ncol = 3, nrow = 4)

# create table for bias and rmse T=6
bias6 <- cbind(bias_fe,bias_fd,bias_hk,bias_jk)
rmse6 <- cbind(rmse_fe,rmse_fd,rmse_hk,rmse_jk)
output6 <- rbind(bias6,rmse6)
colnames(output6) <- c("Fixed-Effects","First-Difference","Hahn-Kuersteiner","Jackknife")
rownames(output6) <- c("Bias_rho=0","Bias_rho=0.5","Bias_rho=0.95","RMSE_rho=0","RMSE_rho=0.5","RMSE_rho=0.95")
stargazer(output6,title="Q1 T=6",align=TRUE,digits=3)

#Simulation Parameters for T = 25
N <- 100
T <- 25
M <- 1000
y <- matrix(NA, N,T+1)
u <- matrix(NA, N,T)
p <- matrix(c(0,0.5,0.95))

p_fe <- matrix(NA,M,3)
colnames(p_fe) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
p_fd <- matrix(NA,M,3)
colnames(p_fd) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
p_hk <- matrix(NA,M,3)
colnames(p_hk) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
sump_jk <- matrix(NA, M, 3)
colnames(sump_jk) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')
p_jk <- matrix(NA,M,3)
colnames(p_jk) <- c('p = 0.0', 'p = 0.5', 'p = 0.95')

bias_fe <- matrix(0,3,1)
bias_fd <- matrix(0,3,1)
bias_hk <- matrix(0,3,1)
bias_jk <- matrix(0,3,1)

rmse_fe <- matrix(0,3,1)
rmse_fd <- matrix(0,3,1)
rmse_hk <- matrix(0,3,1)
rmse_jk <- matrix(0,3,1)

#Simulation for T = 25
for (l in 1:3) {
  for (j in 1:M){
    
    #initialize variables
    u <- matrix(NA, N, T)
    alpha <- rnorm(N, mean = 0, sd = 1)
    y[ ,1] <- rnorm(N, mean = alpha/(1-p[l]), sd = 1/(1-p[l]^2))
    for (i in 1:T) {
      u[ ,i] <- rnorm(N, mean = 0, sd = 1)
    }
    
    #create y matrix
    for (i in 1:T){
      y[ ,i+1] = p[l]*y[ ,i] + alpha + u[ , i]
    }
    
    #create dependent and independent variable
    yy <- y[,2:(T+1)]
    ylag <- y[,1:T]
    
    #demean
    demean_y <- yy-rowMeans(yy)
    demean_lag <- ylag-rowMeans(ylag)
    #Estimation, time for bidness
    
    #fixed effects estimator
    Y_fe <- t(matrix(demean_y, nrow = 1))
    lag_fe <- t(matrix(demean_lag, nrow = 1))
    fe <- lm(Y_fe ~ lag_fe)
    p_fe[j,l] <- fe$coefficients[2]
    
    #first-differences estimator w/ IV
    y_fd <- matrix(,N,T-2)
    lag_fd <- matrix(,N,T-2)
    inst <- matrix(,N,T-2)
    for (k in 1:(T-2)) {
      y_fd[ ,k] <- y[ ,k+2] - y[ ,k+1]
      lag_fd[ ,k] <- y[ ,k+1] - y[ ,k]
      inst[ ,k] <- y[ ,k]
    }
    Y_fd <- t(matrix(y_fd, nrow = 1))
    Lagged_fd <- t(matrix(lag_fd, nrow = 1))
    Instrument <- t(matrix(inst, nrow = 1))
    fd <- ivreg(Y_fd ~ Lagged_fd|Instrument)
    p_fd[j,l] <- fd$coefficients[2] 
    
    #Hahn-Kuersteiner
    p_hk[j,l] <- (1+1/T)*p_fe[j,l]+1/T
    
    #jackknife
    x <- matrix(0, T, 3)
    b <- matrix(0, M, 3)
    for (u in 1:T) {
      #t-1 matrix
      t <- y[,-u]
      #estimation
      yj <- t[,2:(T)]
      yjlag <- t[,1:(T-1)]
      demean_yj <- yj-rowMeans(yj)
      demean_lagj <- yjlag-rowMeans(yjlag)
      Yj_fe <- t(matrix(demean_yj, nrow = 1))
      lagj_fe <- t(matrix(demean_lagj, nrow = 1))
      fej <- lm(Yj_fe ~ lagj_fe)
      z <- fej$coefficients[2]
      x[u,l] <- z
    }
    #sum of rho-t values
    b <- colSums(x)
    b <- matrix(b, nrow = 1, ncol = 3)
    #estimate bias corrected rho
    p_jk[j,l] <- (T*p_fe[j,l])-((T-1)/T)*(b[,l])
  }
  #compute bias
  bias_fe[l] <- mean(p_fe[ ,l])-p[l]
  bias_fd[l] <- mean(p_fd[ ,l])-p[l]
  bias_hk[l] <- mean(p_hk[ ,l])-p[l]
  bias_jk[l] <- mean(p_jk[ ,l])-p[l]
  #compute root-MSE
  rmse_fe[l] <- sqrt(mean((p_fe[,l]-p[l])^2))
  rmse_fd[l] <- sqrt(mean((p_fd[,l]-p[l])^2))
  rmse_hk[l] <- sqrt(mean((p_hk[,l]-p[l])^2))
  rmse_jk[l] <- sqrt(mean((p_jk[,l]-p[l])^2))
}

hist1_fe <- qplot(p_fe[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_fe <- qplot(p_fe[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_fe <- qplot(p_fe[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

hist1_fd <- qplot(p_fd[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_fd <- qplot(p_fd[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_fd <- qplot(p_fd[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

hist1_hk <- qplot(p_hk[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_hk <- qplot(p_hk[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_hk <- qplot(p_hk[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

hist1_jk <- qplot(p_jk[,1],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist2_jk <- qplot(p_jk[,2],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 
hist3_jk <- qplot(p_jk[,3],geom="histogram",xlab="rho estimate",ylab="frequency",fill=I("midnightblue"),col=I("white")) 

ggarrange(hist1_fe, hist2_fe, hist3_fe, hist1_fd, hist2_fd, hist3_fd,hist1_hk, 
          hist2_hk, hist3_hk, hist1_jk, hist2_jk, hist3_jk,
          labels = c("FE(1)", "FE(2)", "FE(3)", "FD(1)", "FD(2)", "FD(3)","HK(1)"
                     , "HK(2)",  "HK(3)", "JK(1)", "JK(2)","JK(3)"),
          ncol = 3, nrow = 4)

# create table for bias and rmse T=25
bias25 <- cbind(bias_fe,bias_fd,bias_hk,bias_jk)
rmse25 <- cbind(rmse_fe,rmse_fd,rmse_hk,rmse_jk)
output25 <- rbind(bias25,rmse25)
colnames(output25) <- c("Fixed-Effects","First-Difference","Hahn-Kuersteiner","Jackknife")
rownames(output25) <- c("Bias_rho=0","Bias_rho=0.5","Bias_rho=0.95","RMSE_rho=0","RMSE_rho=0.5","RMSE_rho=0.95")
stargazer(output25,title="Q1 T=25",align=TRUE,digits=3)
