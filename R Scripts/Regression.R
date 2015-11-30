if( Sys.info()['sysname'] == "Windows"){
    setwd("C:/Users/thealy/G_WD/Machine_Learning/R Scripts")
} else {
    setwd("~/G_WD/Machine_Learning/R Scripts")
}


## ----------------------------- Single Variable Regression ------------------------------##

dat1 = read.csv('ex1data1.txt')
colnames(dat1) = c('x_1', 'y')
dat1 = cbind(x_0 = 1, dat1)


## -------------------------------- Multivariate Regression -------------------------------##

dat2 = read.csv('ex1data2.txt')
colnames(dat2) = c("x_1", "x_2", "y")

## Feature Normalization
dat2$x_1 = (dat2$x_1 - mean(dat2$x_1) ) / sd(dat2$x_1)
dat2$x_2 = (dat2$x_2 - mean(dat2$x_2) ) / sd(dat2$x_2)
dat2 = cbind(x_0 = 1, dat2)


## -------------------------------------- Function ----------------------------------------##

Regress = function(dat, input) {
    X = as.matrix(dat[,1:(ncol(dat) - 1)])
    y = as.vector(dat$y)
    m = length(X)
    iterations = 1500
    alpha = 0.01
    theta = vector(mode = "numeric",length = ncol(X))
    
    
    J = function(X, y, theta) {
        h = X %*% theta
        error = h - y
        error_sqr = error^2
        1 / (2 * m) * sum(error_sqr)
    }
    
    J_hist = vector()
    
    for( i in 1:iterations){
        h = X %*% theta
        errors = h - y
        theta_change = (alpha / m) * (t(X) %*% errors)
        theta = theta - theta_change
        J_hist = append(J_hist, J(X, y, theta))
    }
    plot(J_hist, type = "l")
    paste(c("Theta found by gradient descent:"), theta )
    
    v = c(1, input)
    calc = v %*% theta
    paste(c("Regression Result:", calc))
    
}