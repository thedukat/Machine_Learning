if( Sys.info()['sysname'] == "Windows"){
    setwd("C:/Users/thealy/G_WD/Machine_Learning/R Scripts")
} else {
    setwd("~/G_WD/Machine_Learning/R Scripts")
}


## ----------------------------- Single Variable Inputs -------------------------------------------

dat1 = read.csv('ex1data1.txt')
colnames(dat1) = c('x_1', 'y')


## -------------------------------- Multivariate Inputs -------------------------------------------

dat2 = read.csv('ex1data2.txt')
colnames(dat2) = c("x_1", "x_2", "y")


## -------------------------------------- Linear Regression ---------------------------------------

Regress = function(dat, input) {
    X = as.matrix(dat[,1:(ncol(dat) - 1)])
    mu = as.vector(colMeans(X))
    sigma = as.vector(apply(X, 2, sd))
    X = t((t(X) - mu) / sigma)
    X = cbind(x_0 = 1, X)
    y = dat$y
    m = nrow(X)
    iterations = 1500
    alpha = 0.01
    theta = vector(mode = "numeric",length = ncol(X))
    
    J = function(X, y, theta) {
        error_sqr = t(X %*% theta - y) %*% (X %*% theta - y)
        (1 / (2 * m)) * error_sqr
    }
    
    J_hist = vector()
    
    for( i in 1:iterations){
        theta = theta - alpha * (1 / m) * t(t((X %*% theta) - y) %*% X)
        J_hist = append(J_hist, J(X, y, theta))
    }
    
    plot(J_hist, type = "l")
    h_theta = function(input) {
        v = c(1, (t(input) - mu) / sigma)
        calc = v %*% theta
        calc
    }
    print(c("Thetas:", theta))
    print(c("Result:", h_theta(input)))
    
} 
    
Regress(dat1, 3.5)
Regress(dat2, c(1650, 3))
