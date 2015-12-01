if( Sys.info()['sysname'] == "Windows"){
    setwd("C:/Users/thealy/G_WD/Machine_Learning/R Scripts")
} else {
    setwd("~/G_WD/Machine_Learning/R Scripts")
}

dat1 = read.csv('ex1data1.txt')
colnames(dat1) = c('x_1', 'y')


dat2 = read.csv('ex1data2.txt')
colnames(dat2) = c("x_1", "x_2", "y")


Regression = function(dat, input) {
    X = as.matrix(dat[,1:(ncol(dat) - 1)])
    X = cbind(x_0 = 1, X)
    y = dat$y
    m = nrow(X)
    iterations = 1500
    alpha = 0.01
    theta = vector(mode = "numeric",length = ncol(X))
    
    J = function(theta) {
        error_sqr = t(X %*% theta - y) %*% (X %*% theta - y)
        (1 / (2 * m)) * error_sqr
    }
    
    result <- optim(par = theta, J)
    v = c(1, input)
    h = v %*% result$par
    
    print(c("Thetas:", result$par))
    print(c("Regression Result:", h))
}

Regression(dat1, 3.5)
Regression(dat2, c(1650, 3))