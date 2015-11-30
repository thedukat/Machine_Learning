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
    X = scale(X)
    X = cbind(x_0 = 1, X)
    y = dat$y
    m = nrow(X)
    iterations = 1500
    alpha = 0.01
    theta = vector(mode = "numeric",length = ncol(X))
    
    
    J = function(X, y, theta) {
        h = X %*% theta
        error = h - y
        error_sqr = error^2
        J = (1 / (2 * m)) * sum(error_sqr)
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
    h_theta = function(input) {
        v = c(1, input)
        calc = v %*% theta
        calc
    }
    print(c("Thetas:", theta))
    print(c("Result:", h_theta(input)))
    
} 

Regress(dat1, 3.5)
Regress(dat2, c(1650, 3))
