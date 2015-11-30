if( Sys.info()['sysname'] == "Windows"){
    setwd("C:/Users/thealy/G_WD/Machine_Learning/R Scripts")
} else {
    setwd("~/G_WD/Machine_Learning/R Scripts")
}


dat1 = read.csv('ex1data1.txt')
colnames(dat1) = c('x_1', 'y')
dat1 = cbind(x_0 = 1, dat1)

X = as.matrix(dat1[,1:2])
y = as.vector(dat1$y)
m = length(X)
iterations = 1500
alpha = 0.01
theta = vector(mode = "numeric",length = 2)

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
paste(c("Theta_0 found by gradient descent:"), theta[1] )
paste(c("Theta_1 found by gradient descent:"), theta[2] )
