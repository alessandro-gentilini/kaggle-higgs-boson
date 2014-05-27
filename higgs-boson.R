# See http://pingax.com/logistic-regression-r-step-step-implementation-part-2/
df <- read.csv("training.csv",head=T,sep=",")

# Convert -999 to NA
df[df==-999]<-NA

# Remove NAs
df<-na.omit(df)

# Predictor variables 
X <- as.matrix(df[,seq(from=2,to=32,by=1)])

# Add ones to X 
X <- cbind(rep(1,nrow(X)),X)

# Response variable 
Y <- as.matrix(df$Label)
Y[Y=="s"]<-1
Y[Y=="b"]<-0
Y<-as.numeric(Y)


sigmoid <- function(z) 
{ 
  g <- 1/(1+exp(-z)) 
  return(g) 
}

cost <- function(theta) 
{ 
  m <- nrow(X) 
  g <- sigmoid(X%*%theta) 
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g))) 
  return(J) 
}

# Initial theta 
initial_theta <- rep(0,ncol(X))  

# Cost at inital theta 
cost(initial_theta)

# Derive theta using gradient descent using optim function 
theta_optim <- optim(par=initial_theta,fn=cost)

prediction = rep(0,nrow(X))

for ( i in seq(1,nrow(X)))
{
  p = sigmoid(t(as.numeric(X[i,]))%*%theta_optim$par)
  if ( p > 0.5 ) {
    prediction[i] = 1
  } else {
    prediction[i] = 0  
  }
}

print(table(prediction==Y))