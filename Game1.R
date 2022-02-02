setwd("~/Desktop/Econometrics/Econ 613/Game1")
#======Exerices 1 ==================
#crra function
crra = function(c,theta){
  utilities = rep(1,length(theta))
  for (i in seq(1,length(theta),1)){
    if (theta[i] >= .9997 & theta[i] <= 1.0003){
      utilities[i] = log(c)
    }else{
      utilities[i] = c^(1-theta[i])/(1-theta[i])
    }
  }
  return (utilities)
}
#shape
theta = seq(-0.5,1.5,0.001)
plot(theta,crra(50,theta))
#======Exerices 2 ==================
#read in the lotteries
data <- read.csv("dat_choices.csv")
data = subset(data, select = -c(X))
choices = matrix(rep(0,100),ncol=4,nrow=25,byrow= TRUE)
#list1
choices[1:5,1] = seq(48,16,-8)
choices[1:5,2] = seq(48,112,16)
choices[1:5,3] = seq(40,8,-8)
choices[1:4,4] = seq(64,112,16)
choices[5,4] = 120
#list2
choices[6:10,1] = seq(48,24,-6)
choices[6:10,2] = seq(48,120,18)
choices[6:9,3] = seq(42,24,-6)
choices[10,3] = 16
choices[6:9,4] = seq(66,120,18)
choices[10,4] = 128
#list3
choices[11:15,1] = seq(48,8,-10)
choices[11:15,2] = seq(48,104,14)
choices[11:14,3] = seq(38,8,-10)
choices[15,3] = 0
choices[11:14,4] = seq(62,104,14)
choices[15,4] = 112
#list4
choices[16:20,1] = seq(42,18,-6)
choices[16:20,2] = seq(42,114,18)
choices[16:19,3] = seq(36,18,-6)
choices[20,3] = 10
choices[16:19,4] = seq(60,114,18)
choices[20,4] = 122
#list5
choices[21:25,1] = seq(54,14,-10)
choices[21:25,2] = seq(54,110,14)
choices[21:24,3] = seq(44,14,-10)
choices[25,3] = 6
choices[21:24,4] = seq(68,110,14)
choices[25,4] = 118

#Find indifference theta
library(NLRoot)

diffUtil = function(lottery,fn,theta){
  result = 0.5*fn(lottery[1],theta)+0.5*fn(lottery[2],theta)-0.5*fn(lottery[3],theta)-0.5*fn(lottery[4],theta)
  return (result)
}
diffUtil(choices[1,],crra,0.2)

bisection <- function(f,lottery,fn, a, b, n = 1000, tol = 1e-9) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  if (!(f(lottery,fn,a) < 0) && (f(lottery,fn,b) > 0)) {
    stop('signs of f(a) and f(b) differ')
  } else if ((f(lottery,fn,a) > 0) && (f(lottery,fn,b) < 0)) {
    stop('signs of f(a) and f(b) differ')
  }
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    if ((f(lottery,fn,c) == 0) || ((b - a) / 2) < tol) {
      return(c)
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(lottery,fn,c)) == sign(f(lottery,fn,a)), 
           a <- c,
           b <- c)
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  print('Too many iterations')
}
bisection(f=diffUtil,choices[1,],fn=crra,-5,5)
theta = rep(0,25)
for (i in seq(1,25,1)){
  theta[i] = bisection(f=diffUtil,choices[i,],fn=crra,-5,10)
}
theta
indifferenceLevels  = theta
#======Exerices 3 ==================
library(EnvStats)
devd(1)
flike = function(choice,lotteries,fn,theta,w){
  like = 1
  for (i in seq(1,25,1)){
    lottery1 = lotteries[i,1:2]
    lottery2 = lotteries[i,3:4]
    uLottery1 = fn(w+lottery1[1],theta)+fn(w+lottery1[2],theta)
    uLottery2 = fn(w+lottery2[1],theta)+fn(w+lottery2[2],theta)
    pLot1 = plogis(uLottery1-uLottery2)
    pLot2 = 1- pLot1
    if(choice[i]==0){
      like = like*pLot1
    }else{
      like = like*pLot2
    }
  }
  return (like)
}
flike(data[1,],choices,crra,0.2,20)

#grid search
#for person 900
grid = seq(-5,5,0.001)
maxLoc900 = 0
maxLike900 = -Inf
for (i in seq(1,length(grid),1)){
  like = flike(data[900,],choices,crra,grid[i],20)
  if(like > maxLike){
    maxLike900 = like
    maxLoc900 =i
  }
}
theta900 = grid[maxLoc900]
theta900

#for person 115
maxLoc115 = 0
maxLike115 = -Inf
for (i in seq(1,length(grid),1)){
  like = flike(data[115,],choices,crra,grid[i],20)
  if(like > maxLike){
    maxLike115 = like
    maxLoc115 =i
  }
}
theta115 = grid[maxLoc115]
theta115
