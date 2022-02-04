setwd("~/Desktop/Econometrics/Econ 613/Assignments/A2")
#=======Exercise1 OLS Estimates ===========
data1 = read.csv("./data/datind2009.csv",colClasses=c("idmen"="character"), header=TRUE)
#Correlation between Y and X
#We drop the entries where either Y or X is NA
data1.1 = data1[!is.na(data1$wage) & !is.na(data1$age),]
data1.1 = data1.1[data1.1$wage>0,]
y = data1.1$wage
x = data1.1$age
cov(y,x)/sqrt(var(x)*var(y))
xSquared = x^2
ybar = mean(y)
xbar = mean(x)
cov = sum((y-ybar)*(x-xbar))
xvar = sum((x-xbar)^2)
yvar = sum((y-ybar)^2)
xycor = cov/sqrt(xvar*yvar)
xycor#0.143492
#Calculate the coefficients on this regression
z = rep(1,length(x))
x = rbind(z,x)
beta = solve(x%*%t(x))%*%x%*%y
beta#14141.1794 230.9923

# Calculate the standard errors 
#Check for Homoscedasticity
library(dplyr)
ageDist = as.data.frame(data1.1 %>% group_by(age) %>% summarise(num = n()))
ageDist
yFit = t(beta)%*%x
E = y-t(beta)%*%x
Estandardized = E[1,]/(sqrt(var(E[1,])))
plot(yFit,Estandardized,xlab="Fitted Wage Vales",ylab="Standardize Errors")
#Homoscedasticity is violated, Let's calculate the Heteroscedasticity standard error
Omega = matrix(rep(0,length(y)*length(y)),ncol=length(y),nrow=length(y))
for (i in seq(1,length(y),1)){
  x_condition = data1.1$age[i]
  y_temp = data1.1[data1.1$age==x_condition,]$wage
  y_temp_bar = mean(y_temp)
  Omega[i,i] = sum((y_temp-y_temp_bar)^2)/length(y_temp-1)
}
#Using OLS formula
betaVar = solve(x%*% t(x))%*%x%*%Omega%*%t(x)%*%solve(x %*% t(x))
beta0StdError = sqrt(betaVar[1,1])
beta1StdError = sqrt(betaVar[2,2])
betaStdError = cbind(sqrt(diag(betaVar)))
betaStdError#526.69029 14.20466

#Bootstrap Methods
set.seed(123)
#49 Bootstrap
R    = 49
npoints= nrow(data1.1)
results = mat.or.vec(R,2)
for (i in 1:R)
{
  temp = sample(1:npoints,npoints,rep=TRUE)
  data1.1_temp = data1.1[temp,]
  x_temp = data1.1_temp$age
  y_temp = data1.1_temp$wage
  x_temp = rbind(rep(1,length(x_temp)),x_temp)
  data1.1_temp = t(solve(x_temp%*%t(x_temp))%*%x_temp%*%y_temp)
  results[i,] = data1.1_temp
}
avg_estimate49 = apply(results,2,mean)
sd_estimate49 = apply(results,2,sd)
avg_estimate49#14132.6026   231.2057
sd_estimate49#542.45271 14.73729
#499 Bootstrap
R    = 499
npoints= nrow(data1.1)
results = mat.or.vec(R,2)
for (i in 1:R)
{
  temp = sample(1:npoints,npoints,rep=TRUE)
  data1.1_temp = data1.1[temp,]
  x_temp = data1.1_temp$age
  y_temp = data1.1_temp$wage
  x_temp = rbind(rep(1,length(x_temp)),x_temp)
  data1.1_temp = t(solve(x_temp%*%t(x_temp))%*%x_temp%*%y_temp)
  results[i,] = data1.1_temp
}
avg_estimate499 = apply(results,2,mean)
sd_estimate499 = apply(results,2,sd)
avg_estimate499#14138.2520   231.4511
sd_estimate499#571.53772  15.15403
#The two methods are very different philosophically, the bootstrap method
#is non-parametric, which can be continent when it is difficult to 
#formulate a closed form solution of variance of our estimator or if 
#the closed form solution is computationally inefficient. But as we can see
#the closed form solution does give us a tighter variance estimate.

#=======Exercise2 Detrend Data ===========
#Read in the data
setwd("~/Desktop/Econometrics/Econ 613/Assignments/A2")
individualFiles = list.files(path = "./Data",
                             pattern="dati", 
                             full.names = T) 
data2 = read.csv(individualFiles[1],colClasses=c("idind"="character","idmen"="character"), header=TRUE)
numFile = length(individualFiles)
for (i in seq(2,numFile,1)){
  a = read.csv(individualFiles[i],colClasses=c("idind"="character","idmen"="character"), header=TRUE)
  if(typeof(a$profession)=="integer"){
    a$profession = as.character(a$profession)
  }
  data2 = rbind(data2, a)
}
data2 = subset(data2, select = -c(X))
#plot the wage of each age group across years
#Ignore those with age under 18
data2.1 = data2[!is.na(data2$age) & data2$age>=18,]
data2.1 = data2.1[!is.na(data2.1$wage),]
#crate categorical variables
data2.2 = data2.1
data2.2$ageGroup = "value"
data2.2[data2.2$age<=25 & data2.2$age>=18,]$ageGroup = "18-25"
data2.2[data2.2$age<=30 & data2.2$age>=26,]$ageGroup = "26-30"
data2.2[data2.2$age<=35 & data2.2$age>=31,]$ageGroup = "31-35"
data2.2[data2.2$age<=40 & data2.2$age>=36,]$ageGroup = "36-40"
data2.2[data2.2$age<=45 & data2.2$age>=41,]$ageGroup = "41-45"
data2.2[data2.2$age<=50 & data2.2$age>=46,]$ageGroup = "46-50"
data2.2[data2.2$age<=55 & data2.2$age>=51,]$ageGroup = "51-55"
data2.2[data2.2$age<=60 & data2.2$age>=56,]$ageGroup = "56-60"
data2.2[data2.2$age>=60,]$ageGroup = "60+"
#Plot the wage of each age group across years. Is there a trend?
quantile(data2.2$wage, probs = seq(0, .99, by = .03))
#We see that USD 80000 should cover the 99 percentile of the income distribution
#To prevent the distribution from being squeezed by the outliers, we plot the values below
#80000 USD
library(ggplot2)
bp1 <- ggplot(data2.2, aes(x=ageGroup, y=wage, group=ageGroup)) + 
  geom_boxplot(aes(fill=ageGroup))+facet_grid(year ~ .)+ylim(0, 80000)
bp1

#We see that the distribution of wage for those over 60 seems to be much lower,
#Let's check the data for them
data2.2Age60Plus = data2.2[data2.2$age>60,]
quantile(data2.2Age60Plus$wage, probs = seq(0, .99, by = .03))
#Notice that the income is 0 at 84 percentile. So we give up looking at this group for 
#distribution as most people have retired already.

#Is there a trend?
#It appears that the the 25 to 75 percentile is doing better economically, not sure if this 
#is just inflation or real wage growth
#Consider Y_{it} = \beta X_{it} +\gamma_t +e_{it}


#Year fixed effect with dummies
library(fastDummies)
data2.3 = dummy_cols(data2.2,select_columns = "year")
nrow(data2.3)
y2 = data2.3$wage
x2 = rbind(rep(1,nrow(data2.3)),data2.3$age)#the constant intercept term, linear term and quadratic term
for (i in seq(2006,2018,1)){#Addin dummies for each year
  yearNum = paste("year_",i,sep="")
  x2 = rbind(x2,t(data2.3[yearNum]))
}

beta2 = solve(x2%*%t(x2))%*%x2%*%y2
beta2[1:2]#23701.2115  -239.4312
#Looking at the comparison, we see that we've 
#How do the estimated coefficients change? 
#There is a big change in the intercept where the intercept becomes 
#more than 1000 bigger, but the changes to the coefficient on
#age and age^2 seems small.
#=======Exercise3 Numerical Optimization ===========
setwd("~/Desktop/Econometrics/Econ 613/Assignments/A2")
data3 = read.csv("./data/datind2007.csv",colClasses=c("idmen"="character"), header=TRUE)
#exclude all individuals who are inactive
data3.1 = data3[!is.na(data3$empstat) &(!data3$empstat=="Inactive" & !data3$empstat=="Retired"),]
#Drop those aged below 18
data3.1 = data3.1[!is.na(data3.1$age) & data3.1$age>=18,]
#Write a function that returns the likelihood of the probit of being unemployed
data3.1$Participation = 0
data3.1[data3.1$empstat=="Employed",]$Participation = 1
likelihood = function(param,xvar1,yvar)
{
  betaX = param[1]+param[2]*xvar1
  prob = pnorm(betaX)
  prob[prob>0.999999] = 0.999999
  prob[prob<0.000001] = 0.000001
  likelihoodVal = yvar*log(prob)+(1-yvar)*log(1-prob)
  return (-sum(likelihoodVal))
}
#Optimize the model and interpret the coefficients. 
#You can use pre-programmed optimization pack- ages.

set.seed(123)
numTry = 10000
result = mat.or.vec(numTry,2)
minLoc = 0
minLike = Inf
Participate = data3.1$Participation
age = data3.1$age
for (i in 1:numTry){
  randomStart = runif(2,-1.5,1.5)
  outcome = optim(par = randomStart,fn=likelihood,method="BFGS",control=list(trace=6,maxit=1000),xvar1=age,yvar=Participate)
  if(outcome$value <  minLike){
    minLike= outcome$value
    minLoc = i
  }
  result[i,] = outcome$par
}
parameter = result[minLoc,]
parameter 
#1.058080250 0.006618533
#Notice that the coefficient for age is positive. So older people are more likely
#to be employed.
outcome = optim(par = parameter,fn=likelihood,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),xvar1=age,yvar=Participate,hessian=TRUE)
fisherInfoMatrix = solve(outcome$hessian)
paramStd = sqrt(diag(fisherInfoMatrix))

paramStd#0.059314668 0.001421221

parameter/paramStd#17.838425  4.656935
#No, because only those making wages are employed.

#=======Exercise4 Discrete Choice ===========
setwd("~/Desktop/Econometrics/Econ 613/Assignments/A2")
data4 = subset(data2.3[!data2.3$year %in% c(2016,2017,2018),], select = -c(year_2016,year_2017,year_2018))
#Exclude all individuals who are inactive.
data4.1 = data4[!is.na(data4$empstat) &(!data4$empstat=="Inactive" & !data4$empstat=="Retired"),]
data4.1$Participation = 0
data4.1[data4.1$empstat=="Employed",]$Participation = 1
#Write and optimize the probit, logit, and the linear probability models.
#Probit
like_Probit = function(param,xvar,year06,year07,year08,year09,year10,year11,year12,year13,year14,year15,yvar)
{
  betaX = param[1]+param[2]*xvar+param[3]*year06+param[4]*year07+param[5]*year08+param[6]*year09+param[7]*year10+param[8]*year11+param[9]*year12+param[10]*year13+param[11]*year14+param[12]*year15
  prob = pnorm(betaX)
  prob[prob>0.999999] = 0.999999
  prob[prob<0.000001] = 0.000001
  likelihoodVal = yvar*log(prob)+(1-yvar)*log(1-prob)
  return (-sum(likelihoodVal))
}
set.seed(123)
numTry = 200
resultProbit = mat.or.vec(1,12)
minLocProbit = 0
minLikeProbit = Inf
for (i in 1:numTry){
  randomStart = runif(12,-1,1)
  outcome = optim(par = randomStart,fn=like_Probit,method="BFGS",control=list(trace=6,REPORT=1000,maxit=1000),xvar=data4.1$age,
                  year06=data4.1$year_2006,year07=data4.1$year_2007,year08=data4.1$year_2008,
                  year09=data4.1$year_2009,year10=data4.1$year_2010,year11=data4.1$year_2011,
                  year12=data4.1$year_2012,year13=data4.1$year_2013,year14=data4.1$year_2014,
                  year15=data4.1$year_2015,yvar=data4.1$Participation)
  if(outcome$value <  minLikeProbit){
    minLikeProbit = outcome$value
    minLocProbit = i
    resultProbit = outcome$par
  }
}
parameterProbit = resultProbit
parameterProbit #0.761252928  0.012116778  0.013542292  0.077197421  0.107165660  0.024328589  0.019088048  0.050856170  0.008309555 -0.041108950 -0.035825651 -0.057490376
#Statistical Significance
outcomeProbit = optim(par = parameterProbit,fn=like_Probit,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),xvar=data4.1$age,
                     year06=data4.1$year_2006,year07=data4.1$year_2007,year08=data4.1$year_2008,
                     year09=data4.1$year_2009,year10=data4.1$year_2010,year11=data4.1$year_2011,
                     year12=data4.1$year_2012,year13=data4.1$year_2013,year14=data4.1$year_2014,
                     year15=data4.1$year_2015,yvar=data4.1$Participation,hessian=TRUE)
fisherInfoProbit = solve(outcomeProbit$hessian)
paramStdProbit = sqrt(diag(fisherInfoProbit))
paramStdProbit #0.0230228649 0.0004103355 0.0229353185 0.0230883298 0.0233138347 0.0228458115 0.0226444201 0.0226984932 0.0221839029 0.0224311179 0.0224013667 0.0223840702
parameterProbit/paramStdProbit #33.0650825 29.5289511  0.5904558  3.3435689  4.5966552  1.0649037  0.8429471  2.2405086  0.3745759 -1.8326750 -1.5992618 -2.5683611


#Logit
like_Logit = function(param,xvar,year06,year07,year08,year09,year10,year11,year12,year13,year14,year15,yvar)
{
  betaX = param[1]+param[2]*xvar+param[3]*year06+param[4]*year07+param[5]*year08+param[6]*year09+param[7]*year10+param[8]*year11+param[9]*year12+param[10]*year13+param[11]*year14+param[12]*year15
  like_temp = 1/(1+exp(-1.0*betaX))
  like_temp[like_temp>0.999999] = 0.999999
  like_temp[like_temp<0.000001] = 0.000001
  likelihoodVal = yvar*log(like_temp)+(1-yvar)*log(1-like_temp)
  return (-sum(likelihoodVal))
}
numTry = 100
resultLogit = mat.or.vec(1,12)
minLocLogit = 0
minLikeLogit = Inf
for (i in 1:numTry){
  randomStart = runif(12,-10,10)
  outcome = optim(par = randomStart,fn=like_Logit,method="BFGS",control=list(trace=6,maxit=1000),xvar=data4.1$age,
                  year06=data4.1$year_2006,year07=data4.1$year_2007,year08=data4.1$year_2008,
                  year09=data4.1$year_2009,year10=data4.1$year_2010,year11=data4.1$year_2011,
                  year12=data4.1$year_2012,year13=data4.1$year_2013,year14=data4.1$year_2014,
                  year15=data4.1$year_2015,yvar=data4.1$Participation)
  if(outcome$value <  minLikeLogit){
    minLikeLogit = outcome$value
    minLocLogit = i
    resultLogit = outcome$par
  }
  
}
parameterLogit = resultLogit
parameterLogit# 1.139664486  0.024981732  0.026444460  0.152017213  0.207911578  0.042850540  0.033770854  0.093543001  0.008497174 -0.086613202 -0.075714845 -0.118957927
#Statistical Significance
outcomeLogit = optim(par = parameterLogit,fn=like_Logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),xvar=data4.1$age,
                year06=data4.1$year_2006,year07=data4.1$year_2007,year08=data4.1$year_2008,
                year09=data4.1$year_2009,year10=data4.1$year_2010,year11=data4.1$year_2011,
                year12=data4.1$year_2012,year13=data4.1$year_2013,year14=data4.1$year_2014,
                year15=data4.1$year_2015,yvar=data4.1$Participation,hessian=TRUE)
fisherInfoLogit = solve(outcomeLogit$hessian)
paramStdLogit = sqrt(diag(fisherInfoLogit))
paramStdLogit #0.0445950499 0.0008214027 0.0443799800 0.0450834280 0.0456800991 0.0442201711 0.0438327789 0.0441154559 0.0428712826 0.0431031370 0.0431117895 0.0429714570
parameterLogit/paramStdLogit #25.5558518 30.4135026  0.5958646  3.3719089  4.5514695  0.9690270  0.7704475  2.1204133  0.1982020 -2.0094408 -1.7562446 -2.7683010
#Linear Probability
y4 = data4.1$Participation
x4 = rbind(rep(1,nrow(data4.1)),data4.1$age)
for (i in seq(2006,2015,1)){
  yearNum = paste("year_",i,sep="")
  x4 = rbind(x4,t(data4.1[yearNum]))
}

beta4 = solve(x4%*%t(x4))%*%x4%*%y4
beta4#0.8003158851, 0.0022902495
# 0.0024085951  0.0134025350  0.0178844471  0.0037778745  0.0029272740  0.0081725969  0.0005488462 -0.0084409484 -0.0074079902 -0.0116190676
#Statistical Significance
#When I try to calculate the robust standard errors
#Because of the size of the data, I cannot create a 
#Omega matrix of the appropriate size
#It prompts me with Error vector memory exhausted (limit reached?)
#So I calculate the usual non-robust standard errors
E4 = y4-t(beta4)%*%x4
E4bar = mean(E4) 
E4var = sum((E4-E4bar)^2)/length(E4-1)
beta4StdError = diag(solve(x4 %*% t(x4))*E4var)
beta4StdError #1.785461e-05, 5.596892e-09
#1.679569e-05 1.649295e-05 1.655500e-05 1.656372e-05 1.629498e-05 1.610506e-05 1.568787e-05 1.638094e-05 1.626418e-05 1.637323e-0
#=======Exercise5 Marginal Effects ===========
#Compute the marginal effect of the previous probit and logit models.
#For this part, I will calculate the marginal effect
#Evaluated at the mean on the baseline year, year 2005
data5 = data4.1[data4.1$year_2005==1,]
#Probit Model


Q5ProbitParam = parameterProbit[1:2]
xbar = mean(data5$age)
MEProbit = dnorm(Q5ProbitParam[1]+Q5ProbitParam[2]*xbar)*Q5ProbitParam[2]
MEProbit #0.002197137
#Logit Model
Q5LogitParam = parameterLogit[1:2]
epow = exp(-Q5LogitParam[1]-Q5LogitParam[2]*xbar)
MELogit = Q5LogitParam[2]*epow/((1+epow)^2)
MELogit #0.00231738


#Construct the standard errors of the marginal effects
getCoef = function(likefn,data){
  numTry = 100
  result = mat.or.vec(1,12)
  minLoc = 0
  minLike = Inf
  for (i in 1:numTry){
    randomStart = runif(12,-2,2)
    outcome = optim(par = randomStart,fn=likefn,method="BFGS",control=list(trace=6,REPORT=1000,maxit=1000),xvar=data$age,
                    year06=data$year_2006,year07=data$year_2007,year08=data$year_2008,
                    year09=data$year_2009,year10=data$year_2010,year11=data$year_2011,
                    year12=data$year_2012,year13=data$year_2013,year14=data$year_2014,
                    year15=data$year_2015,yvar=data$Participation)
    if(outcome$value <  minLike){
      minLike = outcome$value
      minLoc = i
      result = outcome$par
    }
  }
  return (result)
}
#Probit
#49 Bootstrap
R    = 49
npoints= nrow(data4.1)
resultsProbit = mat.or.vec(R,1)
for (i in 1:R)
{
  print(i)
  print("===========================================================")
  temp = sample(1:npoints,npoints,rep=TRUE)
  data4.1_temp = data4.1[temp,]
  coef = getCoef(like_Probit,data4.1_temp)
  data4.1_temp_2005 = data4.1_temp[data4.1_temp$year==2005,]
  x_temp_bar = mean(data4.1_temp_2005$age)
  resultsProbit[i] = dnorm(coef[1]+coef[2]*x_temp_bar)*coef[2]
  print("===========================================================")
  print(resultsProbit[i])
}
avg_estimateProbit = mean(resultsProbit)
sd_estimateProbit = sd(resultsProbit)
avg_estimateProbit #0.002197566
sd_estimateProbit #0.0001002162
#Logit
#49 Bootstrap
R    = 49
npoints= nrow(data4.1)
resultsLogit = mat.or.vec(R,1)
for (i in 1:R)
{
  print(i)
  print("===========================================================")
  temp = sample(1:npoints,npoints,rep=TRUE)
  data4.1_temp = data4.1[temp,]
  coef = getCoef(like_Logit,data4.1_temp)
  data4.1_temp_2005 = data4.1_temp[data4.1_temp$year==2005,]
  x_temp_bar = mean(data4.1_temp_2005$age)
  epow = exp(-coef[1]-coef[2]*x_temp_bar)
  resultsLogit[i] = coef[2]*epow/((1+epow)^2)
  print("===========================================================")
  print(resultsProbit[i])
}
avg_estimateLogit = mean(resultsLogit)
sd_estimateLogit = sd(resultsLogit)
avg_estimateLogit#0.002312585
sd_estimateLogit#9.587769e-05
