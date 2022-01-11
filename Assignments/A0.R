#A0: getting familar with R
#============Exercise 1: Introduction =========

#install packages
install.packages(c("Hmisc","gdata","boot","xtable","MASS","moments","snow","mvtnorm"))
#set the working directory
setwd("/Users/liyueru/Desktop/贰零贰贰/Econ 613/Assignments")

#content of directory 
dir()
#content of environment 
ls()

#Check if 9 divide 678
678%%9

#save enviroment
save.image(file='a0.R_env.RData')

#find help on mean, cut2
?mean
??cut2
library("Hmisc")
?cut2

#============Exercise 2: Object Manipulation =========
dimnames(Titanic)

#1
Titanic
# Total Population
sum(Titanic)
# Total Adults
sum(Titanic[,,'Adult',])
#Total Crew
sum(Titanic['Crew',,,])
#3rd class children
sum(Titanic["3rd",,"Child",])
#2nd class adult female
sum(Titanic["2nd","Female","Adult",])
#1st class children male
sum(Titanic["1st","Male","Child",])
#Female Crew Survivor
sum(Titanic["Crew","Female",,"Yes"])
#1st class male survivor
sum(Titanic["1st","Male",,"Yes"])

#2.Using the function prop.table
?prop.table
#Proportion of survivors among first class, male, adult
prop.table(Titanic["1st","Male","Adult",])["Yes"]
#Proportion of survivors among first class, female adult
prop.table(Titanic["1st","Female","Adult",])["Yes"]
#Proportion of survivors among first class, male, children
prop.table(Titanic["1st","Male","Child",])["Yes"]
#Proportion of survivors among third class, female adult
prop.table(Titanic["3rd","Female","Adult",])["Yes"]

#============Exercise 3: Vectors - Introduction =========
#1.create vectors in 3 ways
?rev
a1 = seq(1,50,1)
a2 = 1:50
a3 = rev(50:1)

b1 = seq(50,1,-1)
b2 = 50:1
b3 = rev(1:50)

#2create the vectors
a = rep(c(10,19,7),15)
b = rep(c(1,2,5,6),8)

#3create a vector of function values 
x = seq(3.1,6,0.1)
vec = log(x)*sin(x)

#u4sing the function sample, draw 90 values between (0,100) and calculate the mean.
#Re-do the same operation allowing for replacement
val = c(0:100)
?sample
#90 values without replacement
mean1 = mean(sample(val,90,replace=FALSE))
#90 values with replacement 
mean2 = mean(sample(val,90,replace=TRUE))


#5Calculate the sums
#part a
a = seq(1,20,1)
b = t(seq(1,15,1))

sum(exp(sqrt(a))*log(a^5)/(5+cos(a)%*%sin(b)))
#part b
a = seq(1,20,1)
#val_temp = 0
 
a = seq(1,20,1)
b = t(seq(1,20,1))
m = exp(sqrt(a))*log(a^5)/(5+exp(a%*%b)*cos(a)%*%sin(b))
sum(m[!upper.tri(m)])

#6vector of values
x = seq(3,6,0.1)
vec6 = exp(x)*cos(x)
#============Exercise 4: Vectors - Advanced =========
#1
xVec = sample(0:999,1000,replace = TRUE)
yVec = sample(0:999,1000,replace = TRUE)
#2
#a
zVec = yVec[-1]- xVec[-1000]
#b
wVec = sin(yVec[-1000])/cos(xVec[-1])
#c
subX = xVec[xVec>=200]
#d
y_pos = which(yVec>=600)
#============Exercise 5: Matrix =========
#1
A = rbind(c(1,1,3),c(5,2,6),c(-2,-1,-3))
#a
A%*%A%*%A
#b
A = cbind (A,A[,1]+A[,3])
#c
A[,3] = A[,2]+A[,1]
#d
rowMeans(A)
colMeans(A)
#2
B = matrix(c(2,1,3,1,1,1,1,3,2),ncol=3,nrow=3,byrow= TRUE)
C = c(10,6,13)
solve(B,C)

#============Exercise 6: Matrix =========
#1
fun1 = function(a,n){
  n_seq = seq(1,n,1)
  a_seq = a^n_seq
  return(sum(a_seq/n_seq))
}
#2
func2 = function(x){
  value = 0
  if (x<0){
    value = x^2+2*x+abs(x)
  }else if(x >= 0 && x<2){
    value = x^2+3+log(1+x)
  }else {
    value = x^2 + 4*x-14
  }
  return (value)
}

#============Exercise 7: Indexes =========
#1
v1 = sample(1:20,36,replace=TRUE)
#2
#method 1
subVec = v1[-1]
#method 2
subVec = v1[2:length(v1)]
#3
v2 = v1>5
v2 = as.integer(v2)
#4
m1 = matrix(v1,nrow=6,ncol=6,byrow = TRUE)
#5
x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
#6
x[!(is.na(x)+is.infinite(x))]

#============Exercise 8: Data Manipulation =========
#1
install.packages("AER")
library(AER)
data("GSOEP9402", package = "AER")
dat = GSOEP9402
#2
typeof(dat)
#"list"
class(dat)
#"data.frame"
dim(dat)
#675 rows, 12 columns
names(dat)
#3
require(dplyr)
require(ggplot2)

ggplot(dat %>% group_by(year) %>% summarize(mean_income = mean(income)), aes(x=year,y=mean_income))+ geom_line() + ylab("Average Income")
#4
gender = dat %>% group_by(gender) %>% summarize(mean_income=mean(income))
school = dat %>% group_by(school) %>% summarize(mean_income=mean(income))
employment = dat %>% group_by(memployment) %>% summarize(mean_income=mean(income))
income_diff = c("Male-Female" = gender$mean_income[1]-gender$mean_income[2],"H_School-R_School"=school$mean_income[1]-school$mean_income[2], "H_school-G_School"=school$mean_income[1]-school$mean_income[3],"Fulltime-None"= employment$mean_income[1]-employment$mean_income[3],"Partime-None"=employment$mean_income[2]-employment$mean_income[3])

#============Exercise 9: First Regression =========
#1
data("CASchools",package = "AER")
dat1 = CASchools
#2
str(dat1)
dat1$district = factor(dat1$district)
dat1$school = factor(dat1$school)
reg1 = lm(read ~.-math,dat1)
summary(reg1)

#3
formua = y~x.lm(formula)
reg2 = lm(read ~.-math,dat1[1:200,])

#============Exercise 10: Advanced Indexing =========
#1
require(actuar)
??rpareto
lu = rpareto(200,1,1)  
length(lu[lu>10])
lu[lu>10] = rlogis(length(lu[lu>10]),6.5,0.5)
#2
install.packages("truncnorm")
library(truncnorm)
de = rnorm(200,1,2)
de = log(de)
de[is.na(de)] = rtruncnorm(length(de[is.na(de)]),a=0,mean=0,sd=1)
de[de<0] = rtruncnorm(length(de[de<0]),a=0,mean=0,sd=1)

#3
orig = runif(200,0,1)
dest = runif(200,0,1)
#4
hist = matrix(runif(200*200,0,1),nrow=200,ncol=200)
dist = matrix(runif(200*200,0,1),nrow=200,ncol=200)
#5
#6
temp_d = outer(orig,dest,"+")+dist
su = log(temp_d)/(1+log(temp_d))

temp_h = outer(orig,dest,"+")+hist
se = exp(temp_h)/(1+exp(temp_h))

#7
r = 0.05

getQ = function(w) { 
frac = outer(r+de, r+de, "/")
one = frac * w
two = lu * log(w)
three = lu * (1+log(w))
mid = outer(two,three,"-")
su_four = frac*(rowSums(su)-diag(su))
su_five = outer(rep(1,200),rowSums(su)-diag(su),"*")
su_part = su_four - su_five
se_four = frac*(rowSums(se)-diag(se))
se_five = outer(rep(1,200),rowSums(se)-diag(se),"*")
se_part = se_four - se_five
return(one+mid+su_part+se_part)
}
q1 = getQ(9245)


#8
gridw = seq(9100,55240,50)
#9
system.time(lapply(gridw, FUN=getQ))
#============Exercise 11: Tests and indexing =========
#1
is.array(c(1,2,3))
is.vector(c(1,2,3))
is.matrix(c(1,2,3))
#2
x0  =rnorm(1000)
?table
table(x0>0)[[2]]
table(x0>1)[[2]]
table(x0>2)[[2]]
table(x0>0.5)[[2]]
table(x0<1)[[2]]
table(x0>-1)[[2]]

#3
x1 = cut2(runif(100,0,1),g=10)
levels(x1) = paste("q",1:10,sep="")
x1


#4
is.factor(x1)

#5
sum(as.integer(x1[x1=="q1"]))==10
table(x1=="q1")[[2]] == 10 

#6
as.numeric(x1)
#The levels turn into their corresponding orders

#7
rand = rnorm(1000)

#8
posIndices = which(rand>0)

#9
#a: which
w1 = rand[which(rand>0)]
#b: Subset
w2 = subset(rand,rand>0)
#c: By indexing directly
w3 = rand[rand>0]

#============Exercise 12: Programming =========
#function U
U = function(n){
  if (n < 0){
    return (0)
  }else if(n==0){
    return (1)
  }else if (n==1){
    return (1)
  }else{
    return (U(n-1)+U(n-2))
  }
}
v = function() {
  N = readline(prompt = "Enter a non-negative integer: ")
  N = as.integer(N)
  return (U(N))
}
v()
#1
sum(seq(1,400,1)^2)

#2
sum(seq(1,249,1)*seq(2,250,1))

#3
crra = function(c,theta){
  if (theta<= 1.03 && theta >=0.97) {
    return (log(c))
  }
  return (c^(1-theta)/(1-theta))
  
}
#4
fact = function(n){
  if (n==1 | n==0){
    return (1)
  }
  return (prod(c(1:n)))
}

#============Exercise 13: Apply Functions =========
#
m = matrix(c(rnorm(20,0,10),rnorm(20,-1,10)),nrow=20,ncol=2)
apply(m, MARGIN=1,FUN = mean)
apply(m, MARGIN=1,FUN = median)
apply(m, MARGIN=1,FUN = min)
apply(m, MARGIN=1,FUN = max)
apply(m, MARGIN=1,FUN = sd)
apply(m, MARGIN=2,FUN = mean)
apply(m, MARGIN=2,FUN = median)
apply(m, MARGIN=2,FUN = min)
apply(m, MARGIN=2,FUN = max)
apply(m, MARGIN=2,FUN = sd)

#2
library(datasets)
data("iris",package = "datasets")
names(iris)
library(dplyr)
iris %>% group_by(Species) %>% summarize(Avg.Log.Sepal.Length=mean(Sepal.Length))
tibble = iris %>% group_by(Species) %>% summarise(sum_log_sepal_width = sum(log(Sepal.Width)))
tibble$sum_log_sepal_width
#3
y1 = NULL
for (i in 1:100){
  y1[i]=exp(i)
}
y2=exp(1:100)
y3=sapply(1:100,exp)

#a
sum(abs(y1-y2))==0
sum(abs(y3-y2))==0
#b
test = function(n){
  if (n==1){
    ptm = proc.time()
    y1 = NULL
    for (i in 1:100){
      y1[i]=exp(i)
    }
    proc.time()-ptm
  }else if (n==2){
    ptm = proc.time()
    y2=exp(1:100)
    proc.time()-ptm
  }else {
    ptm = proc.time()
    y3=sapply(1:100,exp)
    proc.time()-ptm
  }
}
y1 = NULL
system.time(for (i in 1:100){
  y1[i]=exp(i)
})
system.time(exp(1:100))
system.time(sapply(1:100,exp))
#============Exercise 14: Apply Functions =========
#1
x = rnorm(10000)
summary(x)
#2
dsummary = function(x){
  return (c("Min"=min(x),quantile(x,0.1), quantile(x,0.25),
            "Median" = median(x), "Mean" = mean(x), "Standard Deviation" = sd(x),
            quantile(x,0.75),  quantile(x,0.9), "Max" = max(x)))
  
}
#3
dnorm(0.5,mean=2,sd=0.25)
pnorm(2.5,mean=2,sd=0.25)
qnorm(0.95,mean=2,sd=0.25)


#4
dt(0.5,df=5)
pt(2.5,df=5)
qt(0.95,df=5)

#5
library(Pareto)
dPareto(0.5,1,3)
pPareto(2.5,1,3)
qPareto(0.95,1,3)
library(actuar)
dpareto(0.5,3,1)
ppareto(2.5,3,1)
qpareto(0.95,3,1)

#============Exercise 15: Moments =========
v = rnorm(100,-2,5)
#1
n = length(v)
#2
m = mean(v)
#3
variance = var(v)
#4
library(moments)
gamma = skewness(v)
#5
k = kurtosis(v)


#============Exercise 16: OLS =========
#1
X = matrix(rbeta(1000*10,2,1),nrow = 1000,ncol=10)
length(X[X<0])==0
#2
sigmaSquared = 0.5
beta = rgamma(10,2,1)

#3
epsilon = rnorm(1000)

#4 
Y = X%*%beta+sqrt(sigmaSquared)*epsilon
#5
betaHat = solve(t(X)%*%X)%*%t(X)%*%Y

#6
yHat = X%*%betaHat
epsilonHat = yHat-Y
hist(epsilonHat, col="gray")
plot(density(epsilonHat))

#7
sigmaSqHat = t(epsilonHat)%*%epsilonHat / (length(epsilon)-length(beta)-1)
dim(sigmaSqHat)
varBetaHat = sigmaSqHat[1,1]*solve(t(X)%*%X)
varBetaHat

#8
param = cbind(betaHat,sqrt(varBetaHat))
model1 = lm(Y~0+X)
summary(model1)
param
#9
confint(model1)

#10
sigmaSquared2 = 0.01
Y2 = X%*%beta+sqrt(sigmaSquared2)*epsilon
model2 = lm(Y2~0+X)
#9
confint(model2)
