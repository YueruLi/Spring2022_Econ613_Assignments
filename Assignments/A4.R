setwd("~/Desktop/Econometrics/Econ 613/Assignments/A4")
#=====Exercise1: Preparing the Data=============
df = read.csv("./Data/dat_A4.csv")
colnames(df)
df = subset(df,select = -c(X.1,X))
#Construct Age variable
#I construct their age in months and in yrs
ageYr = 2022-df$KEY_BDATE_Y_1997
ageMonth = 12*ageYr+4-df$KEY_BDATE_M_1997
#as.integer drops the decimals, so I add 0.5 
df["ageY"] = as.integer(ageMonth/12 + 0.5)
df["ageM"] = ageMonth
df = subset(df,select = -c(KEY_BDATE_Y_1997,KEY_BDATE_M_1997))
#Construct total work experience measured in years
wk_exp_wk = df$CV_WKSWK_JOB_DLI.01_2019
wk_exp_wk[is.na(wk_exp_wk)] = 0
df = subset(df,select = -c(CV_WKSWK_JOB_DLI.01_2019))
for (i in 2:09){
  varName_1 = paste("CV_WKSWK_JOB_DLI.0",i,sep="")
  varName = paste(varName_1,"_2019",sep="")
  wk_exp_temp = df[varName]
  wk_exp_temp[is.na(wk_exp_temp)] = 0
  wk_exp_wk = wk_exp_wk + wk_exp_temp
}
wk_exp_10 = df$CV_WKSWK_JOB_DLI.10_2019
wk_exp_10[is.na(wk_exp_10)] = 0
wk_exp_11 = df$CV_WKSWK_JOB_DLI.11_2019
wk_exp_11[is.na(wk_exp_11)] = 0
wk_exp_wk = wk_exp_wk + wk_exp_10 + wk_exp_11
df["wk_exp_wk"] = wk_exp_wk
df = subset(df,select = -c(CV_WKSWK_JOB_DLI.02_2019,CV_WKSWK_JOB_DLI.03_2019,CV_WKSWK_JOB_DLI.04_2019,
                           CV_WKSWK_JOB_DLI.05_2019,CV_WKSWK_JOB_DLI.06_2019,CV_WKSWK_JOB_DLI.07_2019,
                           CV_WKSWK_JOB_DLI.08_2019,CV_WKSWK_JOB_DLI.09_2019,CV_WKSWK_JOB_DLI.10_2019,
                           CV_WKSWK_JOB_DLI.11_2019))
df["wk_exp_yr"] = df$wk_exp_wk / 52
#additional education variable indicating total years of school from all variables related to education

#Self-Schooling
#drop those with NA
df = df[!is.na(df$YSCH.3113_2019),]
schooling = df$YSCH.3113_2019
#convert  none education to 0
schooling[schooling==1]=0
#I treat GED as equivalent to high school, which is 12 years
schooling[schooling==2]=12
schooling[schooling==3]=12
#I treat Associate degree as 14 years of education, 2 years after high school
schooling[schooling==4]=14
#I treat Bachelor degree as 16 years of education, 4 years after high school
schooling[schooling==5]=16
#I treat Master degree as 18 years of education, 2 years after Bachelor
schooling[schooling==6]=18
#I treat PhD degree as 24 years of education, 6 years after Bachelor
schooling[schooling==7]=22
#I treat Professional degree as 22 years of education, 4 years after Bachelor, DDS and MD take 4 years, JD 3 years, I choose the 4 year mark
schooling[schooling==8]=20
df["School_yr"] = schooling
df = subset(df,select=-c(YSCH.3113_2019))
#drop NA for parents edu as well
df= df[!is.na(df$CV_HGC_BIO_DAD_1997),]
df= df[!is.na(df$CV_HGC_BIO_MOM_1997),]
df= df[!is.na(df$CV_HGC_RES_DAD_1997),]
df= df[!is.na(df$CV_HGC_RES_MOM_1997),]
#Biological Father Schooling
BioDadSch = df$CV_HGC_BIO_DAD_1997
BioDadSch[BioDadSch==95] = 0
df["BioDadSch"] = BioDadSch
df = subset(df,select = -c(CV_HGC_BIO_DAD_1997))
#Biological Mother Schooling
BioMomSch = df$CV_HGC_BIO_MOM_1997
BioMomSch[BioMomSch==95] = 0
df["BioMomSch"] = BioMomSch
df = subset(df,select = -c(CV_HGC_BIO_MOM_1997))
#Residential Father Schooling
ResDadSch = df$CV_HGC_RES_DAD_1997
ResDadSch[ResDadSch==95] = 0
df["ResDadSch"] = ResDadSch
df = subset(df,select = -c(CV_HGC_RES_DAD_1997))
#Residential Mother Schooling
ResMomSch = df$CV_HGC_RES_MOM_1997
ResMomSch[ResMomSch==95] = 0
df["ResMomSch"] = ResMomSch
df = subset(df,select = -c(CV_HGC_RES_MOM_1997))


#Provide the following visualization
#Age Group
library(ggplot2)
incomeDistributionAge <- ggplot(df[df$YINC_1700_2019>0,], aes(x=ageY, y=YINC_1700_2019, group=ageY)) + 
  geom_boxplot(aes(fill=ageY))+ xlab("Age") + ylab("Income")
incomeDistributionAge
#Gender Group
df["gender"] = "Male"
df[df$KEY_SEX_1997==2,]$gender = "Female"
df$gender = as.factor(df$gender)
incomeDistributionGender <- ggplot(df[df$YINC_1700_2019>0,], aes(x=gender, y=YINC_1700_2019, group=gender)) + 
  geom_boxplot(aes(fill=gender))+ xlab("gender") + ylab("Income")
incomeDistributionGender
df = subset(df,select = -c(KEY_SEX_1997))
#Number of Children Group
#use -1 to represent NA
df[is.na(df$CV_BIO_CHILD_HH_U18_2019),]$CV_BIO_CHILD_HH_U18_2019 = -1
df["NumChild"] = df$CV_BIO_CHILD_HH_U18_2019
incomeDistributionChildren <- ggplot(df[df$YINC_1700_2019>0,], aes(x=NumChild, y=YINC_1700_2019, group=NumChild)) + 
  geom_boxplot(aes(fill=NumChild))+ labs(title = "Distribution of Income by Number of Children\n", 
                                                         x = "Number of Children", y = "Yearly Income")+xlim(-2,12)
incomeDistributionChildren
df = subset(df,select = -c(CV_BIO_CHILD_HH_U18_2019))
#Table the share of "0" in the income data by 
df["ZeroIncome"] = "Yes"
df[!is.na(df$YINC_1700_2019) & df$YINC_1700_2019>0,]$ZeroIncome = "No"
df$ZeroIncome = as.factor(df$ZeroIncome)
df["Income"] = df$YINC_1700_2019
df[is.na(df$Income),]$Income = 0
df = subset(df,select = -c(YINC_1700_2019))
#age
ageTable = prop.table(table(df$ZeroIncome,df$ageY),margin=2)
ageTable
#gender
genderTable = prop.table(table(df$ZeroIncome,df$gender),margin=2)
genderTable
#NumChild
NumChildTable = prop.table(table(df$ZeroIncome,df$NumChild),margin=2)
NumChildTable
#Marital Status
#I turn NAs into -1
df[is.na(df$CV_MARSTAT_COLLAPSED_2019),]$CV_MARSTAT_COLLAPSED_2019 = -1
MaritalTable = prop.table(table(df$ZeroIncome,df$CV_MARSTAT_COLLAPSED_2019),margin=2)
MaritalTable

#Marital Status and Num of Children
prop.table(table(df$ZeroIncome[df$CV_MARSTAT_COLLAPSED_2019==-1],df$NumChild[df$CV_MARSTAT_COLLAPSED_2019==-1]),margin=2)
prop.table(table(df$ZeroIncome[df$CV_MARSTAT_COLLAPSED_2019==0],df$NumChild[df$CV_MARSTAT_COLLAPSED_2019==0]),margin=2)
prop.table(table(df$ZeroIncome[df$CV_MARSTAT_COLLAPSED_2019==1],df$NumChild[df$CV_MARSTAT_COLLAPSED_2019==1]),margin=2)
prop.table(table(df$ZeroIncome[df$CV_MARSTAT_COLLAPSED_2019==2],df$NumChild[df$CV_MARSTAT_COLLAPSED_2019==2]),margin=2)
prop.table(table(df$ZeroIncome[df$CV_MARSTAT_COLLAPSED_2019==3],df$NumChild[df$CV_MARSTAT_COLLAPSED_2019==3]),margin=2)
prop.table(table(df$ZeroIncome[df$CV_MARSTAT_COLLAPSED_2019==4],df$NumChild[df$CV_MARSTAT_COLLAPSED_2019==4]),margin=2)

df["marital"] = "married"
df$marital[!df$CV_MARSTAT_COLLAPSED_2019==1]="Not married"
df$marital = as.factor(df$marital)
#re-code Race
df["race"] = "Black"
df$race[df$KEY_RACE_ETHNICITY_1997==2] = "Hispanic"
df$race[df$KEY_RACE_ETHNICITY_1997==3] = "Mixed Race/Non-Hispanic"
df$race[df$KEY_RACE_ETHNICITY_1997==4] = "Non-Black/Non-Hispanic"
df = subset(df,select = -c(KEY_RACE_ETHNICITY_1997))
df$race = as.factor(df$race)
#Recode Gender to 0/1
gender = df$gender
df$gender = 1
df[gender=="Female",]$gender = 0
#Recode Marital to 0/1
marital = df$marital
df$marital = 0
df$marital[marital=="married"] = 1
df["SAT"] = df$TRANS_SAT_MATH_HSTR
df$SAT[is.na(df$SAT)] = 0
#=====Exercise2: Preparing the Data=============
dfEx2 = df[df$ZeroIncome=="No",]
Ex2_model1 = lm(Income~marital+ageY+gender+NumChild+wk_exp_wk+School_yr,data=dfEx2)
summary(Ex2_model1)
library(stargazer)
stargazer(Ex2_model1, type = "latex")
df["NonZeroIncome"] = "Yes"
df[df$ZeroIncome=="Yes",]$NonZeroIncome = "No"
df$NonZeroIncome = as.factor(df$NonZeroIncome)

#Heckman two step
probit <- glm(NonZeroIncome ~SAT +marital+ageY+gender+School_yr+ResDadSch+ResMomSch+BioDadSch+BioMomSch, family = binomial(link = "probit"), 
                data = df)
stargazer(probit, type = "latex")
InverseMillRatio = function(coeff,dat)
{
  argument = coeff[1]
  for (i in dim(dat)[2]){
    argument = argument + coeff[i+1]*dat[,1]
  }
  return (dnorm(argument)/pnorm(argument))
}
ivmRatio = InverseMillRatio(probit$coefficients,df[c("SAT","marital","ageY","gender","School_yr","ResDadSch","ResMomSch","BioDadSch","BioMomSch")])
unique(ivmRatio)
df["InverseMill"] = ivmRatio
dfEx2_2 = df[df$ZeroIncome=="No",]
Ex2_model2 = lm(Income~marital+ageY+gender+NumChild+wk_exp_wk+School_yr+InverseMill,data=dfEx2_2)
summary(Ex2_model2)
stargazer(Ex2_model2, type = "latex")

#=====Exercise3: Censoring=============
#We focus on the population with positive income in this problem so we don't deal with censoring on both ends
dfEx3 = df[df$ZeroIncome == "No",]
hist(dfEx3$Income,breaks=100)
max(dfEx3$Income)
dfEx3["censored"] = 0
dfEx3[dfEx3$Income==max(df$Income),]$censored = 1
colnames(dfEx3)

tobiLike = function(data,param,d){
  #data first column income
  paramLength = length(param)
  val = param[1] 
  for (i in 2:paramLength){
    val = val + param[i]*data[,i]
  }
  fVal = dnorm(data[,1]-val)
  FVal = 1-pnorm(data[,1]-val)
  
  fVal[fVal<0.000001] = 0.000001
  FVal[FVal>0.999999] = 0.999999
  FVal[FVal<0.000001] = 0.000001
  like = (1-d)*log(fVal)+(d*log(FVal))
  #return (FVal)
  return (-sum(like))
}
#turn work experience to year
dfTobit = dfEx3[c("Income","marital","gender","ageY","wk_exp_wk","School_yr")]
paramLength = dim(dfTobit)[2]
numTry = 1000
result = mat.or.vec(numTry,paramLength)
minLoc = 1
minLike = Inf
set.seed(123)
for(i in 1:numTry){
  outcome = optim(par = runif(paramLength,0.0001,0.0001),fn=tobiLike,method="BFGS",control=list(trace=6,maxit=1000),data=dfTobit,d=dfEx3["censored"])
  if(outcome$value < minLike){
    minLike = outcome$value
    minLoc = i
  }
  result[i,]=outcome$par
}
#For comparison purposes
modelEx3_comp = lm(Income~  marital+ageY+gender+NumChild+wk_exp_wk+School_yr , data = dfEx3)
summary(modelEx3_comp)
stargazer(modelEx3_comp, type = "latex")

#=====Exercise4: Panel Data=============
dfEx4 =read.csv("./Data/dat_A4_panel.csv")
dim(dfEx4)
#the data are collected in year 97.98.99.00.01.02.03,04,05,06,07,08,09,10,11,13,15,17,19
#Notice, all fixed parameters will be dropped, eg: sex, race, 
dfEx4 = subset(dfEx4,select= -c(KEY_SEX_1997,KEY_RACE_ETHNICITY_1997))
#First step, construct averages
#Age
dfEx4["Age_1997"] = 1997 - dfEx4$KEY_BDATE_Y_1997
dfEx4["Age_1998"] = dfEx4$Age_1997 + 1
dfEx4["Age_1999"] = dfEx4$Age_1997 + 2
dfEx4["Age_2000"] = dfEx4$Age_1997 + 3
dfEx4["Age_2001"] = dfEx4$Age_1997 + 4
dfEx4["Age_2002"] = dfEx4$Age_1997 + 5
dfEx4["Age_2003"] = dfEx4$Age_1997 + 6
dfEx4["Age_2004"] = dfEx4$Age_1997 + 7
dfEx4["Age_2005"] = dfEx4$Age_1997 + 8
dfEx4["Age_2006"] = dfEx4$Age_1997 + 9
dfEx4["Age_2007"] = dfEx4$Age_1997 + 10
dfEx4["Age_2008"] = dfEx4$Age_1997 + 11
dfEx4["Age_2009"] = dfEx4$Age_1997 + 12
dfEx4["Age_2010"] = dfEx4$Age_1997 + 13
dfEx4["Age_2011"] = dfEx4$Age_1997 + 14
dfEx4["Age_2013"] = dfEx4$Age_1997 + 16
dfEx4["Age_2015"] = dfEx4$Age_1997 + 18
dfEx4["Age_2017"] = dfEx4$Age_1997 + 20
dfEx4["Age_2019"] = dfEx4$Age_1997 + 21
dfEx4 = subset(dfEx4,select= -c(KEY_BDATE_Y_1997,KEY_BDATE_M_1997))
dfEx4["avg_Age"] = rowSums(dfEx4 %>%
                             select(matches('Age_[0-9][0-9][0-9][0-9]')) , na.rm=TRUE)/(2019-1997)
#Work Experience
library(dplyr)
dfEx4["work_exp_1997"]=rowSums(dfEx4 %>%
                   select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_1997')) , na.rm=TRUE)
dfEx4["work_exp_1998"]=rowSums(dfEx4 %>%
                               select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_1998')) , na.rm=TRUE)
dfEx4["work_exp_1999"]=rowSums(dfEx4 %>%
                               select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_1999')) , na.rm=TRUE)
dfEx4["work_exp_2000"]=rowSums(dfEx4 %>%
                               select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2000')) , na.rm=TRUE)
dfEx4["work_exp_2001"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2001')) , na.rm=TRUE)
dfEx4["work_exp_2002"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2002')) , na.rm=TRUE)
dfEx4["work_exp_2003"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2003')) , na.rm=TRUE)
dfEx4["work_exp_2004"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2004')) , na.rm=TRUE)
dfEx4["work_exp_2005"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2005')) , na.rm=TRUE)
dfEx4["work_exp_2006"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2006')) , na.rm=TRUE)
dfEx4["work_exp_2007"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2007')) , na.rm=TRUE)
dfEx4["work_exp_2008"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2008')) , na.rm=TRUE)
dfEx4["work_exp_2009"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2009')) , na.rm=TRUE)
dfEx4["work_exp_2010"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2010')) , na.rm=TRUE)
dfEx4["work_exp_2011"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2011')) , na.rm=TRUE)
dfEx4["work_exp_2013"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2013')) , na.rm=TRUE)
dfEx4["work_exp_2015"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2015')) , na.rm=TRUE)
dfEx4["work_exp_2017"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2017')) , na.rm=TRUE)
dfEx4["work_exp_2019"]=rowSums(dfEx4 %>%
                                 select(matches('CV_WKSWK_JOB_DLI.[0-9][0-9]_2019')) , na.rm=TRUE)
dfEx4["avg_wk_exp"] = rowSums(dfEx4 %>%
                                select(matches('work_exp_[0-9][0-9][0-9][0-9]')) , na.rm=TRUE)/(2019-1997)
dfEx4 = as.data.frame(dfEx4 %>%
  select(-matches("CV_WKSWK_JOB_DLI.[0-9][0-9]_[0-9][0-9][0-9][0-9]")))
colnames(dfEx4)
#Income 
dfEx4["Income_1997"] = dfEx4$YINC.1700_1997
dfEx4["Income_1998"] = dfEx4$YINC.1700_1998
dfEx4["Income_1999"] = dfEx4$YINC.1700_1999
dfEx4["Income_2000"] = dfEx4$YINC.1700_2000
dfEx4["Income_2001"] = dfEx4$YINC.1700_2001
dfEx4["Income_2002"] = dfEx4$YINC.1700_2002
dfEx4["Income_2003"] = dfEx4$YINC.1700_2003
dfEx4["Income_2004"] = dfEx4$YINC.1700_2004
dfEx4["Income_2005"] = dfEx4$YINC.1700_2005
dfEx4["Income_2006"] = dfEx4$YINC.1700_2006
dfEx4["Income_2007"] = dfEx4$YINC.1700_2007
dfEx4["Income_2008"] = dfEx4$YINC.1700_2008
dfEx4["Income_2009"] = dfEx4$YINC.1700_2009
dfEx4["Income_2010"] = dfEx4$YINC.1700_2010
dfEx4["Income_2011"] = dfEx4$YINC.1700_2011
dfEx4["Income_2013"] = dfEx4$YINC.1700_2013
dfEx4["Income_2015"] = dfEx4$YINC.1700_2015
dfEx4["Income_2017"] = dfEx4$YINC.1700_2017
dfEx4["Income_2019"] = dfEx4$YINC.1700_2019
dfEx4 = as.data.frame(dfEx4 %>%
                        select(-matches("YINC.1700_[0-9][0-9][0-9][0-9]")))
dfEx4["Avg_Income"] = log((rowSums(dfEx4 %>%
                                select(matches('YINC.1700_[0-9][0-9][0-9][0-9]')) , na.rm=TRUE)/19)+0.0000001)
dfEx4 = as.data.frame(dfEx4 %>%
                        select(-matches("YINC.1700_[0-9][0-9][0-9][0-9]")))
colnames(dfEx4)
#Marital Status
#I consider them as fixed effects and only rename the variable
dfEx4["Marital_1997"] = 0
dfEx4$Marital_1997[dfEx4$CV_MARSTAT_COLLAPSED_1997==1] = 1
dfEx4["Marital_1998"] = 0
dfEx4$Marital_1998[dfEx4$CV_MARSTAT_COLLAPSED_1998==1] = 1
dfEx4["Marital_1999"] = 0
dfEx4$Marital_1999[dfEx4$CV_MARSTAT_COLLAPSED_1999==1] = 1
dfEx4["Marital_2000"] = 0
dfEx4$Marital_2000[dfEx4$CV_MARSTAT_COLLAPSED_2000==1] = 1
dfEx4["Marital_2001"] = 0
dfEx4$Marital_2001[dfEx4$CV_MARSTAT_COLLAPSED_2001==1] = 1
dfEx4["Marital_2002"] = 0
dfEx4$Marital_2002[dfEx4$CV_MARSTAT_COLLAPSED_2002==1] = 1
dfEx4["Marital_2003"] = 0
dfEx4$Marital_2003[dfEx4$CV_MARSTAT_COLLAPSED_2003==1] = 1
dfEx4["Marital_2004"] = 0
dfEx4$Marital_2004[dfEx4$CV_MARSTAT_COLLAPSED_2004==1] = 1
dfEx4["Marital_2005"] = 0
dfEx4$Marital_2005[dfEx4$CV_MARSTAT_COLLAPSED_2005==1] = 1
dfEx4["Marital_2006"] = 0
dfEx4$Marital_2006[dfEx4$CV_MARSTAT_COLLAPSED_2006==1] = 1
dfEx4["Marital_2007"] = 0
dfEx4$Marital_2007[dfEx4$CV_MARSTAT_COLLAPSED_2007==1] = 1
dfEx4["Marital_2008"] = 0
dfEx4$Marital_2008[dfEx4$CV_MARSTAT_COLLAPSED_2008==1] = 1
dfEx4["Marital_2009"] = 0
dfEx4$Marital_2009[dfEx4$CV_MARSTAT_COLLAPSED_2009==1] = 1
dfEx4["Marital_2010"] = 0
dfEx4$Marital_2010[dfEx4$CV_MARSTAT_COLLAPSED_2010==1] = 1
dfEx4["Marital_2011"] = 0
dfEx4$Marital_2011[dfEx4$CV_MARSTAT_COLLAPSED_2011==1] = 1
dfEx4["Marital_2013"] = 0
dfEx4$Marital_2013[dfEx4$CV_MARSTAT_COLLAPSED_2013==1] = 1
dfEx4["Marital_2015"] = 0
dfEx4$Marital_2015[dfEx4$CV_MARSTAT_COLLAPSED_2015==1] = 1
dfEx4["Marital_2017"] = 0
dfEx4$Marital_2017[dfEx4$CV_MARSTAT_COLLAPSED_2017==1] = 1
dfEx4["Marital_2019"] = 0
dfEx4$Marital_2019[dfEx4$CV_MARSTAT_COLLAPSED_2019==1] = 1
dfEx4 = as.data.frame(dfEx4 %>%
                        select(-matches("CV_MARSTAT_COLLAPSED_[0-9][0-9][0-9][0-9]")))
colnames(dfEx4)
#Highest Degree
#As I did in previous problems, I turn this to years of education

dfEx4_temp = as.data.frame(dfEx4 %>%
  select(matches("CV_HIGHEST_DEGREE_[0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9]")))
dfEx4 = as.data.frame(dfEx4 %>%
                             select(-matches("CV_HIGHEST_DEGREE_[0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9]"))) 
dfEx4_temp_ever = as.data.frame(dfEx4 %>%
                             select(matches("CV_HIGHEST_DEGREE_EVER_EDT_[0-9][0-9][0-9][0-9]")))
dfEx4 = as.data.frame(dfEx4 %>%
                                  select(-matches("CV_HIGHEST_DEGREE_EVER_EDT_[0-9][0-9][0-9][0-9]")))
#convert NA and none education to 0
dfEx4_temp[is.na(dfEx4_temp)] = 0
dfEx4_temp_ever[is.na(dfEx4_temp_ever)] = 0
dfEx4_temp[dfEx4_temp==0]=0
dfEx4_temp_ever[dfEx4_temp_ever==0]=0
#I treat GED as equivalent to high school, which is 12 years
dfEx4_temp[dfEx4_temp==1]=12
dfEx4_temp_ever[dfEx4_temp_ever==1]=12
dfEx4_temp[dfEx4_temp==2]=12
dfEx4_temp_ever[dfEx4_temp_ever==2]=12
#I treat Associate degree as 14 years of education, 2 years after high school
dfEx4_temp[dfEx4_temp==3]=14
dfEx4_temp_ever[dfEx4_temp_ever==3]=14
#I treat Bachelor degree as 16 years of education, 4 years after high school
dfEx4_temp[dfEx4_temp==4]=16
dfEx4_temp_ever[dfEx4_temp_ever==4]=16
#I treat Master degree as 18 years of education, 2 years after Bachelor
dfEx4_temp[dfEx4_temp==5]=18
dfEx4_temp_ever[dfEx4_temp_ever==5]=18
#I treat PhD degree as 24 years of education, 6 years after Bachelor
dfEx4_temp[dfEx4_temp==6]=24
dfEx4_temp_ever[dfEx4_temp_ever==6]=24
#I treat Professional degree as 22 years of education, 4 years after Bachelor, DDS and MD take 4 years, JD 3 years, I choose the 4 year mark
dfEx4_temp[dfEx4_temp==7]=22
dfEx4_temp_ever[dfEx4_temp_ever==7]=22
colnames(dfEx4_temp) = c("Edu_1998","Edu_1999","Edu_2000","Edu_2001","Edu_2002","Edu_2003","Edu_2004","Edu_2005","Edu_2006","Edu_2007","Edu_2008","Edu_2009","Edu_2010","Edu_2011","Edu_2013")
colnames(dfEx4_temp_ever)  =  c("Edu_2010","Edu_2011","Edu_2013","Edu_2015","Edu_2017","Edu_2019")
dfEx4_temp["Edu_2010"] = dfEx4_temp_ever["Edu_2010"] 
dfEx4_temp["Edu_2011"] = dfEx4_temp_ever["Edu_2011"] 
dfEx4_temp["Edu_2013"] = dfEx4_temp_ever["Edu_2013"] 
dfEx4_temp["Edu_2015"] = dfEx4_temp_ever["Edu_2015"] 
dfEx4_temp["Edu_2017"] = dfEx4_temp_ever["Edu_2017"] 
dfEx4_temp["Edu_2019"] = dfEx4_temp_ever["Edu_2019"] 
dfEx4= cbind(dfEx4,dfEx4_temp)
dfEx4["avg_Edu"] = rowSums(dfEx4 %>%
                             select(matches('Edu_[0-9][0-9][0-9][0-9]')) , na.rm=TRUE)/(2019-1997)
#We estimate a simple linear model with log wages
#Between Estimator
modelBetween = lm(Avg_Income~avg_Edu+avg_Age+avg_wk_exp,data=dfEx4)
summary(modelBetween)
stargazer(modelBetween, type = "latex")
#Within Estimator 
df_1998 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_1998","work_exp_1998","Income_1998","Marital_1998","Edu_1998")]
colnames(df_1998) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_1999 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_1999","work_exp_1999","Income_1999","Marital_1999","Edu_1999")]
colnames(df_1999) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2000 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2000","work_exp_2000","Income_2000","Marital_2000","Edu_2000")]
colnames(df_2000) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2001 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2001","work_exp_2001","Income_2001","Marital_2001","Edu_2001")]
colnames(df_2001) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2002 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2002","work_exp_2002","Income_2002","Marital_2002","Edu_2002")]
colnames(df_2002) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2003 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2003","work_exp_2003","Income_2003","Marital_2003","Edu_2003")]
colnames(df_2003) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2004 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2004","work_exp_2004","Income_2004","Marital_2004","Edu_2004")]
colnames(df_2004) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2005 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2005","work_exp_2005","Income_2005","Marital_2005","Edu_2005")]
colnames(df_2005) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2006 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2006","work_exp_2006","Income_2006","Marital_2006","Edu_2006")]
colnames(df_2006) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2007 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2007","work_exp_2007","Income_2007","Marital_2007","Edu_2007")]
colnames(df_2007) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2008 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2008","work_exp_2008","Income_2008","Marital_2008","Edu_2008")]
colnames(df_2008) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2009 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2009","work_exp_2009","Income_2009","Marital_2009","Edu_2009")]
colnames(df_2009) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2010 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2010","work_exp_2010","Income_2010","Marital_2010","Edu_2010")]
colnames(df_2010) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2011 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2011","work_exp_2011","Income_2011","Marital_2011","Edu_2011")]
colnames(df_2011) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2013 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2013","work_exp_2013","Income_2013","Marital_2013","Edu_2013")]
colnames(df_2013) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2015 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2015","work_exp_2015","Income_2015","Marital_2015","Edu_2015")]
colnames(df_2015) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2017 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2017","work_exp_2017","Income_2017","Marital_2017","Edu_2017")]
colnames(df_2017) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
df_2019 = dfEx4[c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age_2019","work_exp_2019","Income_2019","Marital_2019","Edu_2019")]
colnames(df_2019) = c("Avg_Income","avg_Edu","avg_Age","avg_wk_exp","Age","work_exp","Income","Marital","Edu")
dfEx4_within = rbind(df_1998,df_1999,df_2000,df_2001,df_2002,df_2003,df_2004,df_2005,df_2006,df_2007,df_2008,df_2009,
                     df_2010,df_2011,df_2013,df_2015,df_2017,df_2019)
model_within = lm((Income-Avg_Income)~(Edu-avg_Edu)+(Age-avg_Age)+(work_exp-avg_wk_exp),data=dfEx4_within)
summary(model_within)
stargazer(model_within, type = "latex")
#Difference 
df_diff_99 = dfEx4[c("X","PUBID_1997")]
df_diff_99["Income_diff"] = dfEx4$Income_1999-dfEx4$Income_1998
df_diff_99["Wk_exp_diff"] = dfEx4$work_exp_1999-dfEx4$work_exp_1998
df_diff_99["Marital_diff"] = dfEx4$Marital_1999-dfEx4$Marital_1998
df_diff_99["Edu_diff"] = dfEx4$Edu_1999-dfEx4$Edu_1998


df_diff_00 = dfEx4[c("X","PUBID_1997")]
df_diff_00["Income_diff"] = dfEx4$Income_2000-dfEx4$Income_1998
df_diff_00["Wk_exp_diff"] = dfEx4$work_exp_2000-dfEx4$work_exp_1998
df_diff_00["Marital_diff"] = dfEx4$Marital_2000-dfEx4$Marital_1998
df_diff_00["Edu_diff"] = dfEx4$Edu_2000-dfEx4$Edu_1998

df_diff_01 = dfEx4[c("X","PUBID_1997")]
df_diff_01["Income_diff"] = dfEx4$Income_2001-dfEx4$Income_1998
df_diff_01["Wk_exp_diff"] = dfEx4$work_exp_2001-dfEx4$work_exp_1998
df_diff_01["Marital_diff"] = dfEx4$Marital_2001-dfEx4$Marital_1998
df_diff_01["Edu_diff"] = dfEx4$Edu_2001-dfEx4$Edu_1998

df_diff_02 = dfEx4[c("X","PUBID_1997")]
df_diff_02["Income_diff"] = dfEx4$Income_2002-dfEx4$Income_1998
df_diff_02["Wk_exp_diff"] = dfEx4$work_exp_2002-dfEx4$work_exp_1998
df_diff_02["Marital_diff"] = dfEx4$Marital_2002-dfEx4$Marital_1998
df_diff_02["Edu_diff"] = dfEx4$Edu_2002-dfEx4$Edu_1998

df_diff_03 = dfEx4[c("X","PUBID_1997")]
df_diff_03["Income_diff"] = dfEx4$Income_2003-dfEx4$Income_2002
df_diff_03["Wk_exp_diff"] = dfEx4$work_exp_2003-dfEx4$work_exp_2002
df_diff_03["Marital_diff"] = dfEx4$Marital_2003-dfEx4$Marital_2002
df_diff_03["Edu_diff"] = dfEx4$Edu_2003-dfEx4$Edu_2002

df_diff_04 = dfEx4[c("X","PUBID_1997")]
df_diff_04["Income_diff"] = dfEx4$Income_2004-dfEx4$Income_1998
df_diff_04["Wk_exp_diff"] = dfEx4$work_exp_2004-dfEx4$work_exp_1998
df_diff_04["Marital_diff"] = dfEx4$Marital_2004-dfEx4$Marital_1998
df_diff_04["Edu_diff"] = dfEx4$Edu_2004-dfEx4$Edu_1998

df_diff_05 = dfEx4[c("X","PUBID_1997")]
df_diff_05["Income_diff"] = dfEx4$Income_2005-dfEx4$Income_1998
df_diff_05["Wk_exp_diff"] = dfEx4$work_exp_2005-dfEx4$work_exp_1998
df_diff_05["Marital_diff"] = dfEx4$Marital_2005-dfEx4$Marital_1998
df_diff_05["Edu_diff"] = dfEx4$Edu_2005-dfEx4$Edu_1998


df_diff_06 = dfEx4[c("X","PUBID_1997")]
df_diff_06["Income_diff"] = dfEx4$Income_2006-dfEx4$Income_1998
df_diff_06["Wk_exp_diff"] = dfEx4$work_exp_2006-dfEx4$work_exp_1998
df_diff_06["Marital_diff"] = dfEx4$Marital_2006-dfEx4$Marital_1998
df_diff_06["Edu_diff"] = dfEx4$Edu_2006-dfEx4$Edu_1998

df_diff_07 = dfEx4[c("X","PUBID_1997")]
df_diff_07["Income_diff"] = dfEx4$Income_2007-dfEx4$Income_1998
df_diff_07["Wk_exp_diff"] = dfEx4$work_exp_2007-dfEx4$work_exp_1998
df_diff_07["Marital_diff"] = dfEx4$Marital_2007-dfEx4$Marital_1998
df_diff_07["Edu_diff"] = dfEx4$Edu_2007-dfEx4$Edu_1998

df_diff_08 = dfEx4[c("X","PUBID_1997")]
df_diff_08["Income_diff"] = dfEx4$Income_2008-dfEx4$Income_1998
df_diff_08["Wk_exp_diff"] = dfEx4$work_exp_2008-dfEx4$work_exp_1998
df_diff_08["Marital_diff"] = dfEx4$Marital_2008-dfEx4$Marital_1998
df_diff_08["Edu_diff"] = dfEx4$Edu_2008-dfEx4$Edu_1998

df_diff_09 = dfEx4[c("X","PUBID_1997")]
df_diff_09["Income_diff"] = dfEx4$Income_2009-dfEx4$Income_1998
df_diff_09["Wk_exp_diff"] = dfEx4$work_exp_2009-dfEx4$work_exp_1998
df_diff_09["Marital_diff"] = dfEx4$Marital_2009-dfEx4$Marital_1998
df_diff_09["Edu_diff"] = dfEx4$Edu_2009-dfEx4$Edu_1998

df_diff_10 = dfEx4[c("X","PUBID_1997")]
df_diff_10["Income_diff"] = dfEx4$Income_2010-dfEx4$Income_1998
df_diff_10["Wk_exp_diff"] = dfEx4$work_exp_2010-dfEx4$work_exp_1998
df_diff_10["Marital_diff"] = dfEx4$Marital_2010-dfEx4$Marital_1998
df_diff_10["Edu_diff"] = dfEx4$Edu_2010-dfEx4$Edu_1998

df_diff_11 = dfEx4[c("X","PUBID_1997")]
df_diff_11["Income_diff"] = dfEx4$Income_2011-dfEx4$Income_1998
df_diff_11["Wk_exp_diff"] = dfEx4$work_exp_2011-dfEx4$work_exp_1998
df_diff_11["Marital_diff"] = dfEx4$Marital_2011-dfEx4$Marital_1998
df_diff_11["Edu_diff"] = dfEx4$Edu_2011-dfEx4$Edu_1998

df_diff_13 = dfEx4[c("X","PUBID_1997")]
df_diff_13["Income_diff"] = dfEx4$Income_2013-dfEx4$Income_1998
df_diff_13["Wk_exp_diff"] = dfEx4$work_exp_2013-dfEx4$work_exp_1998
df_diff_13["Marital_diff"] = dfEx4$Marital_2013-dfEx4$Marital_1998
df_diff_13["Edu_diff"] = dfEx4$Edu_2013-dfEx4$Edu_1998

df_diff_15 = dfEx4[c("X","PUBID_1997")]
df_diff_15["Income_diff"] = dfEx4$Income_2015-dfEx4$Income_1998
df_diff_15["Wk_exp_diff"] = dfEx4$work_exp_2015-dfEx4$work_exp_1998
df_diff_15["Marital_diff"] = dfEx4$Marital_2015-dfEx4$Marital_1998
df_diff_15["Edu_diff"] = dfEx4$Edu_2015-dfEx4$Edu_1998

df_diff_17 = dfEx4[c("X","PUBID_1997")]
df_diff_17["Income_diff"] = dfEx4$Income_2017-dfEx4$Income_1998
df_diff_17["Wk_exp_diff"] = dfEx4$work_exp_2017-dfEx4$work_exp_1998
df_diff_17["Marital_diff"] = dfEx4$Marital_2017-dfEx4$Marital_1998
df_diff_17["Edu_diff"] = dfEx4$Edu_2017-dfEx4$Edu_1998

df_diff_19 = dfEx4[c("X","PUBID_1997")]
df_diff_19["Income_diff"] = dfEx4$Income_2019-dfEx4$Income_1998
df_diff_19["Wk_exp_diff"] = dfEx4$work_exp_2019-dfEx4$work_exp_1998
df_diff_19["Marital_diff"] = dfEx4$Marital_2019-dfEx4$Marital_1998
df_diff_19["Edu_diff"] = dfEx4$Edu_2019-dfEx4$Edu_1998

df_diff = rbind(df_diff_99,df_diff_00,df_diff_01,df_diff_02,df_diff_03,df_diff_04,df_diff_05,df_diff_06,
                df_diff_07,df_diff_08,df_diff_09,df_diff_10,df_diff_11,df_diff_13,df_diff_15,df_diff_17,df_diff_19)
df_diff["MaritalChange"] = " No Change"
df_diff$MaritalChange[df_diff$Marital_diff==-1] = " Separation"
df_diff$MaritalChange[df_diff$Marital_diff==1] = " Get Married"
df_diff$MaritalChange = as.factor(df_diff$MaritalChange)
differenceEstimator = lm(Income_diff~Wk_exp_diff+MaritalChange+Edu_diff,data=df_diff)
summary(differenceEstimator)
stargazer(differenceEstimator, type = "latex")
