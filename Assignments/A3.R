setwd("~/Desktop/Econometrics/Econ 613/Assignments/A3")
library(dplyr)
library(stringr)
#Read in the data set
#Administrative data on students from junior high school applying for admission to senior highs
datStu = read.csv("./Data/datstu_v2.csv")
colnames(datStu)
#Longitude and Latitude of jss district 
datJssLoc = read.csv("./Data/datjss.csv")
colnames(datJssLoc)
#school information, name, code, school districtm latitude and longitude 
datSch = read.csv("./Data/datsss.csv")
colnames(datSch)


#=======Exercise 1: Basic Statistics ==========

#1. Number of Students, Schools, Programs

#a.Number of students 
dim(datStu)[1] #340823 students in total

#b.Number of schools
datSch = datSch[!datSch$V1==2009,]
datSch = as.data.frame(datSch %>% 
                         group_by(schoolcode) %>% 
                         mutate(schoolname = schoolname[which.max(str_length(schoolname))]) %>%
                         ungroup  )
#Including empty and NA school names
datSch = datSch[!duplicated(datSch$schoolcode),]
nrow(datSch)#898
#Excluding emoty or NA school names, but including schools without location data
datSch2 = datSch[(!is.na(datSch$schoolname) & datSch$schoolname!= "")& datSch$schoolname!= " ",  ]
nrow(datSch2)#689
#Further excluding those without location data
datSch3 = datSch2[!is.na(datSch2$ssslong) ,  ]
nrow(datSch3)#650

#c.Number of programs
program123 = union(union(unique(datStu$choicepgm1),unique(datStu$choicepgm2)),unique(datStu$choicepgm3))
program456 = union(union(unique(datStu$choicepgm4),unique(datStu$choicepgm5)),unique(datStu$choicepgm6))
programs = union(program123,program456)
length(programs) #32 unique non-null programs, this only counts the programs with applicants 


#2. Number of choices 
#convert data from wide to long
library(tidyr)
datStuLongTemp1 = gather(datStu, SchoolRank, SchoolChoices, schoolcode1:schoolcode6)
datStuLongTemp1[datStuLongTemp1$SchoolRank=="schoolcode1",]$SchoolRank = 1
datStuLongTemp1[datStuLongTemp1$SchoolRank=="schoolcode2",]$SchoolRank = 2
datStuLongTemp1[datStuLongTemp1$SchoolRank=="schoolcode3",]$SchoolRank = 3
datStuLongTemp1[datStuLongTemp1$SchoolRank=="schoolcode4",]$SchoolRank = 4
datStuLongTemp1[datStuLongTemp1$SchoolRank=="schoolcode5",]$SchoolRank = 5
datStuLongTemp1[datStuLongTemp1$SchoolRank=="schoolcode6",]$SchoolRank = 6
datStuLongTemp2 = gather(datStuLongTemp1, ProgramRank, ProgramChoices, choicepgm1:choicepgm6)
datStuLongTemp2[datStuLongTemp2$ProgramRank=="choicepgm1",]$ProgramRank = 1
datStuLongTemp2[datStuLongTemp2$ProgramRank=="choicepgm2",]$ProgramRank = 2
datStuLongTemp2[datStuLongTemp2$ProgramRank=="choicepgm3",]$ProgramRank = 3
datStuLongTemp2[datStuLongTemp2$ProgramRank=="choicepgm4",]$ProgramRank = 4
datStuLongTemp2[datStuLongTemp2$ProgramRank=="choicepgm5",]$ProgramRank = 5
datStuLongTemp2[datStuLongTemp2$ProgramRank=="choicepgm6",]$ProgramRank = 6
datStuLong = datStuLongTemp2[datStuLongTemp2$SchoolRank==datStuLongTemp2$ProgramRank,]
datStuLong = subset(datStuLong, select = -c(ProgramRank))
colnames(datStuLong) = c("StuID","score","agey", "male","jssdistrict","rankplace","Rank","schoolcode","Program")

#Remove choices with NA or empty schools
datStuLong = datStuLong[(!is.na(datStuLong$schoolcode) & !datStuLong$schoolcode=="" ) & !datStuLong$schoolcode==" ",]
nrow(unique(datStuLong[c("schoolcode","Program")]))#3080 unique choices 

#If further remove NA program choices
nrow(unique(datStuLong[(!is.na(datStuLong$Program) & !datStuLong$Program=="" ) & !datStuLong$Program==" ",][c("schoolcode","Program")]))#2773 unique choices 


#3 Number of students applying to at least one senior high schools int he same district to home
sssdistricts = unique(datSch$sssdistrict)
#Modify the sssdistrict in datSch 
datSch[datSch$sssdistrict=="Akwapim South",]$sssdistrict = "Akwapim South (Nsawam)"
datSch[datSch$sssdistrict=="KUMASI METRO",]$sssdistrict = "Kumasi Metro"
datSch[datSch$sssdistrict=="Accra Metro",]$sssdistrict = "Accra Metropolitan"
datSch[datSch$sssdistrict=="ACCRA METRO",]$sssdistrict = "Accra Metropolitan"
datSch[datSch$sssdistrict=="Mfantsiman",]$sssdistrict = "Mfantsiman (Saltpond)"
datSch[datSch$sssdistrict=="New Juaben",]$sssdistrict = "New Juaben (Koforidua)"
datSch[datSch$sssdistrict=="Akwapim North",]$sssdistrict = "Akwapim North (Akropong)"
datSch[datSch$sssdistrict=="Abura/Asebu/ Kwaman",]$sssdistrict = "Abura/Asebu/Kwamankese (Abura Dunkwa)"
datSch[datSch$sssdistrict=="TEMA",]$sssdistrict = "Tema"
datSch[datSch$sssdistrict=="Awutu/Efutu/ Senya",]$sssdistrict = "Awutu/Efutu/Senya (Winneba)"
datSch[datSch$sssdistrict=="MANYA KROBO",]$sssdistrict = "Manya Krobo (Odumase-Krobo)"
datSch[datSch$sssdistrict=="Asante Akim North",]$sssdistrict = "Asante Akim North (Konongo)"
datSch[datSch$sssdistrict=="East Akim",]$sssdistrict = "East Akim (Kibi)"
datSch[datSch$sssdistrict=="Gomoa",]$sssdistrict = "Gomoa (Apam)"
datSch[datSch$sssdistrict=="Tamale Metro",]$sssdistrict = "Tamale"
datSch[datSch$sssdistrict=="Dangme West",]$sssdistrict = "Dangme West (Dodowa)"
datSch[datSch$sssdistrict=="Dormaa",]$sssdistrict = "Dormaa (Dormaa Ahenkro)"
datSch[datSch$sssdistrict=="Sekyere East",]$sssdistrict = "Sekyere East (Effiduase)"
datSch[datSch$sssdistrict=="Techiman Municipal",]$sssdistrict = "Techiman"
datSch[datSch$sssdistrict=="Assin North Municipal",]$sssdistrict = "Assin North (Assin Fosu)"
datSch[datSch$sssdistrict=="Assin North",]$sssdistrict = "Assin North (Assin Fosu)"
datSch[datSch$sssdistrict=="Wenchi",]$sssdistrict = "Wenchi Municipal"
datSch[datSch$sssdistrict=="South Tongu",]$sssdistrict = "South Tongu (Sogakope)"
datSch[datSch$sssdistrict=="Kwahu West Municipal",]$sssdistrict = "Kwahu West (Nkawkaw)"
datSch[datSch$sssdistrict=="YILO KROBO",]$sssdistrict = "Yilo Krobo (Somanya)"
datSch[datSch$sssdistrict=="Nzema East",]$sssdistrict = "Nzema East (Axim)"
datSch[datSch$sssdistrict=="Kwahu North",]$sssdistrict = "Kwahu North (Donkorkrom)"
datSch[datSch$sssdistrict=="FANTEAKWA",]$sssdistrict = "Fanteakwa (Begoro)"
datSch[datSch$sssdistrict=="East Mamprusi",]$sssdistrict = "East Mamprusi (Gambaga)"
datSch[datSch$sssdistrict=="West Mamprusi",]$sssdistrict = "West Mamprusi (Walewale)"
datSch[datSch$sssdistrict=="Ga East",]$sssdistrict = "Ga East (Abokobi)"
sssdistricts = unique(datSch$sssdistrict)

#now merge the data set
datStuLongMergedTemp = merge(x=datStuLong,y=datJssLoc,by="jssdistrict",all.x=TRUE)
datStuLongMerged = merge(x=datStuLongMergedTemp,y=datSch,by="schoolcode",all.x=TRUE)
library(dplyr)
colnames(datStuLongMerged)
str(datStuLongMerged)
datTemp = as.data.frame(datStuLongMerged %>% group_by(StuID,Rank) %>% summarise(sameDist = as.numeric(sssdistrict==jssdistrict))
                        %>% group_by(StuID) %>% summarise(atLeastOneSameDist = (sum(sameDist)>0)))
nrow(datTemp[datTemp$atLeastOneSameDist==TRUE,])#261195 students did
#4&5&6 Number of students each senior high school admitted, the quality and cutoff of senior high schools 
#Drop the rows with rankplace empty or NA or 99 and keep only the rows for matched schools and students
datStuLongAdmitTemp = datStuLongMerged[((!is.na(datStuLongMerged$rankplace) & !datStuLongMerged$rankplace=="")& !datStuLongMerged$rankplace==" " ) & !datStuLongMerged$rankplace==99,]
datStuLongAdmit = datStuLongAdmitTemp[datStuLongAdmitTemp$rankplace==datStuLongAdmitTemp$Rank,]
datSchool = as.data.frame(datStuLongAdmit %>% group_by(schoolname,schoolcode) %>% summarise(cutoff = min(score),quality = mean(score),numStudent = n_distinct(StuID)))
head(datSchool)
unique(datSchool$cutoff)
unique(datSchool$quality)
unique(datSchool$numStudent)
nrow(datSchool)
length(unique(datSchool$schoolcode))#double check to make sure school name and school code are 1-1, because I grouped by both
#=======Exercise 2: Data ==========
colnames(datSch)
datSch = subset(datSch,select = -c(V1))
datSchoolTemp = as.data.frame(datStuLongAdmit %>% group_by(schoolcode,Program) %>% summarise(cutoff = min(score),quality = mean(score),numStudent = n_distinct(StuID)))
datSeniorHighs = merge(x=datSch,y=datSchoolTemp,by="schoolcode",all.x=TRUE)
colnames(datSeniorHighs)

#=======Exercise 3: Distance ==========
rm(list=setdiff(ls(), c("datSeniorHighs","datStuLongAdmit","datStuLongMerged","datStu","datSch","datJssLoc")))
rowsNum = nrow(datStuLongMerged)
datStuLongMerged = datStuLongMerged[!is.na(datStuLongMerged$ssslat),]
datStuLongMerged = datStuLongMerged[!is.na(datStuLongMerged$ssslong),]
datStuLongMerged = datStuLongMerged[!is.na(datStuLongMerged$point_x),]
datStuLongMerged = datStuLongMerged[!is.na(datStuLongMerged$point_y),]
datStuLongMerged$dist = sqrt((69.172*(datStuLongMerged$ssslong-datStuLongMerged$point_x)*cos(datStuLongMerged$point_y/57.3))^2+(69.172*(datStuLongMerged$ssslat-datStuLongMerged$point_y))^2)
rowsNum - nrow(datStuLongMerged)#2102 choices were removed
#=======Exercise 4: Dimension Reduction ==========
#scode_rev
datStuLongMerged = datStuLongMerged[!is.na(datStuLongMerged$schoolcode),]
datStuLongMerged$scode_rev = substr(datStuLongMerged$schoolcode,1,3)
#Recode the program variables into 4 categories
unique(datStuLongMerged$Program)
#others
datStuLongMerged$pgm_rev = "others"
#arts(general arts, visual arts)
datStuLongMerged[datStuLongMerged$Program=="General Arts" | datStuLongMerged$Program=="Visual Arts",]$pgm_rev = "arts"
#economics(business and home economics)
datStuLongMerged[datStuLongMerged$Program=="Business" | datStuLongMerged$Program=="Home Economics",]$pgm_rev = "economics"
#science(general science)
datStuLongMerged[datStuLongMerged$Program=="General Science" ,]$pgm_rev = "science"

#choice_rev
datStuLongMerged$choice_rev = paste(datStuLongMerged$scode_rev,datStuLongMerged$pgm_rev,sep="")
datStuLongMerged$Rank = as.integer(datStuLongMerged$Rank)
#

programs = as.data.frame(datStuLongMerged %>% filter(datStuLongMerged$Rank==datStuLongMerged$rankplace) %>%
                                   group_by(choice_rev) %>% summarise(cutoff = min(score), quality = mean(score)) )

datStuLong = merge(x=datStuLongMerged,y=programs,by="choice_rev",all.x=TRUE)
datStuLong = subset(datStuLong, select = -c(schoolcode,scode_rev,pgm_rev,V1,Program,X))
#Let's pick out the top 20,000 students, inclusive for students with atie at 20,000
StuScores = datStuLong[!duplicated(datStuLong$StuID),][c("StuID","score")]
StuScores = StuScores[order(StuScores$score,decreasing = TRUE),]
datStuLong = datStuLong[!is.na(datStuLong$score),]
datStuLong = datStuLong[datStuLong$score >= StuScores$score[20000],]
datChoice = as.data.frame(datStuLong %>% group_by(choice_rev) %>% summarize (cutoff = min(score), quality = mean(score)))
rm(list=setdiff(ls(), c("datSeniorHighs","datStuLongAdmit","datStuLong","datChoice")))


#=======Exercise 5: First Model ==========
#we want to understand the effect of the student test score on his first choice.
#we want to investiagte first choices, so we take that portion of data out
datStuFirstChoice = datStuLong[datStuLong$Rank==1,]
colnames(datStuFirstChoice)
#Let's delete the columns that are excessive for this exercise
datEx5 = subset(datStuFirstChoice, select = -c(agey,male,jssdistrict,rankplace,point_x,point_y,schoolname,sssdistrict,ssslong,ssslat,Rank,dist,cutoff))
colnames(datEx5)
ChoiceEx5 = as.data.frame(datEx5 %>% group_by(choice_rev) %>% summarize (cutoff = min(score), quality = mean(score)))

ChoiceEx5$choice_index = 1:nrow(ChoiceEx5)
datEx5 = merge(x=datEx5,y=ChoiceEx5[c("choice_rev","choice_index")],by="choice_rev",all.x=TRUE)
rm(list=setdiff(ls(), c("datEx5","ChoiceEx5")))

#Each student has a different test score, 
#Because of the limit of the computation power of my laptop
# I will assume that student only differ in test score
#I propose the following multinomial logit model
#v(x_{ij}) = \alpha_j + \beta_j*score_i

#To save space, I collect data into two
#=====Custom Likelihood Function
like_fun5 = function(param,dat_stu)
{
  #individual characteristics
  score =  dat_stu$score
  ch         =  dat_stu$choice_index
  
  
  ni = nrow(dat_stu)
  nj = nrow(dat_choice)
  nj_sub = nj-1
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:nj_sub]
  pn1 = c(0,pn1)
  pn2    = param[(nj_sub+1):(2*(nj_sub))]
  pn2 = c(0,pn2)
  for (j in 1:nj)
  {
    # multinomial logit
    ut[,j] = pn1[j] +  score*pn2[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
  #return (prob)
}

paramLength5_half = 2*(nrow(ChoiceEx5)-1)
Sys.time()
try5 = optim(par = c(runif(paramLength5_half,-10,10),runif(paramLength5_half,-0.1,0.1)),fn=like_fun5,method="BFGS",control=list(trace=6,maxit=1000),dat_stu=datEx5,dat_choice=ChoiceEx5)
Sys.time()
#It took about 24 hours to fit this multionomial logit model on my laptop. The fitted coeffiient is attached at the very end of this R file for it's size.
#Log likelihood: -126028.283127  (I added a negative sign because an extra negative sign was added in like_fun5).


#I didn't actually run 100 rounds of optimization. This is just showing what need to be done to avoid local optimum as much as possible.
#It is unfeasible to run 100 rounds of optimization given that each round takes 24 hours.
numTry = 100
paramLength5_half = (nrow(ChoiceEx5)-1)
result5 = mat.or.vec(numTry,paramLength5)
minLoc5 = 1
minLike5 = Inf
set.seed(123)
Sys.time()
for(i in 1:numTry){
  randomStart = c(runif(paramLength5_half,-10,10),runif(paramLength5_half,-0.1,0.1))
  outcome = optim(par = randomStart,fn=like_fun5,method="BFGS",control=list(trace=6,maxit=1000),dat_stu=datEx5,dat_choice=ChoiceEx5)
  if(outcome$value < minLike){
    minLike5 = outcome$value
    minLoc5 = i
  }
  result5[i,]=outcome$par
}#
Sys.time()

#calculating marginal effects
#all Marginal Effects 
#p
beta = c(0,try5$par[246:490])
alpha = c(0,try5$par[1:245])
score = datEx5$score
v = mat.or.vec(nrow(datEx5),246)
for (i in 1:246){
  v[,j] = alpha[j] +  score*beta[j] 
}
p_temp = exp(v)
p   = sweep(p_temp,MARGIN=1,FUN="/",STATS=rowSums(p_temp)) # an example of how to construct
# marginals
marginal = mat.or.vec(nrow(datEx5),246)
for (i in 1:nrow(datEx5)){
  for ( j in 1:246){
    marginal[i,j] = p[i,j]*(beta[j]-sum(beta*p_ij[i,]))
  }
}
marginal[1,2]#-0.000141312
#=======Exercise 6: Second Model ==========
#Each choice has a different quality score, we need a conditional logit model 
#For the sake of computation simplicity, I propose to estimate the following simple
#conditional logit model
#v(x_{ij}) = \alpha_j + \beta*quality_j
#This is very unreasonable as it assumes students to be homogeneous and differ in purely random ways
#So the best way to interpret this ias a proof of concept showing the algorithm I implement to  work
datEx6 = datEx5
ChoiceEx6 = ChoiceEx5

#==========Custom likelihood function
rm(list=setdiff(ls(), c("datEx6","ChoiceEx6")))
#the likelihood function
like_fun6 = function(param,dat_stu,dat_choice)
{
  #individual characteristics
  score =  dat_stu$score
  quality = dat_choice$quality
  ch         =  dat_stu$choice_index
  
  
  ni = nrow(dat_stu)
  nj = nrow(dat_choice)
  ut = mat.or.vec(ni,nj)
  # conditional logit
  pn1    = param[1:nj-1]
  pn1 = c(0,pn1)
  pn2    = param[nj]
  for (j in 1:nj)
  {
    # conditional logit
    ut[,j] = pn1[j] +  quality[j]*pn2
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
  #return (probc)
}
paramLength6 = nrow(ChoiceEx6)
Sys.time()
try6 =  optim(par = runif(paramLength6,-1,1),fn=like_fun6,method="BFGS",control=list(trace=TRUE,maxit=1000,REPORT=5),dat_stu=datEx6,dat_choice=ChoiceEx6)
Sys.time()
#It took 4 hours for this one round of optimization to converge.
#The coefficients are attached to the end of the file for it's sheer size. 
#Log likelihood: -76690.856693  (I added a negative sign because an extra negative sign was added in like_fun6).




#I didn't actually run 100 rounds of optimization to avoid local optimum. This is purely limited by my computational capabilities
#The following code shows how to run 100 rounds of optimization and identify the optimum among the optimums in the hope of avoiding local optimality.
numTry = 100
result6 = mat.or.vec(numTry,paramLength)
minLoc6 = 1
minLike6 = Inf
set.seed(123)
Sys.time()
for(i in 1:numTry){
  randomStart = runif(paramLength,-1,1)
  outcome = optim(par = randomStart,fn=like_fun6,method="BFGS",control=list(trace=TRUE,maxit=1000,REPORT=5),dat_stu=datEx6,dat_choice=ChoiceEx6)
  if(outcome$value < minLike){
    minLike6 = outcome$value
    minLoc6 = i
  }
  result6[i,]=outcome$par
}
Sys.time()
#calculating marginal effect
#The example
beta_ex6 = try6$par[246]
alpha_ex6_2 = try6$par[1]
alpha_ex6_4 = try6$par[3]
v_ex6_12 = alpha_ex6_2 + beta_ex6*ChoiceEx6$quality[2]
alpha_ex6 = try6$par[1:245]
denominator = sum(exp(alpha_ex6 + beta_ex6*ChoiceEx6$quality[2:246]))
#p_12
p_ex6_12 = exp(v_ex6_12)/denominator

v_ex6_14 = alpha_ex6_4 + beta_ex6*ChoiceEx6$quality[4]
#p_14
p_ex6_14 = exp(v_ex6_14)/denominator

#All Marginal Effects
#marginal effects with respect to self quality
alpha_ex6 = c(0,try6$par[1:245])
beta_ex6 = try6$par[246]
quality_ex6 = ChoiceEx6$quality
v_ex6 = mat.or.vec(nrow(datEx6),246)
for (j in 1:246){
  # conditional logit
  v_ex6[,j] = alpha_ex6[j] +  quality_ex6[j]*beta_ex6
}
p_temp_ex6   = exp(v_ex6)    # exp(XB)
p_ex6   = sweep(p_temp_ex6,MARGIN=1,FUN="/",STATS=rowSums(p_temp_ex6))
marginals_ex6 = mat.or.vec(nrow(datEx6),246)
for (i in 1:nrow(datEx6)){
  for (j in 1:246){
    marginals_ex6[i,j] = p_ex6[i,j]*(1-p_ex6[i,j])*beta_ex6
  }
}
#effect with respect to the quality of the kth choice
k = 4#as an example
marginals_ex6_k = mat.or.vec(nrow(datEx6),246)
for (i in 1:nrow(datEx6)){
  for (j in 1:246){
    if(j == k) {
      marginals_ex6_k[i,j] = marginals_ex6[i,k]
    }else{
      marginals_ex6_k[i,j] = -1*p[i,j]*p[i,k]*beta_ex6
    }
  }
}

#=======Exercise 7: Counterfactual Simulations ==========
#7.2
#pick out the remaining choices
ChoiceEx7 = ChoiceEx6[!grepl("other",ChoiceEx6$choice_rev),]
#the parameters
beta_ex7 = beta_ex6
alpha_ex7 = c(0,alpha_ex6[ChoiceEx7$choice_index-1])#0 for base intercept, pick out the intercepts for remaining choices
quality_ex7 = ChoiceEx7$quality #pick out the quality for the remaining choices
prob7 = exp(alpha_ex7+beta_ex7*quality_ex7 )/sum(exp(alpha_ex6 + beta_ex6*ChoiceEx6$quality[2:246]))
prob7#These are the probabilities of choosing among the remaining choices as ordered in data set ChoiceEx7
#Please find their value inthe attachment
#7.3
#How to they change? 
denominator2 = sum(exp(alpha_ex7 + beta_ex7*quality_ex7))
prob7_new = exp(alpha_ex7+beta_ex7*quality_ex7 )/denominator2
prob7_new/prob7#all equal to 1.035513

#An equivalent way of calculating this would be 
prob7_new2 = prob7/(sum(prob7))
prob7_new2/prob7#all equal to 1.035513


#=========Attachment===========================
#==========Attachment:Exercise 5=========
#Fited parameters for Exercise 5
try5$par 
#The following are 490 parameters. The first 245 are intercepts and the remaining 245 are slopes b_j's. 
#The choice 100Arts is used as the baseline, the order of the parameters follow the order indexed in ChoiceEx5
#  [1]  4.0034651026 -8.6885051531  4.6610041111 -2.0697130044 -0.7702431036  4.5061505747 -0.6000976096  9.9909329933  6.9191031044  6.5641460754
#[11]  5.2331475647 -7.7578701638  6.7793858223  6.2769479473  9.8282437894 -9.0336665977  4.9357892715 -1.6137295682  1.6426898912  6.6146535325
#[21] -9.7291070641 -9.7695647366  9.9288243708  5.3162584808 -3.7038442236  6.2402348872  9.8240100432 -5.9848331413  2.9207444051 -1.2489854218
#[31]  8.7605652306  9.7607789361 -0.8716545277 -5.3878963731  3.9097894119  1.1341552301  1.6947170966 -1.3272987276 -1.4762427405  1.9368519831
#[41] -0.9582766598  9.1318967100  6.8885982965 -5.5444238908 -1.5967302332 -1.6003833720 -9.5841830224 -2.3910406092 -1.5530091728 -6.1714790110
#[51]  4.1062389160 -9.7297059102  4.6240832690 -1.3456473103  4.6249891440 -9.7196506303 -9.0934151090 -1.6317124711  2.8772337493 -5.6041242881
#[61]  6.6682824719  3.8953462693  7.9825388312 -1.7143459232 -1.7795610136 -4.9997002119 -2.5246592193 -5.3622800050  8.6318994129 -0.4731048970
#[71] -9.0466999402  5.4693030757 -3.3776534683  1.0500065508  3.5263714879 -4.4533125823 -8.9428864326 -1.4847952330  2.2023619810  9.5210644865
#[81]  9.3961859818 -8.2304257174 -0.0323374418 -2.0860085589 -2.1931832353 -3.6300101830  3.6516180186 -4.3077018485 -2.1982943889  9.8032383137
#[91]  5.8401146298 -3.7521264365  6.3422869987 -5.5819529017  0.0217475747 -6.6605540738 -7.6068446199  7.9521341781 -8.1256363340  7.3011700384
#[101] -4.4681342126 -9.4542270646  5.7476589586 -2.8268365356 -9.4979406847  4.9214835106 -9.4074884593 -1.3227033801 -4.8653908091 -0.0391886699
#[111]  3.0686873486 -8.3268622868 -6.3907040438 -7.9108436922  6.2476341621  2.5498612533  2.6113616417 -2.1824431071 -1.3549285242 -3.0273199547
#[121]  5.8711857136  7.7385492540  3.0225411172  1.8569941711 -3.4436077274 -8.4862126410  1.8276937927 -6.4332648268  4.8269295877 -4.1223967713
#[131]  2.7837162139 -7.4909330539 -4.8946843250  6.4092646010  6.0764588838 -9.0809501773  7.5105913587 -7.4398192449  2.1766280709 -0.6085823289
#[141]  1.1455677156  4.6076749453 -0.6028263017  9.7726356997  9.6931212244 -2.1338968523  2.1764425839 -3.0903253984 -1.2578567499 -5.4173374093
#[151] -9.8785550380 -9.5224622391 -8.8242015968  5.2915708180 -0.6191852342  7.0167791461  1.6343436252 -6.2622347521  2.1813351513  7.8668344384
#[161]  5.8860395525  0.0818679948 -1.5394345252  3.0874091496  7.7547674548  7.8400520499 -6.4321892429 -4.8499515115 -7.7312299050  5.9431791725
#[171] -7.8092937840  6.5814338908  5.4219385876  8.8740141999 -0.6604194475  8.7974561065  5.7033440311  7.4922484335 -2.1110622359 -8.0062356428
#[181] -4.4608064423  4.4444234791 -2.8739242703 -2.3457623320 -9.3606097696  0.4289503273 -7.5784417940  7.7036835294 -8.5072289148  5.6844831833
#[191]  7.0825276401 -9.2564559020 -4.4972269589  2.8412764111  5.7015617556 -1.9987457618  8.7059571554 -2.1880990180 -1.5198311848 -4.2714752332
#[201]  2.2349681705  1.1762547726 -9.2007684943  4.9053930439 -5.8702515857  3.1297152490  0.4484257134  2.6686245921  2.3511274606 -4.3017918163
#[211] -5.5238555652  0.7805437112  9.9274395453 -3.4216648565 -3.5334584309 -9.2934675663  0.5122904217  1.9361383002  4.8417331837  9.0567425918
#[221] -0.2075752430 -8.3342539032 -4.8015606154  7.4969750232  7.0409610561  6.8580048088  8.9380912691  3.0536326952 -0.2369573895 -6.2448659865
#[231]  4.2606483117 -8.2986005861 -0.7754191508  8.7179000766  6.9058729194  4.3237343384 -7.5237211450  1.8965492257  9.7971110599  2.3081300491
#[241] -4.5383256496 -8.7258631368 -8.5538481795  7.4702668577 -2.7435000660 -0.0185642312  0.0037645591 -0.0172152362  0.0093575186 -0.0780396809
#[251] -0.0147982879  0.0055168385 -0.0286750953 -0.0207577352 -0.0835314242 -0.0175479226 -0.0613530543 -0.0314955198 -0.0339358007 -0.0606059901
#[261]  0.0150513303 -0.0220912054 -0.0937146672 -0.0147271610 -0.0240689192  0.0181708369 -0.0401969089 -0.0343341358 -0.0130095830  0.0094032745
#[271] -0.0745380587 -0.0260201425 -0.0014055131 -0.0989271105 -0.0728472161 -0.0988305797 -0.0237903266  0.0025256626  0.0007859971 -0.0553523562
#[281] -0.0046393229 -0.0081041670 -0.0035428750 -0.0014088301 -0.0168085803 -0.0081695447 -0.0354145250 -0.0296612466  0.0062721891 -0.0052534954
#[291] -0.0072966797 -0.0963673740 -0.0668946565 -0.0074455488 -0.0148014797 -0.0445598457  0.0123574064 -0.0258621941 -0.0152843071 -0.0133330646
#[301]  0.0230680826  0.0183319043 -0.0700790946 -0.0077255826 -0.0454497547 -0.0186721869 -0.0555499834 -0.0325841919  0.0010808716 -0.0001067741
#[311] -0.0678073929 -0.0455839721 -0.0295657858 -0.0414189727 -0.0735756068 -0.0859985494 -0.0111075819 -0.0263677341  0.0018951935 -0.0278503451
#[321] -0.0516699235 -0.0929698018 -0.0045972226 -0.0102352296 -0.0296303850 -0.0291034052 -0.0125263653 -0.0066856668 -0.0017441580 -0.0010745535
#[331] -0.0689471728 -0.0158005183 -0.0571239150 -0.0415422497 -0.0617606958 -0.0900000446 -0.0033992668 -0.0273716471  0.0033894456 -0.0064970394
#[341] -0.0467745037  0.0030458190 -0.0274996129  0.0114865500 -0.0296573573 -0.0010522826  0.0155404535 -0.0335137003 -0.0025569020 -0.0990782052
#[351] -0.0509019081 -0.0187084464 -0.0764096726  0.0106335724 -0.0032102577 -0.0087424734 -0.0924920322  0.0033662338  0.0037790326 -0.0303127409
#[361] -0.0187075677 -0.0374352738 -0.0122489713 -0.0583850671 -0.0945788812 -0.0278188013 -0.0342433041 -0.0372967188 -0.0236134080 -0.0015211474
#[371] -0.0321137659 -0.0214549200 -0.0168386997 -0.0209171029  0.0008182768 -0.0969401792  0.0223949341 -0.0530340033 -0.0120693934 -0.0156166644
#[381]  0.0217209757 -0.0271425079 -0.0082698646 -0.0857303503 -0.0061909768 -0.0862601327 -0.0200820359 -0.0329745502 -0.0290949777 -0.0344396063
#[391]  0.0003790982 -0.1665109110 -0.0820482826 -0.0061277209  0.0001448207 -0.0553487432 -0.0175174620  0.0171556246 -0.0213845644 -0.0079771754
#[401] -0.0259354318 -0.0119007413 -0.0507801092 -0.0148688745 -0.0529798124 -0.0599526022 -0.0154468776 -0.0725101189 -0.0188986829 -0.0392415138
#[411] -0.0394607535 -0.0714729643 -0.0281160021 -0.0508989865 -0.0908188107  0.0083894023 -0.0601672657 -0.0155567681 -0.0271066130 -0.0035860148
#[421] -0.0239828447 -0.0288281226 -0.0304490847 -0.0860641762  0.0025086185 -0.0052165450 -0.0303977074 -0.0016915486 -0.0775231078  0.0093661818
#[431] -0.0101753514 -0.0452085578 -0.0685287222  0.0035470230 -0.0259971506 -0.0366470241  0.0060373193 -0.0009524377 -0.0254172471 -0.0226222439
#[441] -0.0895166299 -0.0327816587  0.0039694969  0.0016707565  0.0028764102 -0.0835459916 -0.0844714776 -0.0091427287 -0.0314036243 -0.0708180250
#[451] -0.0267797224 -0.0082943843 -0.0129824441 -0.0547061012  0.0058542254 -0.0243540022 -0.0078058168 -0.0851432464 -0.0360649407 -0.0022124383
#[461]  0.0129146502 -0.0098033170 -0.0935466007 -0.0924531639 -0.0905303415 -0.0678081930 -0.0064993046 -0.0005061566 -0.0304653609 -0.0373475512
#[471] -0.0340026891 -0.0375270945 -0.0965245597 -0.0108520712 -0.0866048071 -0.0180530257 -0.0729313688 -0.0013940471 -0.0367209588 -0.0486359893
#[481] -0.0728009446  0.0123034603 -0.0576007916 -0.0358618667 -0.0182999388  0.0032250363 -0.0253114201  0.0135704418 -0.0255959247 -0.0689526758

#==========Attachment:Exercise 6=========
#Fited parameters for Exercise 6
try6$par
#There are, intotal, 246 parameters. The first 245 are intercepts. The choice 100arts is used as the baseline for comparison so the parameters are order
#as in ChoiceEx6 starting with the second choice, 100economics. The last parameter is the estimated \beta for school quality.
#1.45930705  -1.10513125   1.51482161   3.25293526   3.05835585   1.00644599   1.10601988   2.44289322   2.17139839  -0.83725663   1.50349700
#[12]   0.60730974  -0.15723684  -6.02042682  -0.56315387  -0.01013447   0.77096261  -2.62212981   0.17994933   1.48737645   1.77829358 -11.08255981
#[23]   2.42787756   3.06606694   2.13214731   2.43450365   1.76488726  -2.79058790  -0.51420872  -1.27811321   0.95113414   2.83940304   1.68451095
#[34]  -0.83364842   0.21574105   2.84434587   1.70608757   0.93783526   1.08006127   0.40478973  -0.14151483   0.88935115  -0.68271468   1.43555960
#[45]  -0.05816148   1.71449088  -1.61090580  -0.43224843   1.84299663   1.30978335   0.33089948   0.85100935   0.40982214  -0.43260292   1.01438682
#[56]   0.76984713  -0.34461649  -0.14017811   1.03293918   0.71201471  -0.48516264   0.07291355  -8.82443187   2.51668527   2.15038408   1.47883766
#[67]   0.88043278   0.91848244  -0.93725698  -0.14183200   2.43272594   1.91809370   1.00567905   0.38398663  -3.29624929   2.31393439   1.78803920
#[78]   0.60593241   0.22711800   1.67597584   1.98336205  -1.35617599   1.33983668   2.31571284   1.30389771   1.47192387   1.64032093   1.83369737
#[89]  -2.64769657   1.44159067  -1.42168693  -3.95874848   1.62395324  -4.95354460   1.50630028   1.48160738  -0.43260616   1.20207568   1.01320976
#[100]   1.21781180   0.18147548   0.67517605  -3.29613279  -1.67436608  -1.61161224  -1.61173005  -0.45810093   3.02801638   2.70616331   1.68314542
#[111]   2.36934287   0.49112119  -0.24720966  -2.95895828  -5.65325994   0.94490528   0.77619362  -2.95927558  -2.28487644  -1.78000425  -0.09528815
#[122]  -4.03069379  -2.11617555  -7.98758504   1.02163140  -1.33896827  -5.50367370   1.67530604   0.98425762   0.57532731   4.63212661   3.93762832
#[133]   2.86521314   2.79556981   2.56473485   2.17821271   1.70560345   0.93938760   0.87290978   0.51113643  -0.22583748   0.62649710   3.01287324
#[144]   2.81470444   0.52166102   1.92915425   1.35002166   1.83526645  -0.14306440   0.76855790   1.44386878  -0.07265396   2.75859329   1.22638170
#[155]   0.31849565   2.00111847   0.45527313   0.43885437   2.34560061   0.69019125  -1.44330362  -1.10590062   0.85458064   0.02765340  -0.43257912
#[166]  -6.92241818  -0.19766270  -1.54878473  -0.57031375 -13.84904195   0.60854084  -0.93793850   3.31110469   2.08443778   2.73777287   1.96179499
#[177]  -0.32745853   1.41417845   0.35874683  -1.78009380  -0.76961558  -0.09476252   2.19379157  -0.76958137  -0.60015297  -0.27586028   0.27142082
#[188]  -0.68252664 -10.54299603  -1.23605733  -3.46478550 -11.36775604  -1.84016994 -14.96467618   0.88165920   1.87137343   0.43237748   2.87856149
#[199]   1.74403252   0.66238442   1.37774679  -0.90708464  -0.43254491  -2.11669448 -11.31607960  -0.26411476   1.58945433   0.69916184   0.49590810
#[210]   1.60716558   2.19571052   1.66433001  -0.13574287   2.09353402   0.43982676  -0.97917795  -0.42213533  -1.27299450   0.01141238  -2.28492035
#[221]   0.34489719  -1.75659355   0.09207057  -3.35854600   0.07281759   0.57785560  -6.67918182  -0.70791154   1.16961986   2.24539209   1.05477288
#[232]  -0.71137022   1.95856956 -14.99194464  -2.62214703  -2.92744195   0.51716243   0.42976716   0.18925931  -0.51418112   0.11413518   0.04164840
#[243]   1.91740096   0.94849553  -1.80501016   0.16834293

#==========Attachment:Exercise 7=========
#prob7
# [1] 9.848142e-04 6.377218e-04 2.106257e-03 6.687944e-02 3.165577e-02 6.257795e-02 5.830255e-03 5.095744e-03 3.331748e-03 1.467378e-04 9.838061e-05
#[12] 1.958122e-04 5.883310e-04 4.403363e-04 2.939980e-04 1.127318e-03 1.078678e-03 6.360412e-04 1.989433e-02 1.372018e-02 1.283822e-02 4.898119e-05
#[23] 1.468505e-04 1.956559e-04 2.685422e-02 1.568007e-02 9.653684e-03 7.349437e-03 3.527451e-03 1.910068e-03 1.958066e-04 1.959653e-04 1.468266e-04
#[34] 7.849548e-04 3.921067e-04 1.967967e-04 4.904401e-05 1.469099e-04 2.447733e-04 2.441633e-04 9.820257e-05 4.903922e-05 8.526161e-03 7.301092e-03
#[45] 1.749373e-02 1.259392e-02 3.723664e-03 8.526648e-03 4.906314e-05 3.421730e-09 3.870362e-03 2.351313e-03 1.077567e-03 1.470133e-03 6.856222e-04
#[56] 1.959031e-04 7.290961e-02 4.517418e-02 7.869426e-02 4.895022e-05 6.516217e-03 3.331230e-03 2.498574e-03 2.498045e-03 2.743148e-03 1.076204e-03
#[67] 9.306279e-04 1.078705e-03 2.943939e-04 1.322910e-03 1.469136e-03 1.078659e-03 9.703270e-05 1.955478e-04 1.459275e-04 1.225286e-03 1.616848e-03
#[78] 1.176669e-03 3.913957e-04 2.939239e-04 4.906025e-04 4.895592e-05 9.817984e-05 4.900937e-05 4.900360e-05 9.602678e-03 6.957625e-03 1.058562e-02
#[89] 1.962300e-04 4.897985e-05 9.596331e-05 1.468713e-04 1.468172e-04 4.896431e-05 4.900697e-05 9.822632e-05 4.903219e-05 6.676875e-09 2.939885e-04
#[100] 9.805346e-05 1.470058e-04 5.873687e-04 3.432507e-04 8.607854e-02 5.266864e-02 8.070036e-02 1.543463e-02 8.280074e-03 6.320338e-03 1.764176e-03
#[111] 6.866423e-04 6.351475e-04 6.614825e-03 3.869162e-03 2.008668e-03 4.399891e-04 2.939819e-04 3.918200e-04 9.837163e-05 2.943945e-04 9.842110e-05
#[122] 1.812385e-03 7.859372e-04 7.829177e-04 7.340668e-04 4.902878e-04 7.862537e-04 4.900769e-05 4.904285e-05 9.855391e-05 1.961271e-04 7.448222e-08
#[133] 6.870861e-04 4.897035e-04 1.468519e-04 3.331779e-11 1.469186e-04 4.902418e-05 8.377283e-03 3.479824e-03 9.995734e-03 9.819595e-05 1.956757e-04
#[144] 3.432028e-04 4.900258e-05 4.909587e-05 4.408943e-04 4.902487e-05 4.406201e-04 1.468548e-04 1.468542e-04 1.423753e-09 1.959039e-04 3.182764e-10
#[155] 9.842944e-05 7.371765e-12 7.340061e-04 5.893193e-04 2.939727e-04 7.545852e-03 5.929021e-03 5.293062e-03 1.468394e-04 4.904206e-05 4.900675e-05
#[166] 5.553649e-10 9.308831e-04 1.421237e-03 1.911010e-03 1.567736e-03 1.567660e-03 3.624449e-03 1.468640e-04 2.448963e-04 5.394212e-04 4.910414e-05
#[177] 2.939410e-04 4.901249e-05 9.809624e-05 9.837089e-05 9.806561e-05 9.810617e-05 4.905843e-05 9.499239e-08 2.453664e-04 1.469091e-04 2.400354e-03
#[188] 1.027788e-03 3.871083e-03 6.594376e-12 4.898599e-05 3.428164e-04 3.428102e-04 6.865099e-04 4.404119e-04 1.471200e-03

