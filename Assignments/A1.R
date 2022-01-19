setwd("~/Desktop/Econometrics/Econ 613/Assignments/A1")
install.packages("janitor")
#========Exercise 1===============
options(digits = 22)

#1:Number of households surveyed in 2007
dhh2007 = read.csv("./Data/dathh2007.csv",colClasses=c("idmen"="character"), header=TRUE)
dhh2007 = subset(dhh2007, select = -c(X))
#check duplicated idmen
length(unique(dhh2007$idmen)) - dim(dhh2007)[1]
str(dhh2007)
nrow(unique(dhh2007["idmen"]))

#2:Number of households with a marital status “Couple with kids” in 2005.
dhh2005 = read.csv("./Data/dathh2005.csv",colClasses=c("idmen"="character"),header=TRUE)
dhh2005 = subset(dhh2005, select = -c(X))
#check duplicated idmen
length(unique(dhh2005$idmen)) - dim(dhh2005)[1]
dhh2005 = dhh2005[!is.na(dhh2005$mstatus) & dhh2005$mstatus=="Couple, with Kids",]
length(unique(dhh2005$idmen))

#3:Number of individuals surveyed in 2008.
dind2008 <- read.csv("./Data/datind2008.csv", colClasses=c("idind"="character","idmen"="character"),header=TRUE)
dind2008 = subset(dind2008, select = -c(X))
#check duplicated idinds
length(unique(dind2008$idind)) - dim(dind2008)[1]
dim(dind2008)[1]

#4:Number of individuals aged between 25 and 35 in 2016.
dind2016 = read.csv("./Data/datind2016.csv",colClasses=c("idind"="character","idmen"="character"),header=TRUE)
dind2016 = subset(dind2016, select = -c(X))
#check duplicated idinds
length(unique(dind2016$idind)) - dim(dind2016)[1]

#If inclusive
dim(dind2016[dind2016["age"]>=25 & dind2016["age"]<=35,])[1]
#If not inclusive
dim(dind2016[dind2016["age"]>25 & dind2016["age"]<35,])[1]


#5 Cross-table gender/profession in 2009
install.packages("crosstable")
library(crosstable)
dind2009 = read.csv("./Data/datind2009.csv",colClasses=c("idind"="character","idmen"="character","profession"="character"),header=TRUE)
dind2009 = subset(dind2009, select = -c(X))
#check duplicated idinds
length(unique(dind2009$idind)) - dim(dind2009)[1]


dind2009$profession = factor(dind2009$profession)
a = crosstable(dind2009,cols="profession",by="gender")
a = a[c("variable","Female","Male")]
colnames(a) = c("Profession","Female","Male")
a = as.data.frame(a)
a


#6 Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, 
#the inter-decile ratio D9/D1 and the Gini coefficient.
dind2005 = read.csv("./Data/datind2005.csv",colClasses=c("idind"="character","idmen"="character"),header=TRUE)
dind2005 = subset(dind2005, select = -c(X))
#check duplicated idinds
length(unique(dind2005$idind)) - dim(dind2005)[1]

dind2019 = read.csv("./Data/datind2019.csv",colClasses=c("idind"="character","idmen"="character"),header=TRUE)
dind2019 = subset(dind2019, select = -c(X))
#check duplicated idinds
length(unique(dind2019$idind)) - dim(dind2019)[1]

#Let's frist filter on the employment status
unique(dind2005$empstat)
unique(dind2019$empstat)
#We should focus on the individuals who are in the labor force, so we pick
#out the indidivudals whose employment status are Employed or Unemployed. 
dind2005 = dind2005[!is.na(dind2005$empstat) & (dind2005$empstat=="Employed" | dind2005$empstat== "Unemployed"),]
dind2019 = dind2019[!is.na(dind2019$empstat) &(dind2019$empstat=="Employed" | dind2019$empstat== "Unemployed"),]
#NA's: There seems to be some NA's in the wage column among people who are in labor force
#Without a consultation with the data collectors it is not clear to me why this is the case
dim(dind2005[is.na(dind2005$wage),])[1]/dim(dind2005)[1]
dim(dind2019[is.na(dind2019$wage),])[1]/dim(dind2019)[1]
#Fortunately, the percentage of NA among labor force participants is very very low, 
#I drop the rows with wage as NA
dind2005 = dind2005[!is.na(dind2005$wage),]
dind2019 = dind2019[!is.na(dind2019$wage),]
#There seem to be a number of people in the labor force, employed but are making 0 wages
#even though the percentage seems to be not trivial. Without better understanding of the data 
#I have to assume that these errors are random and does not affect the overall distribution
#of the wage 
dim(dind2005[(dind2005$empstat=="Employed" & dind2005$wage==0),])[1]
dim(dind2019[(dind2019$empstat=="Employed" & dind2019$wage==0),])[1]
dim(dind2005[(dind2005$empstat=="Employed" & dind2005$wage==0),])[1]/dim(dind2005[(dind2005$empstat=="Employed"),])[1]
dim(dind2019[(dind2019$empstat=="Employed" & dind2019$wage==0),])[1]/dim(dind2019[(dind2019$empstat=="Employed"),])[1]
#I decide to drop these values
dind2005 = dind2005[!(dind2005$empstat=="Employed" & dind2005$wage==0),]
dind2019 = dind2019[!(dind2019$empstat=="Employed" & dind2019$wage==0),]
#There is also a number of people who are unemployed but receive a wage greater than 0
dim(dind2005[(dind2005$empstat=="Unemployed" & !dind2005$wage==0),])[1]
dim(dind2019[(dind2019$empstat=="Unemployed" & !dind2019$wage==0),])[1]
dim(dind2005[(dind2005$empstat=="Unemployed" & !dind2005$wage==0),])[1]/dim(dind2005[(dind2005$empstat=="Unemployed" ),])[1]
dim(dind2019[(dind2019$empstat=="Unemployed" & !dind2019$wage==0),])[1]/dim(dind2019[(dind2019$empstat=="Unemployed"),])[1]
#I decide to keep these value as they might correspond to some welfare payment or insurance payment and they are
#a considerable portion of our data points.

#wage for the labor force participants
library(crosstable)
as.data.frame(crosstable(dind2005,cols="wage"))
as.data.frame(crosstable(dind2019,cols="wage"))
#inter-decile ratio

deciles05 = unname(quantile(dind2005$wage, probs = seq(.1, .9, by = .8)))
d9d1_05 = deciles05[2]/deciles05[1]
deciles19 = unname(quantile(dind2019$wage, probs = seq(.1, .9, by = .8)))
d9d1_19 = deciles19[2]/deciles19[1]

#Gini Coefficient
quantile05= quantile(dind2005$wage, probs = seq(0, 0.999, by = .001))
quantile19= quantile(dind2019$wage, probs = seq(0, 0.999, by = .001))

total_inc05 = length(quantile05)*mean(dind2005$wage)
total_inc19 = length(quantile19)*mean(dind2019$wage)

cum_inc_share_c05 = cumsum(quantile05) / total_inc05
cum_inc_share_c19 = cumsum(quantile19) / total_inc19

Gini05 = (0.5-sum(cum_inc_share_c05)/length(quantile05))/0.5
Gini19 =(0.5- sum(cum_inc_share_c19)/length(quantile19))/0.5
#Distribution of age in 2010. Plot an histogram. 
#Is there any difference between men and women?
library(dplyr)
library(ggplot2)
#Taken together
dind2010 = read.csv("./Data/datind2010.csv",colClasses=c("idind"="character","idmen"="character"),header=TRUE)
dind2010 = subset(dind2010, select = -c(X))
#check duplicated idinds
length(unique(dind2010$idind)) - dim(dind2010)[1]

as.data.frame(crosstable(dind2010,col="age"))
hist(dind2010$age,xlab = "Age", ylab="Count",main = "")

#Comparing by gender
as.data.frame(crosstable(dind2010,col="age",by="gender"))
ggplot(dind2010, aes(x = age)) +
  geom_histogram(aes(color = gender), fill = "white",
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) 
#Female age distribution seem to have a heavier weight on the more elderly 
#end of the spectrum


#8:Number of individuals in Paris in 2011.
#We have to merge data sets to answer this question
dhh2011 = read.csv("./Data/dathh2011.csv",colClasses=c("idmen"="character"),header=TRUE)
#check duplicated idmen
length(unique(dhh2011$idmen)) - dim(dhh2011)[1]

dind2011 = read.csv("./Data/datind2011.csv",colClasses=c("idind"="character","idmen"="character"),header=TRUE)
#check duplicated idinds
length(unique(dind2011$idind)) - dim(dind2011)[1]

dhh2011 = subset(dhh2011, select = -c(X))
dind2011 = subset(dind2011, select = -c(X))


data2011  = dhh2011 %>% full_join(dind2011,by=c("idmen","year"))
data2011 = data2011[!is.na(data2011$location) & data2011$location=="Paris",]

length(unique(data2011$idind))


#========Exercise 2===============
rm(list = ls())
setwd("~/Desktop/Econometrics/Econ 613/Assignments/A1")
library(dplyr)
library(ggplot2)

#==part1==


#1
#Read all individual datasets from 2004 to 2019. Append all these datasets
individualFiles = list.files(path = "./Data",
           pattern="dati", 
           full.names = T) 

datInd = read.csv(individualFiles[1],colClasses=c("idind"="character","idmen"="character"), header=TRUE)
numFile = length(individualFiles)
for (i in seq(2,numFile,1)){
  a = read.csv(individualFiles[i],colClasses=c("idind"="character","idmen"="character"), header=TRUE)
  if(typeof(a$profession)=="integer"){
    a$profession = as.character(a$profession)
  }
  datInd = rbind(datInd, a)
}
datInd = subset(datInd, select = -c(X))



#2
#Read all household datasets from 2004 to 2019. Append all these datasets.
householdFiles = list.files(path = "./Data",
                             pattern="dath", 
                             full.names = T) 

datH = read.csv(householdFiles[1],colClasses=c("idmen"="character"), header=TRUE)
numFile = length(householdFiles)
for (i in seq(2,numFile,1)){
  a = read.csv(householdFiles[i],colClasses=c("idmen"="character"), header=TRUE)
  datH = rbind(datH, a)
}
datH = subset(datH, select = -c(X))


#3
#List the variables that are simultaneously present in the individual and household datasets.
intersect(colnames(datH),colnames(datInd))


#4
#Merge the appended individual and household datasets.
data  = datH %>% full_join(datInd,by = c("idmen","year"))

dataCheck = as.data.frame(data %>% group_by(year,idind) %>% dplyr::summarise(repetition = (n()>1)))
dim(dataCheck[dataCheck$repetition==TRUE,])[1]
#There are only 32 duplicated values. Given the large sample size,
data = data %>% full_join(dataCheck, by=c("idind","year"))
data[data$repetition==TRUE,] 
#It appears that this might have something to do with divorcing so childrens were registerd in both households
#I drop all the households that are associated with the repeated idinds
HhToDelete = unique(data[data$repetition==TRUE,] $idmen)
for (i in HhToDelete){
  data = data[!data$idmen == i,]
}
dim(dataCheck)[1]-dim(data)[1]
#599 observations were deleted from out sample of 413472. I judge this to be a minor adjustment 

#==part2==


#5 
#Number of households in which there are more than four family members
#Because this is a longitudinal dataset, family sizes may change. There are cases
#where family size change from 4 people to 5 people, moving pass the threshold
#an example would be family with idmen = 1200896118640100 in year 2004 and 2005
#There are also cases where a family went from 5 people to 4 people, moving below the 
#threshold, an example would be family with idmen = 1201686017070100
#For consistency, I report this count for each year.
memberCounts = data %>% group_by(idmen,year) %>% dplyr::summarise(Freq = n_distinct(idind))
memberCountsHighFreq = memberCounts[memberCounts$Freq>4,]
length(unique(memberCountsHighFreq$idmen))
numHh4MemByYr = as.data.frame(memberCountsHighFreq %>% group_by(year) %>% dplyr::summarise(numHH = n_distinct(idmen)))
numHh4MemByYr


#6
#Number of households in which at least one member is unemployed
#We take Unployment as defined in a strict sense, not including the 
#individuals who are retired or inactive.
#This is, again, a dynamic process, for example, family with idmen=1202682114470100
#has no unemployed individual in 2004 and 1 unemployed individual in 2005
#We find the number of families that have at least one unemployed
#in each year
dataUnemp = data[!is.na(data$empstat) & data$empstat=="Unemployed",]
countsUnemp = as.data.frame(dataUnemp %>% group_by(year) %>% dplyr::summarise(Unemp = n_distinct(idmen)))
countsUnemp

#7
#Number of households in which at least two members are of the same profession
str(data)

#Similarly to the two problems above,
#We find the number of families in which at least two members are of the same profession
#in each year

unique(data$profession)
#There are many people with blank profession variable. I drop it
dataPro = data[!is.na(data$profession) & (!data$profession==" " & !data$profession=="") ,]
unique(dataPro$profession)
length(unique(dataPro$profession))
#Here is the solution for number household that have at one point has at least two members of the same
#profession
countsPro = as.data.frame(dataPro %>% group_by(idmen,year) %>% dplyr::summarise(diffProf = (n()==n_distinct(profession))))
countsPro2 = as.data.frame(countsPro[!countsPro$diffProf,] %>% group_by(year) %>% dplyr::summarise(numHh = n_distinct(idmen)))
countsPro2

#8
#Number of individuals in the panel that are from household-Couple with kids
dataKids = data[!is.na(data$mstatus) & data$mstatus=="Couple, with Kids" ,]

#For this question, it only makes sense to talk about this for each year
numIndByYr = as.data.frame(dataKids %>% group_by(year) %>% dplyr::summarise(numInd = n_distinct(idind)))
numIndByYr

#9
#Number of individuals in the panel that are from Paris.
dataParis = data[!is.na(data$location) & data$location=="Paris",]
#We do this for each year
numIndParisByYr = as.data.frame(dataParis %>% group_by(year) %>% dplyr::summarise(numInd = n_distinct(idind)))
numIndParisByYr

#10
#Find the household with the most number of family members. Report its idmen.
dataHhMem = as.data.frame(data %>%  group_by(year,idmen) %>% dplyr::summarise(numInd = n_distinct(idind),.groups = 'drop'))
max(dataHhMem$numInd)
dataHhMem[dataHhMem$numInd==max(dataHhMem$numInd),]$idmen
unique(data[data$idmen==dataHhMem[dataHhMem$numInd==max(dataHhMem$numInd),]$idmen[1],]$year)
unique(data[data$idmen==dataHhMem[dataHhMem$numInd==max(dataHhMem$numInd),]$idmen[2],]$year)
data[data$idmen==dataHhMem[dataHhMem$numInd==max(dataHhMem$numInd),]$idmen[2],]
#There are two such households, house A with idmen 2207811124040100 in year 2007 
#and house B with idmen 2510263102990100 in year 2010. In later surveys of household B,
#The number of members drop to 8. household A is only surveyed once.


#11
#Number of households present in 2010 and 2011.

data1011 = data[!is.na(data$year) & (data$year==2010 |data$year==2011 ),]
#I will show
#Number of household that were present in either 2010 or 2011
length(unique(data1011$idmen))
#Number of household that were present in both 2010 or 2011
length(intersect(data1011[data1011$year==2010,]$idmen,data1011[data1011$year==2011,]$idmen))
#Number of household that were present in each year
HhYr1011 = as.data.frame(data1011 %>% group_by(year) %>% dplyr::summarise(numHh = n_distinct(idmen)))
HhYr1011

#clean our dataset for the use of exercies 3
colnames(data)

data = subset(data, select = -c(repetition))


#========Exercise 3===============


#1
#Find out the year each household enters and exit the panel. 
#Report the distribution of the time spent in the survey for each household
library(crosstable)
dataQ3.1 = data 
dataHhYearSpent = as.data.frame(dataQ3.1 %>% group_by(idmen) %>% dplyr::summarise(YearsInservey=n_distinct(year)))
dataQ3.1 = dataQ3.1  %>% full_join(dataHhYearSpent,by=c("idmen"))
YearInSurveryDist = as.data.frame(crosstable(dataQ3.1,cols="YearsInservey"))
YearInSurveryDist = subset(YearInSurveryDist,select=c("label","variable","value"))
YearInSurveryDist


#2
#Based on datent, 
#identify whether or not a household moved into its current dwelling at the year of survey. 
#Report the first 10 rows of your result and plot the share of individuals in that situation across years.
dataQ3.2 = data
dim(dataQ3.2[is.na(dataQ3.2$datent),])[1]
dataQ3.2[is.na(dataQ3.2$datent),]$datent = 0 
#There are some NA in the datent entry, we turn them to 0, so we know they were NA's and 
#they will not match any year.
dataQ3.2$movedInThisYr  = as.data.frame(dataQ3.2  %>% dplyr::summarise(movedInThisYr = (year==datent)))$movedInThisYr
dataQ3.2$movedInThisYr = as.integer(dataQ3.2$movedInThisYr)
#A boolean indicator of whether or not the household moved into its current dwelling at the year of survey. 
#is stored in the movedInThisYr variable, with 1 representing TRUE and 0 for FALSE.
dataMvYrSurHh = as.data.frame(dataQ3.2 %>% group_by(idmen,year) %>% dplyr::summarise(movedInThisYr = (year==datent)))
dataMvYrSurHh = dataMvYrSurHh[!duplicated(dataMvYrSurHh),]
head(dataMvYrSurHh,n=10)
movedInRateByYear = as.data.frame(dataQ3.2 %>% group_by(year) %>% dplyr::summarise(shareMovedThisYear= (sum(movedInThisYr)/n())))

plot(movedInRateByYear$year,movedInRateByYear$shareMovedThisYear,type="b", xlab="Year", ylab="Share of Inviduals in the Situation")

dataMvYrSurHh = as.data.frame(dataQ3.2 %>% group_by(idmen,year) %>% dplyr::summarise(movedInThisYr = (year==datent)))

#3
#Based on myear and move, identify whether or not household migrated at the year of survey. 
#Report the first 10 rows of your result and plot the share of individuals in that situation across years.
dataQ3.3 = data
unique(dataQ3.3[dataQ3.3$year>2014,]$myear)
unique(dataQ3.3[dataQ3.3$year<=2014,]$myear)
unique(dataQ3.3[dataQ3.3$year<2014,]$move)
unique(dataQ3.3[dataQ3.3$year<=2014,]$move)
dim(dataQ3.3[is.na(dataQ3.3$myear)&is.na(dataQ3.3$move),])[1]
dim(dataQ3.3[is.na(dataQ3.3$move),])[1]
dim(dataQ3.3[is.na(dataQ3.3$myear),])[1]
#There will still be many NA's 
#We use myear to fill in move before 2015
dataQ3.3[!is.na(dataQ3.3$myear) & dataQ3.3$myear==dataQ3.3$year,]$move = 2
dataQ3.3[!is.na(dataQ3.3$myear) & !dataQ3.3$myear==dataQ3.3$year,]$move = 1


table(dataQ3.3$move,useNA="ifany")
dataQ3.3 = dataQ3.3[!is.na(dataQ3.3$move),]
dataQ3.3[dataQ3.3$move==1,]$move = 0
dataQ3.3[dataQ3.3$move==2,]$move = 1

dataMvYrSurHh2 = dataQ3.3 %>% group_by(idmen,year)  %>% dplyr::summarise(movedInThisYr = (move==1))
dataMvYrSurHh2 = dataMvYrSurHh2[!duplicated(dataMvYrSurHh2),]
head(dataMvYrSurHh2,n=10)
migratedRateByYear = as.data.frame(dataQ3.3 %>% group_by(year) %>% dplyr::summarise(shareMovedThisYear= (sum(move)/n())))
plot(migratedRateByYear$year,migratedRateByYear$shareMovedThisYear,type="b",xlab="Year", ylab="Share of Inviduals in the Situation")
#4
#Mix the two plots you created above in one graph, 
#clearly label the graph. 
#Do you prefer one method over the other? Justify.

movedInRateByYear$shareWithMyear = migratedRateByYear$shareMovedThisYear
names(movedInRateByYear) = c("Year", "using_datent", "using_myear_move")
library(ggplot2)
library(reshape2)
plot.m = melt(movedInRateByYear, id.vars ="Year", measure.vars = c( "using_datent", "using_myear_move"))
ggplot(plot.m, aes(Year, value, colour = variable)) + geom_point() + geom_line() + ylim(0,0.1) +  ylab("Share of Movers")+ 
  scale_color_discrete(labels = c("using datent","using myear and move"))+scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#I prefer using datent. The two plots are consistent with one another until
#year 2015 because there are too many NA values by using the 
#myear and move variable past year 2014. 


#5
#For households who migrate,find out how many households had at least one family member 
#changed his/her profession or employment status.
#I keep the blank profession variable to account for possibility of moving into employment 
library(dplyr)
dataQ3.5 = dataQ3.2
#I drop the individuals with NA for both employment status and profession
dataQ3.5 = dataQ3.5[!(is.na(dataQ3.5$profession) & is.na(dataQ3.5$empstat)),]
#I count people who changed employment status and profession separately 
familiesMigrated = dataQ3.5 %>% group_by(idmen) %>% dplyr::summarise(onceMoved = (sum(movedInThisYr)>0))
familiesMigrated = familiesMigrated[familiesMigrated$onceMoved==TRUE,]
dataMigrated = dataQ3.5[dataQ3.5$idmen %in% familiesMigrated$idmen, ]




for (i in seq(2005,2019,1)){
  #Count the number of households who migrated in year i that were also surveyed the year before 
  temp1 = dataMigrated[dataMigrated$year==(i-1),]
  temp2 = dataMigrated[(dataMigrated$movedInThisYr==1 & dataMigrated$year==i),]
  alsoSurveyed = length(intersect(temp1$idmen,temp2$idmen))
  part0 = paste(alsoSurveyed,"Households were also surved last year")
  
  #Identify the number of households that migrated
  dataTemp = dataMigrated[(dataMigrated$movedInThisYr==1 & dataMigrated$year==i) | dataMigrated$year==i-1 , ]
  numFamilyMigrated = length(unique(dataTemp[dataTemp$year==i,]$idmen))
  part1 = paste("In year",i)
  part2 = paste(numFamilyMigrated, "families migrated")
  print(paste(part1,part2))
  print(part0)
  #Identify the households with at least one member change career
  IndiTemp1 = as.data.frame(dataTemp %>% group_by(idind) %>% dplyr::summarize(ChangedCareer = (1 < n_distinct(profession))))
  IndiTemp1 = IndiTemp[IndiTemp$ChangedCareer==TRUE,]
  
  #Identify the households with at least one member change employment status
  IndiTemp2 = as.data.frame(dataTemp %>% group_by(idind) %>% dplyr::summarize(ChangedEmpstat = (1 < n_distinct(empstat))))
  IndiTemp2 = IndiTemp2[IndiTemp2$ChangedEmpstat==TRUE,]
  #Combine the two results
  familyCount = dataTemp[dataTemp$idind %in% IndiTemp$idind | dataTemp$idind %in% IndiTemp2$idind,]
  numChangedCareer = length(unique(familyCount$idmen))
  print(paste(numChangedCareer, " of them had at least one member changed career or employment status"))
}





#========Exercise 4===============
#Compute the attrition across each year, 
#where attrition is defined as the reduction in the number of individuals staying in the data panel. 
#Report your final result as a table in proportions.
for (i in seq(2005,2019,1)){
  temp1 = data[data$year==(i-1),]
  temp2 = data[data$year==(i),]
  num1 = length(unique(temp1$idind))
  num2 = length(unique(temp2$idind))
  stayerNum = length(intersect(temp1$idind,temp2$idind))
  newComer = num2 - stayerNum
  part0 = paste("number surveyed in",i)
  part1 = paste(part0,num2)
  part2 = paste("stayers are", stayerNum)
  part3 = paste("new comers are",newComer )
  print(part1)
  print(part2)
  print(part3)
}
#calculate attritions
for (i in seq(2004,2018,1)){
  temp1 = data[data$year==(i),]
  temp2 = data[data$year==(i+1),]
  num1 = length(unique(temp1$idind))
  stayerNum = length(intersect(temp1$idind,temp2$idind))
  attrition = (num1-stayerNum)/num1
  print(attrition)
}
