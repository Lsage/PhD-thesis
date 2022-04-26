#################################
# Chapter 3 thesis Lucas Sage
#################################
# Version March 2022
# THIRD SCRIPT: COUNTERFACTUAL RANDOM REALLOCATIONS
# R

library(haven)
library(dplyr)
library(moments)
library(tidyr)
library(plotly)

# Load the data obtained after stata scripts have been executed:
data<-read_dta("C:/Users/AQRDEDA_L_SAGE000/Documents/Data/dadsEdpFullResultsReg.dta")


# A BIT OF VAR RECODING AND DATA CLEANING:
#===================================================================================
data<-data%>%filter(main_job_gross==1,pcs2!="")
# Take only the observations that were used in the regression:
data<-data[which(complete.cases(data$ffe)==TRUE),]

data$edu1<-ifelse(is.na(data$educ)==TRUE,NA,0)
data$edu2<-ifelse(is.na(data$educ)==TRUE,NA,0)
data$edu3<-ifelse(is.na(data$educ)==TRUE,NA,0)
data$edu4<-ifelse(is.na(data$educ)==TRUE,NA,0)

data[which(data$educ==1),]$edu1<-1
data[which(data$educ==2),]$edu2<-1
data[which(data$educ==3),]$edu3<-1
data[which(data$educ==4),]$edu4<-1

data$ga1<-ifelse(is.na(data$ageCat)==TRUE,NA,0)
data$ga2<-ifelse(is.na(data$ageCat)==TRUE,NA,0)
data$ga3<-ifelse(is.na(data$ageCat)==TRUE,NA,0)
data$ga4<-ifelse(is.na(data$ageCat)==TRUE,NA,0)

data[which(data$ageCat==1),]$ga1<-1
data[which(data$ageCat==2),]$ga2<-1
data[which(data$ageCat==0),]$ga3<-1
data[which(data$ageCat==3),]$ga4<-1
#===================================================================================


data<-read_dta("C:/Users/AQRDEDA_L_SAGE000/Documents/Data/dadsEdpFullResultsReg.dta")

# PRELIMINATORY STEPS FOR RANDOM REALLOCATION:
#===============================================================
# Get rid of certain variables useless:
data<-select(data,-SB,-SECT,-SBR,-s_brut,-pannouv,-nic4)
data<-select(data,-age2,numObs2)

# Select complete cases:
data<-data%>%filter(main_job_gross==1,pcs2!="")

# numeric sex:
data$SX<-as.numeric(data$SX)

# Rename the residual variable automatically named by stata
# but that has a name problematic for R:
names(data)[26]<-"residual"

# Select only the cases that entered in the regression:
data<-data[complete.cases(data$residual),]

# Loop for random reallocation:

# Collect the organizational and occupational FE + their SE 
# in two small data frames:
# FIRMS:
firmEffects<- data%>%
  group_by(clust)%>%
  summarize(mean(ffe),
            mean(ffeSe))

# OCCUPATIONS
occupEffects<- data%>%
  group_by(pcs2)%>%
  summarize(mean(ofe),
            mean(ofeSe))

# Rename variables to harmonize and facilitate append later: 
names (firmEffects)<-c("fRealloc","sffe","seffe")
names (occupEffects)<-c("oRealloc","sofe","seofe")

# Do the same for:
# YEAR:
yearEffects<- data%>%
  group_by(year)%>%
  summarize(sann=mean(ann),
            seann=mean(annSe))

# GENDER inter AGE:
genderAgeEffects<- data%>%
  group_by(genderAge)%>%
  summarize(sGenderAgeCoef=mean(genderAgeCoef),
            seGenderAgeCoef=mean(genderAgeCoefSe))

# AGE:
ageEffects<- data%>%
  group_by(ageCat)%>%
  summarize(sAgeCoef=mean(ageCoef),
            seAgeCoef=mean(ageCoefSe))

# The idea is that depending in which occup/orga the indivi
# will be reallocated, I will draw the correspond FE
# in order to compute the new wage

# Create vectors with the probability of each cluster of firms 
# and another with proba occup
firmProba <- data %>%
  group_by(clust)%>%
  summarize(size=n(), 
            obs = nrow(data))

firmProba <- firmProba%>%
  mutate(proba=size/obs)

firmProba<- firmProba%>%
  select(clust,proba)

# Same for occupations:
occupProba <- data %>%
  group_by(pcs2)%>%
  summarize(size=n(), obs = nrow(data))

occupProba$proba <- occupProba$size/occupProba$obs

occupProba<- occupProba%>%
  select(pcs2,proba)

# Put this information in the small datasets containing FE & SE:
# FIRMS:
firmEffects<-data.frame(firmEffects,firmProba)
firmEffects<- firmEffects%>%
  select(-clust)

# OCCUPATIONS:
occupEffects<-data.frame(occupEffects,occupProba)
occupEffects<- occupEffects%>%
  select(-pcs2)
rm(firmProba, occupProba)

# Determine the bounds of the confidence interval 95%:
firmEffects<-firmEffects%>% mutate(lower=sffe-1.96*seffe,
                      uper=sffe+1.96*seffe)

occupEffects<-occupEffects%>% mutate(lower=sofe-1.96*seofe,
                                   uper=sofe+1.96*seofe)

yearEffects<-yearEffects%>% mutate(lower=sann-1.96*seann,
                                    uper=sann+1.96*seann)
  
genderAgeEffects<-genderAgeEffects%>% mutate(lower=sGenderAgeCoef-1.96*seGenderAgeCoef,
                                        uper=sGenderAgeCoef+1.96*seGenderAgeCoef)

ageEffects<-ageEffects%>% mutate(lower=sAgeCoef-1.96*seAgeCoef,
                                       uper=sAgeCoef+1.96*seAgeCoef)

# Add a colum numbering rows
firmEffects$n<-1:24
occupEffects$n<-1:24
yearEffects$n<-1:6
ageEffects$n<-1:4
genderAgeEffects$n<-1:4

# Generate the worker FE variable that were not computed in stata reghdfe
# (I have also checked with another stata command felsdvreg that they were equivalent)
data<-data%>%
  mutate(wfe=y1-(ann+genderAgeCoef+ageCoef+ffe+ofe+residual))

#Create a worker Type variable:
data$worTyp<-paste(as.character(data$ageCat),
                   as.character((data$SX+1)),
                   as.character(data$educ),
                   sep="")

data[which(is.na(data$educ)==TRUE),]$worTyp<-NA

varNames<-c("mIntercept","mEdu2","mEdu3","mEdu4",
            "mSx1","mAge1","mAge2","mAge3",
            "vIntercept","vEdu2","vEdu3","vEdu4",
            "vSx1","vAge1","vAge2","vAge3")
#===============================================================

# RANDOM REALLOCATION LOOP:
#===============================================================
# Determine how many times you want to do the reallocation exercise:
numberReallocations<-200

# Prepare dataframes for storing results:
storeStats<-c()
storeStats<-as.data.frame(storeStats)
storeStats2<-c()
storeStats2<-as.data.frame(storeStats2)

# LOOP really starts:
for (i in 1:numberReallocations){ # repeat number of times wanted:
  l<-c()
  # First draw the value of the OFE and FFE sampled with the standard error
  # We do a 95% confidence interval uniformly distributed (conservative)
  for (m in unique(firmEffects$n)){
    # Go in each row of the dataset containing firm FE, SE and proba
    # Take each line:
    small<-firmEffects[firmEffects$n==m,]
    # Sample a value between limits:
    valueSampled<-runif(1,small$lower,small$uper)
    # put it in the vector l
    l<-c(l,valueSampled)
  }
  # Add the vector of sampled values of FFE in the firmEffects data frame
  firmEffects$ffeSampled<-l
  
  # Clean the l vector:
  l<-c()
  
  # Do the same for Occupational FE
  for (m in unique(occupEffects$n)){
    small<-occupEffects[occupEffects$n==m,]
    valueSampled<-runif(1,small$lower,small$uper)
    l<-c(l,valueSampled)
  }
  # Add it to the occupEffects dataframe:
  occupEffects$ofeSampled<-l
  
  # Same for year:
  l<-c()
  for (m in unique(yearEffects$n)){
    small<-yearEffects[yearEffects$n==m,]
    valueSampled<-runif(1,small$lower,small$uper)
    l<-c(l,valueSampled)
  }
  yearEffects$annSampled<-l
  a1<-yearEffects[,c(1,7)]
  
  # Same for age:
  l<-c()
  for (m in unique(ageEffects$n)){
    small<-ageEffects[ageEffects$n==m,]
    valueSampled<-runif(1,small$lower,small$uper)
    l<-c(l,valueSampled)
  }
  ageEffects$ageSampled<-l
  a2<-ageEffects[,c(1,7)]
  
  # Same for Gender inter Age
  l<-c()
  for (m in unique(genderAgeEffects$n)){
    small<-genderAgeEffects[genderAgeEffects$n==m,]
    valueSampled<-runif(1,small$lower,small$uper)
    l<-c(l,valueSampled)
  }
  genderAgeEffects$genderAgeSampled<-l
  a3<-genderAgeEffects[,c(1,7)]
  
  # Clean:
  rm(l,m,valueSampled,small)
  
  # Create a smaller dataset with less variables:
  small<-data%>%
    select(wid,SX,educ,ageCat,worTyp,genderAge,clust,y1,pcs2,year,
           ofe,wfe,ffe,residual,year,ageCat)#plus the SE 
  
  small<-merge(small,a1, by='year')
  small<-merge(small,a2, by='ageCat')
  small<-merge(small,a3, by='genderAge')

  # Take the vector of individuals:
  artifData<-data.frame(unique(small$wid))
  names(artifData)<-c("wid")
  
  # Draw a value in the new dataframe with sampled FFE and OFE
  # depending on the vector of probabilities (which is proport  
  # to each ocup/orga's respective size) => one for each indiv:
  simData<-firmEffects[sample(1:nrow(occupEffects),nrow(artifData),
                              prob=firmEffects$proba,replace=TRUE),]
  simData2<-occupEffects[sample(1:nrow(occupEffects),nrow(artifData),
                                prob=occupEffects$proba,replace=TRUE),]
  
  # Now attribute these sampled OFE and FFE to individuals:
  artifData<-data.frame(artifData,simData,simData2)
  artifData<-artifData%>%select(wid,fRealloc,ffeSampled,oRealloc,ofeSampled)
  
  # Merge it with the original dataset (with more than one obs per individual):
  small<-merge(small,artifData,by="wid")
  
  # Compute the three counterfactual wage distributions:
  small<-small%>%
    mutate(sy1=residual+wfe+ageSampled+genderAgeSampled+annSampled+ofe+ffeSampled,
           sy2=residual+wfe+ageSampled+genderAgeSampled+annSampled+ofeSampled+ffe,
           sy3=residual+wfe+ageSampled+genderAgeSampled+annSampled+ofeSampled+ffeSampled)
  
  # Compute the summary statistics on these distributions:
  # Overall variance:
  stat2<-var(small$sy1)
  stat2<-as.data.frame(stat2)
  names(stat2)<-"varSim1"
  stat2$varSim2=var(small$sy2)
  stat2$varSim3=var(small$sy3)
  # Overall mean:
  stat2$averageWage1=mean(small$sy1)
  stat2$averageWage2=mean(small$sy2)
  stat2$averageWage3=mean(small$sy3)
  # Add a column numbering iterations:
  stat2$ID<-i
  
  # Obtain VFR coefficients
  # (not standard errors because I do it differently here:)
  # First counterfactual distribution:
  reg11<- lm(sy1~as.factor(educ)+
              as.factor(SX)+
              as.factor(ageCat),
            data=small)
  # Store the squared residual:
  small$res1<-residuals(reg11)*residuals(reg11)
  
  # Gamma log link with squared residual:
  reg12<-glm(res1~as.factor(educ)+
               as.factor(SX)+
               as.factor(ageCat),
             data=small,family = Gamma(link="log"))
  
  coef1<-c(reg11$coefficients,reg12$coefficients)
  coef1<-as.data.frame(t(coef1))
  names(coef1)<-varNames
  rm(reg11,reg12)
  coef1$distribution<-1
  
  #Second counterfactual distribution:
  reg11<- lm(sy2~as.factor(educ)+
               as.factor(SX)+
               as.factor(ageCat),
             data=small)
  small$res1<-residuals(reg11)*residuals(reg11)
  
  reg12<-glm(res1~as.factor(educ)+
               as.factor(SX)+
               as.factor(ageCat),
             data=small,family = Gamma(link="log"))
  
  coef2<-c(reg11$coefficients,reg12$coefficients)
  coef2<-as.data.frame(t(coef2))
  names(coef2)<-varNames
  rm(reg11,reg12)
  coef2$distribution<-2
  
  #Third counterfactual distribution:
  reg11<- lm(sy3~as.factor(educ)+
               as.factor(SX)+
               as.factor(ageCat),
             data=small)
  small$res1<-residuals(reg11)*residuals(reg11)
  
  reg12<-glm(res1~as.factor(educ)+
               as.factor(SX)+
               as.factor(ageCat),
             data=small,family = Gamma(link="log"))
  
  coef3<-c(reg11$coefficients,reg12$coefficients)
  coef3<-as.data.frame(t(coef3))
  names(coef3)<-varNames
  rm(reg11,reg12)
  coef3$distribution<-3
  
  # Get all coefs:
  stat<-rbind(coef1,coef2,coef3)

  storeStats<-rbind(storeStats,stat)
  storeStats2<-rbind(storeStats2,stat2)
  print(i)
}
#===============================================================
# end LOOP
 