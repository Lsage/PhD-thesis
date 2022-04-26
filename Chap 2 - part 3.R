#################################
# Chapter 2 thesis Lucas Sage
#################################
# Version March 2022
# THIRD SCRIPT: COUNTERFACTUAL WAGE TRAJECTORIES
# R

#===================================================================================
library(haven)
library(dplyr)
library(reshape)
library(ggridges)
library(nlme)
library(ggplot2)
library(moments)
library(viridis)
#===================================================================================


# Merge men and women results:
#===================================================================================
men<-read_dta("resultsBaseMen.dta")
women<-read_dta("resultsBaseWom.dta")

data<-men[which(complete.cases(men$education)&
                  complete.cases(men$edu_origine)&
                  complete.cases(men$sex)&
                  complete.cases(men$logTotSalDay1)&
                  complete.cases(men$potExp)&
                  men$potExp<=15&
                  men$principalJob13==1&
                  men$numObs13>3&
                  complete.cases(men$b1est)),]

data2<-women[which(complete.cases(women$education)&
                     complete.cases(women$edu_origine)&
                     complete.cases(women$sex)&
                     complete.cases(women$logTotSalDay1)&
                     complete.cases(women$potExp)&
                     women$potExp<=15&
                     women$principalJob13==1&
                     women$numObs13>3&
                     complete.cases(women$b1est)),]

data<-rbind(data,data2)

rm(data2, men, women)
#===================================================================================


# Counterfactual distributions to draw Figures 2.12 and 2.13
#===================================================================================
#####################
######## MEN ########
#####################
d<-data[which(data$sex==1),]

#Group based prediction
d<-d%>%mutate(predGrp=
                (intBase+intEdu+intOri+intCoh)+
                (sloBase+sloEdu+sloOri+sloCoh)*potentialExp+
                (quadra)*potentialExp*potentialExp)

# group + random terms together:
# be careful: if we just sum it up, we will count the intercept
d<-d %>% mutate(predRand=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2est)*potentialExp+
                  (quadra)*potentialExp*potentialExp)

# reconstruct empirical value (just to check):
d<-d %>% mutate(predEmpi=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2est)*potentialExp+
                  (quadra)*potentialExp*potentialExp+
                  residual)

# Check that it gives exactly the same thing as the empirical wage
d<-d%>%mutate(test=predEmpi-logTotSalDay1)

# store these data and then do the same for the model on women:
d2<-d

#####################
####### WOMEN #######
#####################
d<-data[which(data$sex==0),]

#Group based prediction
d<-d%>%mutate(predGrp=
                (intBase+intEdu+intOri+intCoh)+
                (sloBase+sloEdu+sloOri+sloCoh)*potentialExp+
                (quadra)*potentialExp*potentialExp)

# group + random terms together:
# be careful: if we just sum it up, we will count the intercept
d<-d %>% mutate(predRand=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2est)*potentialExp+
                  (quadra)*potentialExp*potentialExp)

# reconstruct empirical value (just to check):
d<-d %>% mutate(predEmpi=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2est)*potentialExp+
                  (quadra)*potentialExp*potentialExp+
                  residual)

# Check that it gives exactly the same thing as the empirical wage
d<-d%>%mutate(test=predEmpi-logTotSalDay1)

# Put men and women together:
d<-rbind(d2,d)

# Get the three evolutions of variance to create Figures 2.12 and 2.13 in the thesis:
des<-d%>%
  group_by(sex,potExp)%>%
  summarize(varEmpi=var(logTotSalDay1,na.rm=TRUE),
            varGrp=var(predGrp,na.rm=TRUE), # This when group membership only is considered
            varRand=var(predRand,na.rm=TRUE), # Same + rand slope and intercept
            varPredFull=var(predEmpi,na.rm=TRUE)) # Empirical Wage variance

rm(des,des2,des3,des4,descrip,descrip2,desEdu,desEduO,d2)
#===================================================================================


# Counterfactual distributions Figure 2.18
#===================================================================================
#####################
######## MEN ########
#####################
# Create a data set with one row per individual only (first line but this is accessory)
indiv<-d[which(d$sex==1),]%>%
  group_by(id1)%>%
  filter(row_number()==1,)

# We want to manipulate the strength of the correlation between random slope and intercept
# WITHIN groups of education and education of parents:
simLarge<-c() # This will be the final dataframe containing one row per individual:
for (i in unique(d$education)){
  for (j in unique(d$edu_origine)){
    # Select only the i level of education
    # and the j level of education of parents
    # in a temporary dataframe:
    subData<-indiv[which(indiv$education==i &
                           indiv$edu_origine==j),]
    # Based on this, select only certain variable 
    # in another tempo dataframe that will be used
    # to store the simulated data:
    sim<-subData[,c("id1","b1est",
                    "education","edu_origine")]
    
    # Sort in ascending order on the estimated random intercept
    sim<-sim%>%
      arrange(b1est)
    
    # Create a new vector of random slopes sorted in 
    # ascending order
    b2est<-sort(subData$b2est)
    
    # Put this new random slope vector in the simulated tempo dataframe:
    sim$b2simCor1<-b2est
    
    # Do the same thing but in descending order:
    b2est<-sort(b2est,decreasing = TRUE)
    sim$b2simCor_1<-b2est
    
    # Finally random shuffle:
    b2est<-sample(b2est)
    sim$b2simCor0<-b2est
    
    # SUM UP: b2simCor1= maximum cor between rand slope and intercept
    # b2simCor_1= maximum negative cor between rand slope and intercept
    # b2simCor0= no cor between rand slope and intercept
    
    # Put it in the final dataframe containing one row per individual:
    simLarge<-rbind(simLarge,sim)
    
    # Check where we are in the loop:
    print(i)
  }
}

# Store it in another dataframe to be able to rewrite on the first one
# to do the same thing for women:
simLarge2<-simLarge

#####################
####### Women #######
#####################
indiv<-d[which(d$sex==0),]%>%
  group_by(id1)%>%
  filter(row_number()==1,)


# We want to manipulate the strength of the correlation between random slope and intercept
# WITHIN groups of education and education of parents:
simLarge<-c() # This will be the final dataframe containing one row per individual:
for (i in unique(d$education)){
  for (j in unique(d$edu_origine)){
    # Select only the i level of education
    # and the j level of education of parents
    # in a temporary dataframe:
    subData<-indiv[which(indiv$education==i &
                           indiv$edu_origine==j),]
    # Based on this, select only certain variable 
    # in another tempo dataframe that will be used
    # to store the simulated data:
    sim<-subData[,c("id1","b1est",
                    "education","edu_origine")]
    
    # Sort in ascending order on the estimated random intercept
    sim<-sim%>%
      arrange(b1est)
    
    # Create a new vector of random slopes sorted in 
    # ascending order
    b2est<-sort(subData$b2est)
    
    # Put this new random slope vector in the simulated tempo dataframe:
    sim$b2simCor1<-b2est
    
    # Do the same thing but in descending order:
    b2est<-sort(b2est,decreasing = TRUE)
    sim$b2simCor_1<-b2est
    
    # Finally random shuffle:
    b2est<-sample(b2est)
    sim$b2simCor0<-b2est
    
    # SUM UP: b2simCor1= maximum cor between rand slope and intercept
    # b2simCor_1= maximum negative cor between rand slope and intercept
    # b2simCor0= no cor between rand slope and intercept
    
    # Put it in the final dataframe containing one row per individual:
    simLarge<-rbind(simLarge,sim)
    
    # Check where we are in the loop:
    print(i)
  }
}

names(simLarge2)[2]<-"b1estSim"
names(simLarge)[2]<-"b1estSim"
# this should be the same as the original dataframe 
# but I keep it just to check that it is indeed the case

# Put men and women together:
simLarge<-rbind(simLarge,simLarge2)

# Merge the dataset with one observation per individual
# containing the random slopes corresponding to each of the 
# three counterfactual correlations, with the original panel
# dataset:
d<-merge(d,simLarge,by="id1")

rm(simLarge,simLarge2)

# Create new individual wage trajectories 
# according to the simulated parameters

# FIRSTLY when correlation is minimum
# group + random terms together:
# be careful: if we just sum it up, we will count the intercept
d<-d %>% mutate(predRandSim_1=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2simCor_1)*potentialExp+
                  (quadra)*potentialExp*potentialExp)

# Reconstruct the overall Wage variance
d<-d %>% mutate(predEmpiSim_1=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2simCor_1)*potentialExp+
                  (quadra)*potentialExp*potentialExp+
                  residual)

# SECONDLY when correlation is null
d<-d %>% mutate(predRandSim0=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2simCor0)*potentialExp+
                  (quadra)*potentialExp*potentialExp)

# Reconstruct the overall Wage variance
d<-d %>% mutate(predEmpiSim0=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2simCor0)*potentialExp+
                  (quadra)*potentialExp*potentialExp+
                  residual)

# THIRDLY when correlation is maximum
d<-d %>% mutate(predRandSim1=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2simCor1)*potentialExp+
                  (quadra)*potentialExp*potentialExp)

# Reconstruct the overall Wage variance
d<-d %>% mutate(predEmpiSim1=
                  (intBase+intEdu+intOri+intCoh+b1est)+
                  (sloBase+sloEdu+sloOri+sloCoh+b2simCor1)*potentialExp+
                  (quadra)*potentialExp*potentialExp+
                  residual)

# FINALLY compute the new predicted curves based on this:
des<-d%>%
  group_by(sex,potExp)%>%
  summarize(varEmpi=var(logTotSalDay1,na.rm=TRUE),
            varGrp=var(predGrp,na.rm=TRUE),
            varRand=var(predRand,na.rm=TRUE),
            varPredFull=var(predEmpi,na.rm=TRUE),
            varRandSim_1=var(predRandSim_1),
            varEmpiSim_1=var(predEmpiSim_1),
            varRandSim1=var(predRandSim1),
            varEmpiSim1=var(predEmpiSim1),
            varRandSim0=var(predRandSim0),
            varEmpiSim0=var(predEmpiSim0))

# This is the dataframe used to create Figure 2.18 in the thesis 
#===================================================================================
