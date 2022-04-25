*****************************************
** Chapter 2 thesis Lucas Sage **********
*****************************************
* Version March 2022
* FIRST SCRIPT: DATA CLEANING 

cd "C:\Users\AQRDEDA_L_SAGE000\Documents\Growth curve chapter"

***********************
use dadsEdpFull,clear
***********************

rename (an nninouv SIR)(year id f1)

sort id year

********************************************************************************
* Delete variables that are already contained in AFER (année fin d'étude)
drop AFE68 AFE75 AFE82

* Delete some of the variables that I won't use
drop entpan arriv DOMEMPL TaiN dip_tot_anc

drop SBR SB


***************************
save dadsEdpFull, replace
***************************


* Data cleaning:
*==========================================================================
***************************
use dadsEdpFull, clear
***************************

* Delete people with too much experience
drop if xp >= 21

* Gen new firm and id identifiers
egen id1=group(id)
egen siren=group(f1)

* Sort
order id1 year siren

* Drop cases where missing year or id
drop if id1==.
* I am not deleting if the siren is missing
* I prefer to delete if wage is missing instead  
drop if year==.

* I keep the old siren in case I need matching but drop old id
drop id
********************************************************************************

********************************************************************************
* Work on the dependent variable:
* Predict missing or strange values on the dep variable using another net wage:
* The two variables are extremely similar, I predict one on the basis 
* of the other with a regression. Best model:
reg NETNETR SNR c.SNR#c.SNR c.SNR#c.SNR#c.SNR c.SNR#c.SNR#c.SNR#c.SNR
* predict predSal
* Of course this worked only on the cases with non missing values for the 
* NETNETR variable 
* the equation is:
* y=b0 + b1X + b2X^2 + b3X^3 + b4X^4

gen predSal=.
* replace predSal= 
* use the coefs found previously

* gen an indicator saying whether the value has been imputed or not to check
* later whether results are dependent on this imputation
gen imputed=.
replace imputed=1 if missing(NETNETR) & !missing(predSal)

* Impute 
replace NETNETR=predSal if missing(NETNETR) & !missing(predSal)
drop if missing(NETNETR)
********************************************************************************

********************************************************************************
* Some variables are strings and should not be:
destring dip_tot, replace
destring dip_tot_mere, replace
destring dip_tot_pere, replace
destring cs_mere, replace
destring cs_pere, replace
destring SX, replace
destring filtre, replace


sort id1 year siren
********************************************************************************

********************************************************************************
* Drop cases with missing information on a key variable
* But first if the information is contained somwhere in other cases
* impute the value for the rest of the individual
by id1: egen sex=mode(SX)
drop if missing(sex)
drop SX
by id1: egen educ=mode(dip_tot)
drop if missing(educ)

sort id1 year siren
********************************************************************************

********************************************************************************
* CLEAN THE DEPENDENT VARIABLE

sort NETNETR
* Drop negative and missing values of wage
replace NETNETR=predSal if NETNETR<=0 & !missing(NETNETR) & !missing(predSal)
drop if NETNETR <= 0
drop if missing(NETNETR)
 
sort year
by year, sort: egen p999 = pctile(NETNETR),p(99.9)
* To determine whether the high values are outliers I consider that they are not
* only if the same individual appears several times in the top 10 % of the distribution
by year, sort: egen p90 = pctile(NETNETR),p(90)

sort id1 year siren
* Count the number of times each individual is in the top 10%
gen counter=0 
replace counter=1 if NETNETR > p90 
by id1: egen obsQ90=total(counter)

* Detect if some observations are suspect (individuals who have wage in top 0.1%
* but no other observations in top 10%
gen det=.
replace det=1 if NETNETR> p999 & obsQ90 < 2
* In some cases this could be due to individuals with only a few observations
gen changeYear=0
by id1: replace changeYear=1 if year[_n] != year[_n-1]
by id1: gen numObs=sum(changeYear)
* numObs is the cumulative sum counting the number of obs in different years
* ex: if the individual has 2 obs in 2011 and 2 in 2012, numObs ==1 for the 
* 2 first obs and numObs==2 for the two second 

* Now we are going to count how many observation in total the person has
by id1: egen totNumObs=max(numObs)

* And now check whether the suspect observations are for individuals with one obs
* only. 
*=====>DET2 indicates that the obs is suspect only because the pers has one obs
gen det2=.
replace det2=1 if det==1 & totNumObs==1
* the indiv had only one observation in total so they automatically are supect

* Spread that value across all obs of the individual to be able to check them 
* visually easily:
gen det3=.
replace det3=1 if det==1 & det2!=1 
by id1: egen det4= max(det3)
* If you want to see the cases execute the next line:
* sort det4 id1 year siren

* If the value is suspect impute the value of the previous year in the same firm
* if it exists. Take into account the number of days worked
* First detect the previous observation in the same firm
sort id1 year siren
gen firm=siren if det==1 & det2!=1 
gen theYear=year if det==1 & det2!=1 

* Spread the value for all obs of the individual
by id1: egen firm1=mode(firm)
by id1: egen theYear1=mode(theYear)
replace theYear1=theYear1 - 1

* Recode the variable number of days worked: if superior to 360 (which is not 
* possible) = 360
replace DP=360 if DP>360

* Take the salary in the last observation in the same firm (by individual)
gen lastSalSameFirm=.
replace lastSalSameFirm=NETNETR if theYear1==year & siren==firm1
* replace to get the salary/ day
replace lastSalSameFirm=lastSalSameFirm/DP
* spread the value across all obs 
by id1: egen lastSal=max(lastSalSameFirm)
* Create the imputed salary
gen imputedSal=.
replace imputedSal=lastSal*DP if det==1
replace NETNETR=imputedSal if !missing(imputedSal)

* Those who remain are those who were not in the same firm in the previous year
* for them I impute the value of the next year if they are in same firm
* First drop variables and re-run same process for next value imputation
* Spread the value for all obs of the individual
replace theYear1=theYear1 + 2

* Take the salary in the next observation in the same firm (by individual)
gen nextSalSameFirm=.
replace nextSalSameFirm=NETNETR if theYear1==year & siren==firm1 
* replace to get the salary/ day
replace nextSalSameFirm=nextSalSameFirm/DP
* spread the value across all obs 
by id1: egen nextSal=max(nextSalSameFirm)
* Create the imputed salary
gen imputedSal2=.
replace imputedSal2=nextSal*DP if det==1
* To avoid replacing values that have already been imputed (based on the 
* salary in the previous year I need a variable that detects this
by id1: egen alreadyImputed=max(imputedSal)
replace NETNETR=imputedSal2 if !missing(imputedSal2) & missing(alreadyImputed)

* previous or next wage ==> missing value
by id1: egen alreadyImputed2=max(imputedSal2)
* replace in the dep variable
replace NETNETR=. if missing(imputedSal) & missing(imputedSal2) & det==1 & det2!=1
drop if missing(NETNETR)

* Next we check whether there are still some extreme strange values
sort NETNETR id1 year siren
* There are many obs with close to 0 values but I will take the sum of all 
* salaries in the same year, so it is not necessarily a problem
* Top values seem ok because the individuals appear several times on top
sort id1 year siren
********************************************************************************

********************************************************************************
* First there is an issue with some strange values for AGE => delete
drop if AGE < 16
* Detect the first observation of an individual in the panel:
by id1: generate firstObs=_n 
replace firstObs=. if firstObs > 1

* Gen the age the person has at the first observation we have 
* (to be compared with age end of study)
gen ageFirstObs= firstObs * AGE
* give the value of the variable to all obs of the individual
bysort id1(ageFirstObs): replace ageFirstObs = ageFirstObs[1] if missing(ageFirstObs)
********************************************************************************

********************************************************************************
* DROP VARIABLES:
drop alreadyImputed2 alreadyImputed imputedSal2 nextSal nextSalSameFirm ///
imputedSal lastSal lastSalSameFirm theYear1 firm1 theYear firm det4 det3 ///
det2 totNumObs numObs counter changeYear p999 p90 det obsQ90 pannouv 

* Drop individuals for whom we don't observe LM entry
drop if xpFirstObs > 1
* NOTE: it is the actual experience, not the potential experience

* Delete individuals for who the age at the first observation is higher than 35
drop if ageFirstObs>35
* NOTE: I lose some strange cases with 0 xp and age=40 
* This likely came from a problem in the coding of xp (it is imputed 
* by INSEE on the basis of AFER)

* Check whether there are some problem with people too young to be true:
* sort ageFirstObs id1 year
* =>That's OK

sort id1 year siren
********************************************************************************

* DEAL WITH MULTIPLE OBSERVATION PER YEAR:
********************************************************************************
* I will opt for two different solutions, second one is to keep only the higher
* wage value, the first one is to keep the sum of all wages in the same year
gen detector_several_firms=.
bysort id1 (year): replace detector_several_firms=year[_n] - year[_n-1]
* That variable equals 0 everytime the worker had more than one observation 
* per year 
gen detector=detector_several_firms
replace detector=0 if detector==.

*************************
**** FIRST SOLUTION *****
*************************
* keep the sum of wages and days 
* corresponding to the pay.
bysort id1 year: egen totSal=sum(NETNETR)

* I use the variable filtre which corresponds to non annex job, to create a new
* variable of number of days worked that corresponds only to the number of days
*worked in principal job:
gen dp2=0
replace dp2=DP if !missing(filtre) & filtre==1
bysort id1 year: egen totDaysWorked=sum(dp2) 

* It can be that some individuals have only annex jobs in a given year
* For these take the value of sum of DP
bysort id1 year: egen totDaysWorked2=sum(DP) 
replace totDaysWorked=totDaysWorked2 if totDaysWorked==0
replace totDaysWorked=totDaysWorked2 if missing(totDaysWorked)

* Top code these variables if they have more than 360 days
replace totDaysWorked=360 if totDaysWorked>360
replace totDaysWorked2=360 if totDaysWorked2>360

* Down code them as well: at least 45 days in total
replace totDaysWorked=. if totDaysWorked<45
replace totDaysWorked2=. if totDaysWorked2<45

* SUMMARY: 
* TotDaysWorked is the number of days worked in non annex jobs in a year
* except for the year where someone had nothing else than annex jobs (in this case
* TotDaysWorked2 is the number of days worked in all kind of jobs in a year
* They are down coded: individuals need to have worked at least 45 days a year 

* gen a variable counting the number of jobs per year
bysort id1 year: gen numJobs= _N

* Detect the principal job each year by taking the job in which the Durée de paie
* is the highest (similar to original AKM paper 1999)
* In case the worker works an equal number of days in the two jobs,
* choose the one with the highest salary:
bysort id1 year: egen mostDays=max(DP)

gen principalJob=.
replace principalJob=1 if float(mostDays)==float(DP)
replace principalJob=0 if principalJob==.

* The problem is that if a person had two jobs with the max number of days 
* in the same year=> then it detects two pricipal jobs
* So first we need to detect these cases
bysort id1 year: egen detectorPrincipals=sum(principalJob)
* When this variable is superior to one it means that the person had several
* jobs this year with more than one with an equal number of days worked
* In this case: select the principal job based on the salary
bysort id1 year: egen maxSal=max(NETNETR)

* Objective is to have only one value 1 per year per individual for principal Job
* First put all the problematic cases back to 0
replace principalJob=0 if  detectorPrincipals>1
* And then simply select the job with the best salary in that year
replace principalJob=1 if detectorPrincipals>1 & float(NETNETR) == float(maxSal)

* We check again:
drop detectorPrincipals
bysort id1 year: egen detectorPrincipals=sum(principalJob)
* We have a few observations that are suspect => missing values
replace principalJob=. if principalJob==0 | detectorPrincipals>1
* We do both changes at the same time
* to get the number of changes
gen tempor=.
replace tempor=1 if detectorPrincipals>1
summarize tempor
drop tempor detectorPrincipals
* 1,219 changes

*************************
**** SECOND SOLUTION ****
*************************
* use maxSal together with another detector variable: principalJobMax to get
* the maximum salary:
gen principalJobMax=.
replace principalJobMax=1 if float(maxSal)==float(NETNETR)
* Similarly check whether there are some problematic cases
replace principalJobMax=0 if missing(principalJobMax)
bysort id1 year: egen detectorPrincipals=sum(principalJobMax)
replace principalJobMax=. if principalJobMax==0 | detectorPrincipals>1 



* Finish cleaning dependent variable
* I am taking wage per day as the dependent variable:
gen totSalDay1=totSal / totDaysWorked
gen totSalDay2=totSal / totDaysWorked2
* (240 missing values generated)

* With max wage 
gen maxSalDay=maxSal / DP
* (1,130 missing values generated)

**** NOTE:
* Each of these variables has to be used with the appropriate detector


drop SN SNR detector_several_firms

* I will create new detectors to delete observations with too few values
gen principalJob1=principalJob
gen principalJob2=principalJob
replace principalJob1=. if totSalDay1<1
replace principalJob2=. if totSalDay2<1

gen principalJobMax1=principalJobMax
replace principalJobMax1=. if maxSalDay<1

* Other variable more restrictive:
gen principalJob12=principalJob
gen principalJob22=principalJob
replace principalJob12=. if totSalDay1<5
replace principalJob22=. if totSalDay2<5

gen principalJobMax12=principalJobMax
replace principalJobMax12=. if maxSalDay<5


****NOTE: I Changed this later in the script "VFR growth curve paper" 
* and it becomes more restricted i.e. more small values are excluded 


***********************
save reduced, replace
***********************

********************************************************************************
**** SUMMARY ****
* principalJob1, principalJob2, principalJobMax1, detect Ok values for each
* dependent variable
* principalJob1 -> use with totSalDay1
* principalJob2 -> use with totSalDay2
* principalJobMax1 -> use with maxSalDay
* They are the detectors for which the salaries below 1 euro per day have been
* deleted
********************************************************************************

********************************************************************************

***********************
use reduced, clear
***********************

* GENERATE POTENTIAL EXPERIENCE
********************************************************************************
by id1: generate time=_n
replace time=. if time > 1
* This is the year for which exp=0
gen yearFirstObs=year * time
* Distribute the value to all observations of the same id
bysort id1(yearFirstObs): replace yearFirstObs = yearFirstObs[1] if missing(yearFirstObs)

* I try an alternative where potential experience can not an integer
gen momentStarted=.
replace momentStarted=year - xp if time==1
* Distribute that value to all observations
bysort id1(momentStarted): replace momentStarted = momentStarted[1] if missing(momentStarted)
* Generate potential experience as the year - mm started
gen potentialExp=year - momentStarted

* Time span
by id1: egen minPotExp=min(potentialExp) if principalJob1==1
by id1: egen maxPotExp=max(potentialExp) if principalJob1==1
gen timeSpan= maxPotExp - minPotExp
* Spread the value for all obs of individals
bysort id1: replace timeSpan = timeSpan[_n-1] if missing(timeSpan)
bysort id1: replace timeSpan = timeSpan[_n+1] if missing(timeSpan)

drop minPotExp maxPotExp

* Count number of observations per individual
gen tempo=principalJob1
replace tempo=0 if missing(principalJob1)
by id1: egen numObs1=sum(tempo)
drop tempo

* Other dependent variable
gen tempo=principalJob2
replace tempo=0 if missing(principalJob2)
by id1: egen numObs2=sum(tempo)
drop tempo

* For the other dependent variables (max salary)
gen tempo=principalJobMax1
replace tempo=0 if missing(principalJobMax1)
by id1: egen numObsMaxSal=sum(tempo)
drop tempo

* Select only individuals with observations that have at least a distance of 
* 4 years between them:
drop if timeSpan <5

* Drop useless observation on the basis of all principalJob var
drop if missing(principalJob1) & missing(principalJob2) & missing(principalJobMax1)


* RECODE INDEPENDENT VARIABLES
********************************************************************************
* Recode education into four categories
gen education=.

replace education=1 if dip_tot==1 | ///
dip_tot==2 | dip_tot==3 | ///
dip_tot==4

replace education=2 if dip_tot==5 
replace education=3 if dip_tot==6
replace education=4 if dip_tot==7
* 1 = aucun dip, CEP, BEPC CAP
* 2 = bac techniques et pro + bac general
* 3 = BTS DUT DEUG
* 4 = dip universitaire de 2e ou 3e cycle + grandes ecoles

* Generate salary variables
*gen salaryMax=best_salary / nombre_heures_remunerees
*gen salaryTot=total_salary / total_heures
*gen logSalMax=log(salaryMax)

drop if education==.

* Father
gen educP=.
replace educP=1 if dip_tot_pere==1 | ///
dip_tot_pere==2 | dip_tot_pere==3 | ///
dip_tot_pere==4
replace educP=2 if dip_tot_pere==5 
replace educP=3 if dip_tot_pere==6
replace educP=4 if dip_tot_pere==7

* Mother
gen educM=.
replace educM=1 if dip_tot_mere==1 | ///
dip_tot_mere==2 | dip_tot_mere==3 | ///
dip_tot_mere==4
replace educM=2 if dip_tot_mere==5 
replace educM=3 if dip_tot_mere==6
replace educM=4 if dip_tot_mere==7

* Gen single variable with max educ of both parents
gen edu_origine=.
replace edu_origine=educP if educP > educM
replace edu_origine=educM if educM > educP
replace edu_origine=educM if educM== educP
* Be careful when one of the two is missing:
replace edu_origine=educP if educM==.
replace edu_origine=educM if educP==.
********************************************************************************

* LOG DEP VARIABLES:
********************************************************************************
* Log dependent variables
gen logTotSalDay1=log(totSalDay1)
gen logTotSalDay2=log(totSalDay2)
gen logMaxSalDay=log(maxSalDay)
********************************************************************************

* TIME VARYING INDEPENDENT VARIABLES
********************************************************************************
* Pb with certain categories
* First test use the first digit of CS2 "catégorie socioprofessionnelle" 2 digits
gen c1= substr(CS2,1,1)
* It is exactly the same
drop c1
replace CS1="" if CS1=="Z" | CS1=="9" | CS1=="8" | CS1=="1"
** So there are no farmers in the data (too few so deleted)
** The last problem are the 0s: I try some imputation on the basis of other
* values for the same individual
by id1:egen modePCS= mode(CS1)
replace CS1=modePCS if CS1=="0"
replace CS1="" if CS1=="0"
destring CS1, replace

* Generate a dummy for sector:
gen private=1 
replace private=0 if SECT != "PRIV"
replace private=. if SECT ==""
drop SECT

* Generate log size establishment:
replace NBSA_ET=. if NBSA_ET<=0
* These come only from size==0
gen logSizeEsta=log(NBSA_ET)
drop NBSA_ET

* Generate variable Type of working contract:
gen cdi=0
replace cdi=1 if CONTRAT_TRAVAIL=="01"
drop CONTRAT_TRAVAIL

* Distribute  age at first obs
by id1: egen ageFirstO=min(ageFirstObs)

* Alternative to CDI we use variable condition d'emploi (CE)
gen fullTime=0
replace fullTime=1 if CE=="C"

drop time 
by id1: generate time=_n
replace time=. if time > 1

sort id1 year siren 

* Generate an alternative potential experience variable DISCRETE
gen potExp=floor(potentialExp)

* Generate the cohort variable:
gen cohort=1
replace cohort=2 if yearFirstObs>1990 & yearFirstObs <=2000
replace cohort=3 if yearFirstObs>2000 & yearFirstObs <=2005
replace cohort=4 if yearFirstObs>2005 

* Create dummies for education, education of parents and cohort:
tab education, gen(edu)
tab edu_origine,gen(eduO)
tab cohort,gen(c)
********************************************************************************
********************************************************************************

* DROP USELESS VARIABLES:
********************************************************************************
drop  numObs1 numObs2 numObsMaxSal NETNET NETNET_corrCPSOR aem1 entsir JOUR_EDP ///
NBHEUR cs_mere cs_pere dip_tot_mere dip_tot_pere filtre lieu_naiss lieu_naiss_mere ///
lieu_naiss_pere nat_mere nat_naiss nat_pere NBLIG detector NETNETR ///
principalJob principalJobMax CE modePCS
********************************************************************************


* More restrictive conditions on the detectors: minimum salary of 25 euros a 
* day (which corresponds to about 3 hours at the minimum wage), 
* and 45 days worked in the year. 
********************************************************************************
* Create new detectors for which cases to consider the dependent var
* RECALL:
* principalJob1, principalJob2, principalJobMax1, detect Ok values for each
* dependent variable
* principalJob1 -> use with totSalDay1
* principalJob2 -> use with totSalDay2
* principalJobMax1 -> use with maxSalDay
* They are the detectors for which the salaries below 1 euro per day have been
* deleted
gen principalJob13=principalJob1
gen principalJob23=principalJob2
gen principalJobMax13=principalJobMax1

replace principalJob13=. if totSalDay1<25
replace principalJob23=. if totSalDay2<25
replace principalJobMax13=. if maxSalDay<25

replace principalJob13=. if totDaysWorked < 45
replace principalJob23=. if totDaysWorked2 < 45
********************************************************************************

* DROP SOME MORE USELESS VARIABLES:
********************************************************************************
drop ANNAI dip_tot DP CS2_ANC PCS4 pcs_v2 aen1 aen2 aen3 apet2 APET APEN apen2 ///
predSal totSal   numJobs timeSpan educP educM totSalDay1 totSalDay2 maxSalDay
********************************************************************************

* GENERATE counters of number of observations on the observational window
********************************************************************************
gen tempo=principalJob1
replace tempo=0 if missing(principalJob1)
by id1: egen numObs1=sum(tempo)
drop tempo

* Other dependent variable
gen tempo=principalJob2
replace tempo=0 if missing(principalJob2)
by id1: egen numObs2=sum(tempo)
drop tempo

* For the other dependent variables (max salary)
gen tempo=principalJobMax1
replace tempo=0 if missing(principalJobMax1)
by id1: egen numObsMaxSal=sum(tempo)
drop tempo

gen tempo=principalJob12
replace tempo=0 if missing(principalJob12)
by id1: egen numObs12=sum(tempo)
drop tempo

* Other dependent variable
gen tempo=principalJob22
replace tempo=0 if missing(principalJob22)
by id1: egen numObs22=sum(tempo)
drop tempo

* For the other dependent variables (max salary)
gen tempo=principalJobMax12
replace tempo=0 if missing(principalJobMax12)
by id1: egen numObsMaxSal2=sum(tempo)
drop tempo

gen tempo=principalJob13
replace tempo=0 if missing(principalJob13)
by id1: egen numObs13=sum(tempo)
drop tempo

* Other dependent variable
gen tempo=principalJob23
replace tempo=0 if missing(principalJob23)
by id1: egen numObs23=sum(tempo)
drop tempo

* For the other dependent variables (max salary)
gen tempo=principalJobMax13
replace tempo=0 if missing(principalJobMax13)
by id1: egen numObsMaxSal3=sum(tempo)
drop tempo

* SELECT INDIVIDUALS WITH AT LEAST TWO OBSERVATIONS:
drop if numObs1 < 3 & numObs2 < 3 & numObsMaxSal < 3


/*
SUMMARY:
depending on the dependent variable I use, need to use 
logTotSalDay1 + totSalDay1 + principalJob13    + numObs1
logTotSalDay2 + totSalDay2 + principalJob23    + numObs2
logMaxSalDay  + maxSalDay  + principalJobMax13 + numObsMaxSal
*/
********************************************************************************
