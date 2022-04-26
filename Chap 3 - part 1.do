*****************************************
** Chapter 3 thesis Lucas Sage **********
*****************************************
* Version March 2022
* FIRST SCRIPT: DATA CLEANING 
* STATA


********************************************************************************
* Merging of different files and BLM clustering alogrithm have been made in R
********************************************************************************

* PREPARATION:
********************************************************************************
rename nninouv id
rename an year
rename sir f1
sort id year f1 dads
* Generate a new individual id
egen wid=group(id)

* Select on age:
drop if AGE < 18 
drop if AGE > 65
********************************************************************************

* DEPENDENT VARIABLE:
********************************************************************************
* We want to detect the best salary of each individual in a given year
* Immediately work with hourly wage:
gen hourlyWage= S_BRUTR / NBHEUR

* Delete missing or negative wages
drop if hourlyWage==.
drop if hourlyWage<=0

* Delete observations with less than half the minimum hourly wage
* For this I use the non deflated wage variable:
gen wage=s_brut / NBHEUR
drop if wage <9.61 / 2 & year==2015
drop if wage <9.53 / 2 & year==2014
drop if wage <9.43 / 2 & year==2013
drop if wage <9.22 / 2 & year==2012
drop if wage <9.00 / 2 & year==2011
drop if wage <8.86 / 2 & year==2010

* Delete observations that are suspisciously too high (similarly to chapter 2
sort year
by year, sort: egen p999 = pctile(hourlyWage),p(99.9)
* To determine whether the values are outliers I consider that they are not
* only if the same individual appears several times in the top 10 % of the distribution
by year, sort: egen p90 = pctile(hourlyWage),p(90)
sort wid year f1
* Count the number of times each individual is in the top 10%
gen counter=0 
replace counter=1 if hourlyWage > p90 
by wid: egen obsQ90=total(counter)

* Detect if some observations are suspect (individuals who have wage in top 0.1%
* but no other observations in top 10%)
gen det=.
replace det=1 if hourlyWage> p999 & obsQ90 < 2

* In some cases this could be due to individuals with only a few observations
gen changeYear=0
by wid: replace changeYear=1 if year[_n] != year[_n-1]
by wid: gen numObs=sum(changeYear)
* numObs is the cumulative sum counting the number of obs in different years
* ex: if the individual has 2 obs in 2011 and 2 in 2012, 
* numObs ==1 for the 2 first obs and numObs==2 for the two second 

* Now count how many observation in total the person has:
by wid: egen totNumObs=max(numObs)
* And now check whether the suspect observations are for individuals with one obs
* only. 

* DET2 indicates that the obs is suspect only because the pers has only one obs
gen det2=.
replace det2=1 if det==1 & totNumObs==1
* For these obs they are suspect only because the indiv had only one observation
*  in total so they automatically are supect

* Spread that value across all obs of the individual to be able to check them visually easily
gen det3=.
replace det3=1 if det==1 & det2!=1 
by wid: egen det4= max(det3)
* If you want to see the cases:
* sort det4 wid year f1

* If the value is suspect impute the value of the previous year in the same 
* firm if it exists. Take into account the number of days worked.
* First detect the previous observation in the same firm
sort wid year f1
gen firm=f1 if det==1 & det2!=1
gen theYear=year if det==1 & det2!=1

* Spread the value for all obs of the individual
by wid: egen firm1=mode(firm)
by wid: egen theYear1=mode(theYear)
replace theYear1=theYear1 - 1

gen lastSalSameFirm=.
replace lastSalSameFirm=hourlyWage if theYear1==year & f1==firm1
* Spread the value across all obs 
by wid: egen lastSal=max(lastSalSameFirm)
* Create the imputed salary
gen imputedSal=.
replace imputedSal=lastSal if det==1
gen hourlyWageImputed=hourlyWage
replace hourlyWageImputed=imputedSal if !missing(imputedSal)
* Those who remain are those who were not in the same firm in the previous year
* for them I impute the value of the next year if they are in same firm
* First drop variables and re-run same process for next value imputation

* Spread the value for all obs of the individual
replace theYear1=theYear1 + 2
gen nextSalSameFirm=.
replace nextSalSameFirm=hourlyWage if theYear1==year & f1==firm1 

* Spread the value across all obs 
by wid: egen nextSal=max(nextSalSameFirm)
* Create the imputed salary
gen imputedSal2=.
replace imputedSal2=nextSal if det==1
* To avoid replacing values that have already been imputed (based on the 
* salary in the previous year I need a variable that detects this)
by wid: egen alreadyImputed=max(imputedSal)
replace hourlyWageImputed=imputedSal2 if !missing(imputedSal2) & missing(alreadyImputed)

* This leaves 46 values for which I could not impute based on 
* previous or next wage ==> missing value
by wid: egen alreadyImputed2=max(imputedSal2)
replace hourlyWageImputed=. if missing(imputedSal) & missing(imputedSal2) & det==1 & det2!=1

* Next we check whether there are still some extreme strange values
sort hourlyWageImputed wid year f1
* There are many obs with close to 0 values but I will take the sum of all 
* salaries in the same year, so it is not necessarily a problem
* Top values seem ok because the individuals appear several times in the top of
* the distribution making them less suspect.

* Drop useless variables:
drop alreadyImputed2 alreadyImputed imputedSal2 nextSal nextSalSameFirm ///
imputedSal lastSal lastSalSameFirm theYear1 firm1 theYear firm det4 det3 ///
det2 changeYear det obsQ90 counter p90 p999 wage numObs totNumObs

* Drop missing values
drop if hourlyWageImputed==.

* Sort
sort wid year f1
********************************************************************************

* INDEPENDENT VARIABLES:
********************************************************************************
* Exclude PCS that we do not want
* Generate occupations 2 digits based on PCS 4 digits:
gen c1= substr(pcs4,1,1)
gen c2= substr(pcs4,2,1)
destring c1, replace force
destring c2, replace force
gen pcs1= c1 
gen pcs2= (c1 * 10) + c2 

gen pcsOk=.
replace pcsOk=1 if pcs2==21 | pcs2==22 | pcs2==23 | pcs2==31 | pcs2==33 | ///
pcs2==34 | pcs2==35 | pcs2==37 | pcs2==38 | pcs2==42 | pcs2==43 | pcs2==44 | ///
pcs2==45 | pcs2==46 | pcs2==47 | pcs2==48 | pcs2==52 | pcs2==53 | pcs2==54 | ///
pcs2==55 | pcs2==56 | pcs2==62 | pcs2==63 | pcs2==64 | pcs2==65 | pcs2==67 | ///
pcs2==68 | pcs2==69 

replace pcs2= pcs2 * pcsOk

drop c1 c2 hourlyWage

* Drop Some of the PCS:
drop if pcs2=="10" | pcs2=="21" | pcs2=="42" | pcs2=="44" | pcs2=="69"

********************************************************************************
* Select only individuals with at least one observation in two different years
gen detector_several_years=.
bysort wid (year): replace detector_several_years=year[_n] - year[_n-1]

replace detector_several_years=0 if detector_several_years==.
by wid: egen detector=sum(detector_several_years)
order wid year f1 hourlyWageImputed detector_several_years detector
* Detector equals 0 if the individual has 1 obs, 1 if he has 2 obs in 2 diffferent years etc.

* Select the individuals with at least two observations 
drop if detector<1
drop detector PCS4 detector_several_years

********************************************************************************
* Detect the principal job in a year by taking the number of hours worked:
bysort wid year: egen main_job_year=max(NBHEUR)
gen main_job_gross=.
replace main_job_gross=1 if float(NBHEUR)==float(main_job_year)

by wid year: gen numObs=sum(main_job_gross)
by wid year: egen numObs2=max(numObs)
by wid year: egen maxSal=max(hourlyWageImputed)

* For cases where there are more than one salary with the same number of hours 
* worked in the same year, I take the highest salary has the principal job
replace main_job_gross=. if main_job_gross==1 & numObs2>1 & hourlyWageImputed!=maxSal
drop numObs numObs2 maxSal

* For cases where there are still more than one principal job, I take the 
* Job in the previous firm:
by wid year: gen numObs=sum(main_job_gross)
by wid year: egen numObs2=max(numObs)
by wid year: replace main_job_gross=. if numObs2>1 & f1[_n]!=f1[_n-1]
drop  numObs numObs2
drop if main_job_gross!=1

********************************************************************************
* EDUCATION
rename dip_tot diplome_nouvelle_nomenclature
gen education=.
destring diplome_nouvelle_nomenclature, replace force

replace education=1 if diplome_nouvelle_nomenclature==1 | ///
diplome_nouvelle_nomenclature==2 | diplome_nouvelle_nomenclature==3 | ///
diplome_nouvelle_nomenclature==4

replace education=2 if diplome_nouvelle_nomenclature==5 
replace education=3 if diplome_nouvelle_nomenclature==6
replace education=4 if diplome_nouvelle_nomenclature==7

egen educ=max(education), by(wid)
drop education diplome_nouvelle_nomenclature

********************************************************************************
* OTHER VARS
destring AGE, replace force
gen age2= AGE * AGE
gen y1= log(hourlyWageImputed)
destring SX, replace
* Categories of age
gen ageCat=.
replace ageCat=0 if AGE<=54 & AGE>=45
replace ageCat=1 if AGE<=30
replace ageCat=2 if AGE>30 & AGE<45
replace ageCat=3 if AGE>54

* Interaction between AGE and gender
gen genderAge=ageCat * SX
destring pcs2,replace

*******************************
save dadsEdpFullFinal, replace
*******************************

