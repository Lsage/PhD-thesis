*****************************************
** Chapter 2 thesis Lucas Sage **********
*****************************************
* Version March 2022
* FIRST SCRIPT: STATISTICAL MODELS
* STATA
********************************************************************************

* VARIANCE REGRESSION FUNCTION
* Mainly taken from Western and Bloome (2009) Sociological methodology
********************************************************************************
* This is one example, for men, for the first 15 years of potential experience
* These are repeated VFR for different varlues of potential experience:
foreach i of numlist   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15{

* Use the dataset created with the data cleaning script (first part chap 2)
******************
use final, clear
******************

* Keep men only:
drop if sex==0

* Define the independent variables:
local xvari i.education											 ///
		    i.edu_origine										 ///
			i.cohort

* Simple OLS model (weighted by variable poids) (could equally be done without
* results don't change). Use the principalJob13 detector, to select observations
* that match criteria (see script part 1).
regress logTotSalDay1 `xvari' if potExp==`i' & principalJob13==1 [aw=poids]
predict R, r
gen R2=R^2
glm R2 `xvari' if potExp==`i' & principalJob13==1 [aw=poids], family(gamma) link(log) iterate(100)
predict S2 , mu
gen LOGLIK=-.5*(ln(S2)+(R2/S2))
egen LL0 = sum(LOGLIK)
di LL0

gen DLL=1
while DLL > .00001 {
  capture drop R
  quietly: reg logTotSalDay1 `xvari' if potExp==`i' & principalJob13==1 [aw=poids/S2]
  capture drop S2
  capture drop bgi
  predict bgi , xb
  predict R, r
  replace R2= R^2
  est store BETA
  quietly:
  glm R2 `xvari' if potExp==`i' & principalJob13==1 [aw=poids], family(gamma) link(log) iterate(100)
  predict S2, mu
  est store LAMBDA
  replace LOGLIK=-.5*(ln(S2)+(R2/S2))
  egen LLN = sum(LOGLIK)
  di LLN
  replace DLL=LLN-LL0
  replace LL0=LLN
  drop LLN
}
* Put all interesting coefficients in a new table
* load the beta reg coef
est restore BETA
gen b0=_b[:_cons]
gen b1=_b[:2.education]
gen b2=_b[:3.education]
gen b3=_b[:4.education]
gen b4=_b[:2.edu_origine]
gen b5=_b[:3.edu_origine]
gen b6=_b[:4.edu_origine]
gen b8=_b[:2.cohort]
gen b9=_b[:3.cohort]
gen b10=_b[:4.cohort]

* Store the standard errors:
gen bse0=_se[:_cons]
gen bse1=_se[:2.education]
gen bse2=_se[:3.education]
gen bse3=_se[:4.education]
gen bse4=_se[:2.edu_origine]
gen bse5=_se[:3.edu_origine]
gen bse6=_se[:4.edu_origine]
gen bse8=_se[:2.cohort]
gen bse9=_se[:3.cohort]
gen bse10=_se[:4.cohort]

* Load the lambda reg coef:
est restore LAMBDA
gen l0=_b[:_cons]
gen l1=_b[:2.education]
gen l2=_b[:3.education]
gen l3=_b[:4.education]
gen l4=_b[:2.edu_origine]
gen l5=_b[:3.edu_origine]
gen l6=_b[:4.edu_origine]
gen l8=_b[:2.cohort]
gen l9=_b[:3.cohort]
gen l10=_b[:4.cohort]

* Store the standard errors:
gen lse0=_se[:_cons]
gen lse1=_se[:2.education]
gen lse2=_se[:3.education]
gen lse3=_se[:4.education]
gen lse4=_se[:2.edu_origine]
gen lse5=_se[:3.edu_origine]
gen lse6=_se[:4.edu_origine]
gen lse8=_se[:2.cohort]
gen lse9=_se[:3.cohort]
gen lse10=_se[:4.cohort]

* I create one stata dataset of one line for each value of potential experience
* That will be easily opened in R
gen t= _n
gen lvlExp=`i'
gen lvlExp2=`i'
drop if t!=1

keep lvlExp b0 b1 b2 b3 b4 b5 b6  b8 b9 b10 bse0 bse1 bse2 bse3 bse4 ///
bse5 bse6  bse8 bse9 bse10 lvlExp2 ///
l0 l1 l2 l3 l4 l5 l6  l8 l9 l10 lse0 lse1 lse2 lse3 lse4 lse5 lse6  ///
lse8 lse9 lse10

order lvlExp b0 b1 b2 b3 b4 b5 b6  b8 b9 b10 bse0 bse1 bse2 bse3 bse4 ///
bse5 bse6  bse8 bse9 bse10 lvlExp2 ///
l0 l1 l2 l3 l4 l5 l6  l8 l9 l10 lse0 lse1 lse2 lse3 lse4 lse5 lse6  ///
lse8 lse9 lse10

save "menpotExp`i'.dta",replace
}
* End of all VFRs 
********************************************************************************


* MULTILEVEL GROWTH CURVE MODEL: 
********************************************************************************
* One example for men without time varying covariates:

* Use the dataset created with the data cleaning script (first part chap 2)
******************
use final, clear
******************

* Model (unstructured):
xtmixed logTotSalDay1 i.education i.edu_origine i.cohort potentialExp ///
c.potentialExp#i.education ///
c.potentialExp#i.edu_origine ///
c.potentialExp#i.cohort ///
c.potentialExp#c.potentialExp ///
|| id1: potentialExp if principalJob13==1 & sex==1 & potExp <= 15 ///
& !missing(education) & !missing(edu_origine) & !missing(sex) ///
& !missing(logTotSalDay1), variance cov(unstr)


* Store the diverse residual elements in the dataset:
predict b2est b1est,reffects
predict residual, residuals
predict b2se b1se, reses

* Manually store all coefficients in the dataset:
* Coef for baseline wage:
matrix b = e(b)
gen intBase=_b[:_cons]
gen intEdu=0
replace intEdu=_b[:2.education] if education==2
replace intEdu=_b[:3.education] if education==3
replace intEdu=_b[:4.education] if education==4
gen intOri=0
replace intOri=_b[:2.edu_origine] if edu_origine==2
replace intOri=_b[:3.edu_origine] if edu_origine==3
replace intOri=_b[:4.edu_origine] if edu_origine==4
gen intCoh=0
replace intCoh=_b[:2.cohort] if cohort==2
replace intCoh=_b[:3.cohort] if cohort==3
replace intCoh=_b[:4.cohort] if cohort==4

* Coef for Slope:
gen sloBase=_b[:c.potentialExp]
gen sloEdu=0
replace sloEdu=_b[:2.education#c.potentialExp] if education==2
replace sloEdu=_b[:3.education#c.potentialExp] if education==3
replace sloEdu=_b[:4.education#c.potentialExp] if education==4
gen sloOri=0
replace sloOri=_b[:2.edu_origine#c.potentialExp] if edu_origine==2
replace sloOri=_b[:3.edu_origine#c.potentialExp] if edu_origine==3
replace sloOri=_b[:4.edu_origine#c.potentialExp] if edu_origine==4
gen sloCoh=0
replace sloCoh=_b[:2.cohort#c.potentialExp] if cohort==2
replace sloCoh=_b[:3.cohort#c.potentialExp] if cohort==3
replace sloCoh=_b[:4.cohort#c.potentialExp] if cohort==4

* Quadratic term:
gen quadra=_b[:c.potentialExp#c.potentialExp]

* Manually store all standard errors in the dataset:
* SE for baseline wage:
gen intBaseSe=_se[:_cons]
gen intEduSe=0
replace intEduSe=_se[:2.education] if education==2
replace intEduSe=_se[:3.education] if education==3
replace intEduSe=_se[:4.education] if education==4
gen intOriSe=0
replace intOriSe=_se[:2.edu_origine] if edu_origine==2
replace intOriSe=_se[:3.edu_origine] if edu_origine==3
replace intOriSe=_se[:4.edu_origine] if edu_origine==4
gen intCohSe=0
replace intCohSe=_se[:2.cohort] if cohort==2
replace intCohSe=_se[:3.cohort] if cohort==3
replace intCohSe=_se[:4.cohort] if cohort==4

* SE for slope
gen sloBaseSe=_se[:c.potentialExp]
gen sloEduSe=0
replace sloEduSe=_se[:2.education#c.potentialExp] if education==2
replace sloEduSe=_se[:3.education#c.potentialExp] if education==3
replace sloEduSe=_se[:4.education#c.potentialExp] if education==4
gen sloOriSe=0
replace sloOriSe=_se[:2.edu_origine#c.potentialExp] if edu_origine==2
replace sloOriSe=_se[:3.edu_origine#c.potentialExp] if edu_origine==3
replace sloOriSe=_se[:4.edu_origine#c.potentialExp] if edu_origine==4
gen sloCohSe=0
replace sloCohSe=_se[:2.cohort#c.potentialExp] if cohort==2
replace sloCohSe=_se[:3.cohort#c.potentialExp] if cohort==3
replace sloCohSe=_se[:4.cohort#c.potentialExp] if cohort==4

* SE Quadratic term
gen quadraSe=_se[:c.potentialExp#c.potentialExp]

*********************************
save resultsBaseMen.dta,replace
*********************************
********************************************************************************
