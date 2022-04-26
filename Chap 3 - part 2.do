*****************************************
** Chapter 3 thesis Lucas Sage **********
*****************************************
* Version March 2022
* FIRST SCRIPT: REGRESSION
* STATA

*********************************
use dadsEdpFullFinal.dta,clear
*********************************

* This is the command to get clustered standard errors:
reghdfe y1 i.pcs2 i.clust i.year i.ageCat i.genderAge ///
if main_job_gross==1, absorb(wid) vce(cluster i.pcs2 i.clust i.year) residuals 
matrix b = e(b)

* Manually extract all coefficients:
gen ofe =.
gen ffe=.
gen ann=.
gen ageCoef=.
gen genderAgeCoef=.

* Occupational fixed effects:
replace ofe=b[1,1] if pcs2==22
replace ofe=b[1,2] if pcs2==23
replace ofe=b[1,3] if pcs2==31
replace ofe=b[1,4] if pcs2==33
replace ofe=b[1,5] if pcs2==34
replace ofe=b[1,6] if pcs2==35
replace ofe=b[1,7] if pcs2==37
replace ofe=b[1,8] if pcs2==38
replace ofe=b[1,9] if pcs2==43
replace ofe=b[1,10] if pcs2==45
replace ofe=b[1,11] if pcs2==46
replace ofe=b[1,12] if pcs2==47
replace ofe=b[1,13] if pcs2==48
replace ofe=b[1,14] if pcs2==52
replace ofe=b[1,15] if pcs2==53
replace ofe=b[1,16] if pcs2==54
replace ofe=b[1,17] if pcs2==55
replace ofe=b[1,18] if pcs2==56
replace ofe=b[1,19] if pcs2==62
replace ofe=b[1,20] if pcs2==63
replace ofe=b[1,21] if pcs2==64
replace ofe=b[1,22] if pcs2==65
replace ofe=b[1,23] if pcs2==67
replace ofe=b[1,24] if pcs2==68

* Organizational fixed effects:
replace ffe=b[1,25] if clust==1
replace ffe=b[1,26] if clust==2
replace ffe=b[1,27] if clust==3
replace ffe=b[1,28] if clust==4
replace ffe=b[1,29] if clust==5
replace ffe=b[1,30] if clust==6
replace ffe=b[1,31] if clust==7
replace ffe=b[1,32] if clust==8
replace ffe=b[1,33] if clust==9
replace ffe=b[1,34] if clust==10
replace ffe=b[1,35] if clust==11
replace ffe=b[1,36] if clust==12
replace ffe=b[1,37] if clust==13
replace ffe=b[1,38] if clust==14
replace ffe=b[1,39] if clust==15
replace ffe=b[1,40] if clust==16
replace ffe=b[1,41] if clust==17
replace ffe=b[1,42] if clust==18
replace ffe=b[1,43] if clust==19
replace ffe=b[1,44] if clust==20
replace ffe=b[1,45] if clust==21
replace ffe=b[1,46] if clust==22
replace ffe=b[1,47] if clust==23
replace ffe=b[1,48] if clust==24

* Year fixed effects:
replace ann=b[1,49] if year==2010
replace ann=b[1,50] if year==2011
replace ann=b[1,51] if year==2012
replace ann=b[1,52] if year==2013
replace ann=b[1,53] if year==2014
replace ann=b[1,54] if year==2015

* Age:
replace ageCoef=b[1,55] if ageCat==0
replace ageCoef=b[1,56] if ageCat==1
replace ageCoef=b[1,57] if ageCat==2
replace ageCoef=b[1,58] if ageCat==3

* Interaction Age * Gender:
replace genderAgeCoef=b[1,59] if genderAge==0
replace genderAgeCoef=b[1,60] if genderAge==1
replace genderAgeCoef=b[1,61] if genderAge==2
replace genderAgeCoef=b[1,62] if genderAge==3

* Manually extract Standard errors:
gen ofeSe=.
gen ffeSe=.
gen annSe=.
gen ageCoefSe=.
gen genderAgeCoefSe=.

* Occupational fixed effects SE
replace ofeSe=_se[22.pcs2] if pcs2==22
replace ofeSe=_se[23.pcs2] if pcs2==23
replace ofeSe=_se[31.pcs2] if pcs2==31
replace ofeSe=_se[33.pcs2] if pcs2==33
replace ofeSe=_se[34.pcs2] if pcs2==34
replace ofeSe=_se[35.pcs2] if pcs2==35
replace ofeSe=_se[37.pcs2] if pcs2==37
replace ofeSe=_se[38.pcs2] if pcs2==38
replace ofeSe=_se[43.pcs2] if pcs2==43
replace ofeSe=_se[45.pcs2] if pcs2==45
replace ofeSe=_se[46.pcs2] if pcs2==46
replace ofeSe=_se[47.pcs2] if pcs2==47
replace ofeSe=_se[48.pcs2] if pcs2==48
replace ofeSe=_se[52.pcs2] if pcs2==52
replace ofeSe=_se[53.pcs2] if pcs2==53
replace ofeSe=_se[54.pcs2] if pcs2==54
replace ofeSe=_se[55.pcs2] if pcs2==55
replace ofeSe=_se[56.pcs2] if pcs2==56
replace ofeSe=_se[62.pcs2] if pcs2==62
replace ofeSe=_se[63.pcs2] if pcs2==63
replace ofeSe=_se[64.pcs2] if pcs2==64
replace ofeSe=_se[65.pcs2] if pcs2==65
replace ofeSe=_se[67.pcs2] if pcs2==67
replace ofeSe=_se[68.pcs2] if pcs2==68

* Organizational fixed effects SE
replace ffeSe=_se[1.clust] if clust==1
replace ffeSe=_se[2.clust] if clust==2
replace ffeSe=_se[3.clust] if clust==3
replace ffeSe=_se[4.clust] if clust==4
replace ffeSe=_se[5.clust] if clust==5
replace ffeSe=_se[6.clust] if clust==6
replace ffeSe=_se[7.clust] if clust==7
replace ffeSe=_se[8.clust] if clust==8
replace ffeSe=_se[9.clust] if clust==9
replace ffeSe=_se[10.clust] if clust==10
replace ffeSe=_se[11.clust] if clust==11
replace ffeSe=_se[12.clust] if clust==12
replace ffeSe=_se[13.clust] if clust==13
replace ffeSe=_se[14.clust] if clust==14
replace ffeSe=_se[15.clust] if clust==15
replace ffeSe=_se[16.clust] if clust==16
replace ffeSe=_se[17.clust] if clust==17
replace ffeSe=_se[18.clust] if clust==18
replace ffeSe=_se[19.clust] if clust==19
replace ffeSe=_se[20.clust] if clust==20
replace ffeSe=_se[21.clust] if clust==21
replace ffeSe=_se[22.clust] if clust==22
replace ffeSe=_se[23.clust] if clust==23
replace ffeSe=_se[24.clust] if clust==24

* Year fixed effects SE
replace annSe=_se[2010.year] if year==2010
replace annSe=_se[2011.year] if year==2011
replace annSe=_se[2012.year] if year==2012
replace annSe=_se[2013.year] if year==2013
replace annSe=_se[2014.year] if year==2014
replace annSe=_se[2015.year] if year==2015

* Age SE
replace ageCoefSe=_se[0.ageCat] if ageCat==0
replace ageCoefSe=_se[1.ageCat] if ageCat==1
replace ageCoefSe=_se[2.ageCat] if ageCat==2
replace ageCoefSe=_se[3.ageCat] if ageCat==3

* Interaction gender age SE
replace genderAgeCoefSe=_se[0.genderAge] if genderAge==0
replace genderAgeCoefSe=_se[1.genderAge] if genderAge==1
replace genderAgeCoefSe=_se[2.genderAge] if genderAge==2
replace genderAgeCoefSe=_se[3.genderAge] if genderAge==3


****************************************
save dadsEdpFullResultsReg.dta, replace
****************************************
