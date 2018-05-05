********************************************************************************************
* Stillbirth project: secular trends and risk by socioeconomic status in Spain 2007-2015
* MA. LUQUE-FERNANDEZ, AURIELLE THOMAS, BIZU GELAYE, JUDITH RACAPE, MJ SANCHEZ, MA WILLIAMS
* Date created 21/11/2017
* Data: All Spanish births (alive and stillborn) from 2007-2015
* Restriction to >=28 gestation weeks (early stillbirth)
* N = 4,083,919 births
********************************************************************************************

// 		      |         stillbirth
//       Year |        No        Yes |     Total
// -----------+----------------------+----------
//       2007 |   396,015      1,099 |   397,114 
//       2008 |   516,602      1,409 |   518,011 
//       2009 |   491,856      1,344 |   493,200 
//       2010 |   483,456      1,303 |   484,759 
//       2011 |   468,936      1,288 |   470,224 
//       2012 |   451,678      1,259 |   452,937 
//       2013 |   422,742      1,188 |   423,930 
//       2014 |   424,498      1,111 |   425,609 
//       2015 |   417,023      1,112 |   418,135 
// -----------+----------------------+----------
//      Total | 4,072,806     11,113 | 4,083,919 

**************************************************************

global data "your path"
clear
set more off
cd $data
use SpainBirths0715.dta

////////////////////////////////////
// TABLE ONE
////////////////////////////////////

// Determing "Stillbirth rate per 1,000 births (95% CI)" and univariate rate ratios for Table 1.

// Stillbirth Rates 95% CI
preserve
collapse (sum) stillbirth (sum) n, by(mage)
tabrate stillbirth mage, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n, by(CatIHD)
tabrate stillbirth CatIHD, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n, by(meduc)
tabrate stillbirth meduc, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n, by(parity)
tabrate stillbirth parity, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n, by(mcorigin)
tabrate stillbirth mcorigin, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n, by(catgweek)
tabrate stillbirth catgweek, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n, by(gweek)
tabrate stillbirth gweek, exp(n) per 1000 trend
restore

preserve
collapse (sum) stillbirth (sum) n if CatIHD==4, by(gweek)
tabrate stillbirth gweek, exp(n) per 1000 trend
restore

// Univariate Rate Ratios (FETUSES-AT-RISK): Attributable risk and Population Attributable Risk (PAR)

// Maternal age
glm stillbirth ib1.mage, exp(gweek) eform fam(pois) link(log)   
forvalues i = 0/4{
nlcom (exp(_b[`i'.mage])-1)/exp(_b[`i'.mage])  //Delta Method for 95%CI
}
punaf, atzero(mage=0) atspec(mage=1) eform
punaf, atzero(mage=2) atspec(mage=1) eform
punaf, atzero(mage=3) atspec(mage=1) eform
punaf, atzero(mage=4) atspec(mage=1) eform

// Maternal Education
glm stillbirth ib3.meduc, exp(gweek) eform fam(pois) link(log)    
forvalues i = 1/3{
nlcom (exp(_b[`i'.meduc])-1)/exp(_b[`i'.meduc])  //Delta Method for 95%CI
}
punaf, atzero(meduc=1) atspec(meduc=3) eform
punaf, atzero(meduc=2) atspec(meduc=3) eform

// Maternal country of origin 
glm stillbirth i.mcorigin, exp(gweek) eform fam(pois) link(log)    
forvalues i = 0/5{
nlcom (exp(_b[`i'.mcorigin])-1)/exp(_b[`i'.mcorigin])  //Delta Method for 95%CI
}
punaf, atzero(mcorigin=1) atspec(mcorigin=0) eform
punaf, atzero(mcorigin=2) atspec(mcorigin=0) eform
punaf, atzero(mcorigin=3) atspec(mcorigin=0) eform
punaf, atzero(mcorigin=4) atspec(mcorigin=0) eform
punaf, atzero(mcorigin=5) atspec(mcorigin=0) eform

// Maternal country of origin Human Developement Index
glm stillbirth i.CatIHD, exp(gweek) eform fam(pois) link(log)   
forvalues i = 1/4{
nlcom (exp(_b[`i'.CatIHD])-1)/exp(_b[`i'.CatIHD])  //Delta Method for 95%CI
}
punaf, atzero(CatIHD=2) atspec(CatIHD=1) eform
punaf, atzero(CatIHD=3) atspec(CatIHD=1) eform
punaf, atzero(CatIHD=4) atspec(CatIHD=1) eform

// Parity
glm stillbirth i.parity, exp(gweek) eform fam(pois) link(log)   
nlcom (exp(_b[1.parity])-1)/exp(_b[1.parity])  //Delta Method for 95%CI
punaf, atzero(parity=1) atspec(parity=0) eform
 
// Gestation week
glm stillbirth i.catgweek, exp(gweek) eform fam(pois) link(log)    
nlcom (exp(_b[1.catgweek])-1)/exp(_b[1.catgweek])  //Delta Method for 95%CI
punaf, atzero(catgweek=1) atspec(catgweek=0) eform

// Calendar period
glm stillbirth i.yeardlv, exp(gweek) eform fam(pois) link(log)    
forvalues i = 2008/2015{
nlcom (exp(_b[`i'.yeardlv])-1)/exp(_b[`i'.yeardlv])  //Delta Method for 95%CI
}

 
// FIGURE 1 Stillbirth rates over time by maternal education 

preserve
collapse (sum) stillbirth (sum) n, by(yeardlv  meduc)
forvalues i = 0/5{
gen Rate_Meduc`i' = stillbirth/n*1000 if meduc==`i'
}
egen Rate = rowtotal(Rate_Meduc*)
drop Rate_Meduc*
rename yeardlv Year
label var Year "Year"
tw(line Rate Year if meduc==1)||(line Rate Year if meduc==2)(line Rate Year if meduc==3), ylabel(0(1)7, gmax angle(horizontal)) xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log)
restore

// Stillbirth Rate of Change 

glm stillbirth i.medu##c.yeardlv, exp(gweek) eform fam(pois) link(log)
lincom ANOPAR + 2.medu#c.yeardlv, eform
lincom ANOPAR + 3.medu#c.yeardlv, eform


// Note: Figure 2 same as Figure 1 but stratified by levels of maternal age (<35 and >=35 years)

// Supplementary Figure 2 Stillbirth rates over time by maternal country of origin

preserve
collapse (sum) stillbirth (sum) n, by(yeardlv  mcorigin)
forvalues i = 0/5{
gen Rate_MCO`i' = stillbirth/n*1000 if mcorigin==`i'
}
egen Rate = rowtotal(Rate_MCO*)
drop Rate_MCO*
rename yeardlv Year
label var Year "Year"
line Rate Year, by(mcorigin) ylabel(0(1)7, gmax angle(horizontal)) xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log)
restore

// Trend test p-value for maternal country of origin over time

preserve
collapse (sum) stillbirth (sum) n, by(yeardlv mcorigin)
forvalues i = 0/5{
tabrate stillbirth yeardlv if mcorigin==`i', exp(n) per 1000 trend
}
restore

// MULTIVARITE analysis: Fetuses at risk approach

// Model 2
glm stillbirth i.CatIHD ib1.mage, exp(gweek) eform fam(pois) link(log)  
// Model 3 
glm stillbirth i.CatIHD ib1.mage ib3.meduc, exp(gweek) eform fam(pois) link(log)  
// Model 4
glm stillbirth i.CatIHD ib1.mage ib3.meduc i.parity, exp(gweek) eform fam(pois) link(log)  
// Model 5
glm stillbirth i.CatIHD ib1.mage ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log)  

// Model 6 Multiple Imputation (FCS assuming MVN: 5 datasets imputed with AMELIA in R)

* Keep substantive variables for imputation associated with gestation weeks at delivery 
keep stillbirth CatIHD mage meduc mcorigin parity yeardlv gweek 
save IMPDATA.dta
* Impute FCS: MVN using AMELIA impuation model stillbirth CatIHD mage meduc mcorigin parity yeardlv
// library(Amelia)
// amelia(x = getAmelia("amelia.data"), m = 5, idvars = NULL, ts = NULL, 
//        cs = NULL, priors = NULL, lags = NULL, empri = 0, intercs = FALSE, 
//        leads = NULL, splinetime = NULL, logs = NULL, sqrts = NULL, 
//        lgstc = NULL, ords = "meduc", noms = NULL, bounds = c(2, 
//        4, 28, 1, 42, 3), max.resample = 1000, tolerance = 1e-04)
clear
set more off
cd "your path"
clear

forvalues i = 0/40{
use imp`i'.dta
drop mj 
drop mi
save imp`i', replace
clear
}

forvalues i = 0/40{
use imp`i'.dta
gen mj = `i'
gen mi = _n
save imp`i', replace
clear
}

clear 
use imp0.dta
append using imp1 imp2 imp3 imp4 imp5 imp6 imp7 imp8 imp9 ///
imp10 imp11 imp12 imp13 imp14 imp15 imp16 imp17 imp18 imp19 ///
imp20 imp21 imp22 imp23 imp24 imp25 imp26 imp27 imp28 imp29 ///
imp30 imp31 imp32 imp33 imp34 imp35 imp36 imp37 imp38 imp39 imp40, gen(mj2)
gen double id = _n //Note double precission here!
save Imp0to40.dta, replace

* Stata Imputation
set processors 4
count
mi import flong, m(mj2) id(mi) imputed(gweek meduc)
mi describe 
mi stset, clear
mi estimate: glm stillbirth i.CatIHD ib1.mage ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log) 


////////////////////////////////////////////////////////////////////////////////
//  TABLE 3 Joint effects for Human Development Index and Maternal Education
////////////////////////////////////////////////////////////////////////////////

sumup SEMANAS, by(stillbirth)
glm stillbirth i.CatIHD ib1.mage ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log)  

// Joint effects education and Human Development Index 
lincom 1.meduc + 2.CatIHD, eform
lincom 1.meduc + 3.CatIHD, eform
lincom 1.meduc + 4.CatIHD, eform

lincom 2.meduc + 2.CatIHD, eform
lincom 2.meduc + 3.CatIHD, eform
lincom 2.meduc + 4.CatIHD, eform

// MATERNAL AGE in four categories
gen mage2 = EDADM
recode mage2 (min/24=1) (25/29=0)(30/34=2) (35/max=3)
tab mage2, miss /* There shouldn't be any missing. */
lab def mage2 0"25-29" 1"<25" 2"30-34" 3">=35", replace
lab val mage2 mage2
tab mage2, miss

glm stillbirth i.CatIHD i.mage2 ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log)  

// Joint effects education and age
lincom 1.meduc + 1.mage2, eform
lincom 1.meduc + 2.mage2, eform
lincom 1.meduc + 3.mage2, eform

lincom 2.meduc + 1.mage2, eform
lincom 2.meduc + 2.mage2, eform
lincom 2.meduc + 3.mage2, eform

// END
