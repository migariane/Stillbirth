********************************************************************************************
* Secular Trends in Stillbirth by Maternal Socioeconomic Status in Spain 2007-2015: 
* A Population-Based Study of 4 Million Births 
* MA. LUQUE-FERNANDEZ, AURIELLE THOMAS, BIZU GELAYE, JUDITH RACAPE, MJ SANCHEZ, MA WILLIAMS
* Date created 10/11/2018
* Data: All Spanish births including stillbirths from 2007-2015
* restricted to >= 28 weeks gestation
* N = 4,179,402 births
********************************************************************************************

/*
Copyright (c) 2018  <Miguel Angel Luque-Fernandez>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

// 		      |         stillbirth
//       Year |        No        Yes |     Total
// -----------+----------------------+----------
//       2007 |   491,288      1,309 |   492,597 
//       2008 |   516,602      1,409 |   518,011 
//       2009 |   491,856      1,344 |   493,200 
//       2010 |   483,456      1,303 |   484,759 
//       2011 |   468,936      1,288 |   470,224 
//       2012 |   451,678      1,259 |   452,937 
//       2013 |   422,742      1,188 |   423,930 
//       2014 |   424,498      1,111 |   425,609 
//       2015 |   417,023      1,112 |   418,135 
// -----------+----------------------+----------
//      Total | 4,168,079     11,323 | 4,179,402 

**************************************************************

global data "your path"
clear
set more off
cd $data
use SpainBirths0715.dta

////////////////////////////////////
// TABLE ONE
////////////////////////////////////

// Stillbirth rate per 1,000 births (95% CI) and univariate rate ratios for Table 1.
// Stillbirth Rates 95% CI
preserve
collapse (sum) stillbirth (sum) n, by(mage2)
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
collapse (sum) stillbirth (sum) n, by(yeardlv)
tabrate stillbirth yeardlv, exp(n) per 1000 trend
restore

// Univariate Rate Ratios (FETUSES-AT-RISK): Attributable risk and Population Attributable Risk (PAR)
// Maternal age
glm stillbirth ib2.mage2, exp(gweek) eform fam(pois) link(log)   
// Maternal Education
glm stillbirth ib3.meduc, exp(gweek) eform fam(pois) link(log)    =
// Maternal country of origin 
glm stillbirth i.mcorigin, exp(gweek) eform fam(pois) link(log)    
// Maternal country of origin Human Developement Index
glm stillbirth i.CatIHD, exp(gweek) eform fam(pois) link(log)   
// Parity
glm stillbirth i.parity, exp(gweek) eform fam(pois) link(log)   
// Gestation weeks
glm stillbirth i.catgweek, exp(gweek) eform fam(pois) link(log)    
// Calendar period
glm stillbirth i.yeardlv, exp(gweek) eform fam(pois) link(log)    

// Figure 1 Stillbirth rates by calendar period and maternal education attainment
preserve
collapse (sum) stillbirth (sum) n, by(yeardlv  meduc)
forvalues i = 0/5{
gen Rate_Meduc`i' = stillbirth / n * 1000 if meduc == `i'
}
egen Rate = rowtotal(Rate_Meduc*)
drop Rate_Meduc*
rename yeardlv Year
label var Year "Year"
tw(line Rate Year if meduc == 1)||(line Rate Year if meduc==2) ///
(line Rate Year if meduc==3), ylabel(0(1)7, gmax angle(horizontal)) ///
 xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log)
restore

// Stillbirth Rate of Change 
glm stillbirth i.medu##c.yeardlv, exp(gweek) eform fam(pois) link(log)
lincom yeardlv + 2.medu#c.yeardlv, eform
lincom yeardlv + 3.medu#c.yeardlv, eform


// Figure 2 Stillbirth rates by calendar period, maternal education and age 
preserve
keep if age<35
collapse (sum) stillbirth (sum) n, by(yeardlv  meduc)
forvalues i = 0/5{
gen Rate_Meduc`i' = stillbirth/n*1000 if meduc == `i'
}
egen Rate = rowtotal(Rate_Meduc*)
drop Rate_Meduc*
rename yeardlv Year
label var Year "Year"
tw(line Rate Year if meduc == 1)||(line Rate Year if meduc == 2) ///
(line Rate Year if meduc == 3), ylabel(0(1)7, gmax angle(horizontal)) ///
xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log) saving(a)
restore

preserve
keep if age>=35 | age<40
collapse (sum) stillbirth (sum) n, by(yeardlv  meduc)
forvalues i = 0/5{
gen Rate_Meduc`i' = stillbirth / n * 1000 if meduc == `i'
}
egen Rate = rowtotal(Rate_Meduc*)
drop Rate_Meduc*
rename yeardlv Year
label var Year "Year"
tw(line Rate Year if meduc == 1)||(line Rate Year if meduc == 2) ///
(line Rate Year if meduc == 3), ylabel(0(1)7, gmax angle(horizontal)) ///
xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log) saving(b)
restore

preserve
keep if age>40
collapse (sum) stillbirth (sum) n, by(yeardlv  meduc)
forvalues i = 0 / 5{
gen Rate_Meduc`i' = stillbirth / n * 1000 if meduc == `i'
}
egen Rate = rowtotal(Rate_Meduc*)
drop Rate_Meduc*
rename yeardlv Year
label var Year "Year"
tw(line Rate Year if meduc == 1)||(line Rate Year if meduc == 2) ///
(line Rate Year if meduc == 3), ylabel(0(1)7, gmax angle(horizontal)) ///
 xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log) saving(c)
restore

graph combine a.gph b.gph c.gph, r(1)

// Stillbirth Rate of Change by calendar year
preserve 
keep if age<35
glm stillbirth i.medu##c.yeardlv, exp(gweek) eform fam(pois) link(log)
lincom yeardlv + 2.medu#c.yeardlv, eform
lincom yeardlv + 3.medu#c.yeardlv, eform
restore

preserve 
keep if age>=35 | age<40
glm stillbirth i.medu##c.yeardlv, exp(gweek) eform fam(pois) link(log)
lincom yeardlv + 2.medu#c.yeardlv, eform
lincom yeardlv + 3.medu#c.yeardlv, eform
restore

preserve 
keep if age>40
glm stillbirth i.medu##c.yeardlv, exp(gweek) eform fam(pois) link(log)
lincom yeardlv + 2.medu#c.yeardlv, eform
lincom yeardlv + 3.medu#c.yeardlv, eform
restore

// Supplementary Figure 2 Stillbirth rates over time by maternal country of origin
preserve
collapse (sum) stillbirth (sum) n, by(yeardlv  mcorigin)
forvalues i = 0/5{
gen Rate_MCO`i' = stillbirth/n*1000 if mcorigin ==`i'
}
egen Rate = rowtotal(Rate_MCO*)
drop Rate_MCO*
rename yeardlv Year
label var Year "Year"
line Rate Year, by(mcorigin) ylabel(0(1)7, gmax angle(horizontal)) ///
xlabel(2006(1)2016, angle(45)) ytitle("Stillbirth Rates by Maternal Country of Origin") yscale(log)
restore

// Testing linear trend for stillbirth rates by maternal country of origin and maternal educational attainment over time
preserve
collapse (sum) stillbirth (sum) n, by(yeardlv mcorigin)
forvalues i = 0/5{
tabrate stillbirth yeardlv if mcorigin ==`i', exp(n) per 1000 trend
}
restore

preserve
collapse (sum) stillbirth (sum) n, by(yeardlv meduc)
forvalues i = 0/3{
tabrate stillbirth yeardlv if meduc ==`i', exp(n) per 1000 trend
}
restore

// MULTIVARITE analysis: Fetuses at risk approach
// Model 1
glm stillbirth i.CatIHD
// Model 2
glm stillbirth i.CatIHD ib2.mage2, exp(gweek) eform fam(pois) link(log)  
// Model 3 
glm stillbirth i.CatIHD ib2.mage2 ib3.meduc, exp(gweek) eform fam(pois) link(log)  
// Model 4
glm stillbirth i.CatIHD ib2.mage2 ib3.meduc i.parity, exp(gweek) eform fam(pois) link(log)  
// Model 5
glm stillbirth i.CatIHD ib2.mage2 ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log)  

// Model 6 Multiple Imputation (FCS assuming MVN: 5 datasets imputed with AMELIA in R)
* Keep substantive variables for imputation associated with gestation weeks at delivery 
preserve
keep stillbirth CatIHD age meduc mcorigin parity yeardlv gweek 
save IMPDATA.dta
restore
* Impute FCS: MVN using AMELIA impuation model stillbirth CatIHD age meduc mcorigin parity yeardlv
// library(Amelia)
// amelia(x = getAmelia("amelia.data"), m = 40, idvars = NULL, ts = NULL, 
//        cs = NULL, priors = NULL, lags = NULL, empri = 0, intercs = FALSE, 
//        leads = NULL, splinetime = NULL, logs = NULL, sqrts = NULL, 
//        lgstc = NULL, ords = "meduc", "gweek", noms = NULL, bounds = c(2, 
//        4, 28, 1, 44, 3), max.resample = 1000, tolerance = 1e-04)
clear
set more off
cd "your path"
clear

forvalues i = 1/40{
use imp-imp`i'.dta
gen mj = `i'
gen mi = _n
save imp`i', replace
clear
}

clear 
use imp1.dta
append using imp2 imp3 imp4 imp5 imp6 imp7 imp8 imp9 ///
imp10 imp11 imp12 imp13 imp14 imp15 imp16 imp17 imp18 imp19 ///
imp20 imp21 imp22 imp23 imp24 imp25 imp26 imp27 imp28 imp29 ///
imp30 imp31 imp32 imp33 imp34 imp35 imp36 imp37 imp38 imp39 imp40, gen(mj2)
gen double id = _n //Note double precission here!
save Imp1to40.dta, replace

// Stata Multiple Imputation Analysis: Rubin's rules (mi estimate)
set processors 4
count
mi import flong, m(mj2) id(mi) imputed(gweek meduc)
mi describe 
gen mage2 = age
recode mage2 0/19 = 1 20/24 = 0 25/29 = 2 30/34 = 3 35/39 = 4 40/80 = 5
mi estimate: glm stillbirth i.CatIHD i.mage2 ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log) 

// Sensitivity analysis (MAR): Characteristics missing information

// Subsample complete case
tab mage2 yeardlv, col 
tab meduc yeardlv, col
tab mcorigin yeardlv, col

// Subsample missing data
preserve
gen flag = cond(meduc ==. | gweek ==. ,1 , 0.)
keep if flag == 1
tab mage2 yeardlv, col 
tab meduc yeardlv, col 
tab mcorigin yeardlv, col 
restore

////////////////////////////////////////////////////////////////////////////////
//  TABLE 3 Joint effects for Human Development Index and Maternal Education
////////////////////////////////////////////////////////////////////////////////

sumup gweek, by(stillbirth)
glm stillbirth i.CatIHD ib1.mage2 ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log)  

// Joint effects education and Human Development Index 
lincom 1.meduc + 2.CatIHD, eform
lincom 1.meduc + 3.CatIHD, eform
lincom 1.meduc + 4.CatIHD, eform

lincom 2.meduc + 2.CatIHD, eform
lincom 2.meduc + 3.CatIHD, eform
lincom 2.meduc + 4.CatIHD, eform

// MATERNAL AGE in four categories
gen mage3 = ag
recode mage3 (min/24 = 1) (25/29 = 0)(30/34 = 2) (35/max = 3)
tab mage3, miss /* There shouldn't be any missing. */
lab def mage3 0"25-29" 1"<25" 2"30-34" 3">=35", replace
lab val mage3 mage3
tab mage3, miss

glm stillbirth i.CatIHD i.mage3 ib3.meduc i.parity yeardlv, exp(gweek) eform fam(pois) link(log)  

// Joint effects education and age
lincom 1.meduc + 1.mage3, eform
lincom 1.meduc + 2.mage3, eform
lincom 1.meduc + 3.mage3, eform

lincom 2.meduc + 1.mage3, eform
lincom 2.meduc + 2.mage3, eform
lincom 2.meduc + 3.mage3, eform

// END
