

****************************************DESCRIPTIVE STATISTICS****************************************

***** Information used for Table 1********
*******Note: Scheme and Still_in_the_UK and Still_in_Academia variables are ommitted to avoid identification of individuals**
*table ( scheme ) ( success )
table ( gender_bi ) ( success ), m
table ( bame ) ( success ), m
table ( gt_sub ) ( success ), m
*table ( still_acad ) ( success ), m
*table ( still_UK ) ( success ), m
ttest age_sub, by (success)

***** Information used for Table 2********
table success gender_bi board, m

***** Information used for Table 3********
table gender_bi success, m
table gender_bi success if board==1, m



****************************************MAIN ANALYSIS****************************************
****************************************PREPARATION****************************************

*****keep if they reached the board level************
keep if board==1
*****drop if they have received another ECR fellowship*****
keep if other_fellow==0

**************standardisation of publications/income pre submission*****************************************
bysort for_l1_n: egen z2pubs_pre = std(pubs_pre)
gen ln_citations_pre=ln(citations_pre)
bysort for_l1_n: egen z2cit_pre = std(ln_citations_pre)
bysort for_l1_n: egen z2avercr_pre= std(avercr_pre)
bysort for_l1_n: egen z2avefcr_pre= std(avefcr_pre)
bysort for_l1_n: egen z2avealt_pre= std(avealt_pre)
bysort for_l1_n: egen z2income_pre = std(income_pre) 

*Standardisation of income inverse hyperbolic sine tranformation********
gen h_income_pre=asinh(income_pre)
gen h_income_post=asinh(income_post)
gen h_pi_income_post=asinh(pi_income_post)
gen h_income_post_pa=asinh(income_post_pa)
gen h_pi_income_post_pa=asinh(pi_income_post_pa)


gen pubs_pa_post=pubs_post/yr_post_sub
bysort for_l1_n: egen z2pubs_post_pa_post = std(pubs_pa_post)
gen citations_pa_post=citations_post/yr_post_sub
bysort for_l1_n: egen z2citations_pa_post = std(citations_pa_post)

bysort for_l1_n: egen z2avercr_post= std(avercr_post)
bysort for_l1_n: egen z2avefcr_post= std(avefcr_post)
bysort for_l1_n: egen z2avealt_post= std(avealt_post)

bysort for_l1_n: egen z2income_post_pa = std(income_post_pa) 
bysort for_l1_n: egen z2pi_income_post_pa = std(pi_income_post_pa) 


******Information for Table 4**************************************************
****Total sample****************
logit success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical
predict preprob2, pr
capture drop psweight
gen psweight=.
replace psweight=(1/preprob2) if success==1
replace psweight=(1/(1-preprob2)) if success==0

* Clear previous matrices
cap matrix drop pvals_unweighted
cap matrix drop pvals_weighted

* Store the p-values from the regressions
local vars age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical

foreach var in `vars' {
    logit success `var'
    matrix pvals_unweighted = nullmat(pvals_unweighted) \ e(p)'

    logit success `var' [pweight=psweight]
    matrix pvals_weighted = nullmat(pvals_weighted) \ e(p)'
}

* For unweighted p-values
svmat pvals_unweighted
* For weighted p-values
svmat pvals_weighted

* Generate model labels
gen model = ""
local names "Age Gender BAME GT Publications Citations AvgCR AvgFCR AvgAlt Income Past_Applications Clinical"

local i = 1
foreach name in `names' {
    replace model = "`name'" in `i'
    local i = `i' + 1
}

* Rename columns
rename pvals_unweighted1 unweighted_pval
rename pvals_weighted1 weighted_pval

* Show the results
*drop if model == ""
list model unweighted_pval weighted_pval in 1/12, separator(0)

drop  model


************male ***************
* Run initial logistic regression for all variables
logit success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical if gender_bi==1

* Predict probabilities for males
predict preprob2_male if gender_bi==1, pr

* Create psweight_male
capture drop psweight_male
gen psweight_male = . if gender_bi == 1
replace psweight_male = (1 / preprob2_male) if success == 1
replace psweight_male = (1 / (1 - preprob2_male)) if success == 0

* List of variables for logit
local vars age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical

* Loop over the variables to run logit models with and without weights
foreach var of local vars {
    logit success `var' if gender_bi == 1
    matrix m_unweighted = nullmat(m_unweighted) \ e(p)'
   
    logit success `var' if gender_bi == 1 [pweight = psweight_male]
	matrix m_weighted = nullmat(m_weighted) \ e(p)'
}

svmat m_unweighted
svmat m_weighted

gen model = ""
local names "Age BAME GT Publications Citations AvgCR AvgFCR AvgAlt Income Past_Applications Clinical"

local i = 1
foreach name in `names' {
    replace model = "`name'" in `i'
    local i = `i' + 1
}

list model m_unweighted m_weighted in 1/11, separator(0)

drop  model

************female ***************
* Run initial logistic regression for all variables
logit success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical if gender_bi==0

* Predict probabilities for males
predict preprob2_female if gender_bi==0, pr

* Create psweight_female
capture drop psweight_female
gen psweight_female = . if gender_bi == 0
replace psweight_female = (1 / preprob2_female) if success == 1
replace psweight_female = (1 / (1 - preprob2_female)) if success == 0

* List of variables for logit
local vars age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical

* Loop over the variables to run logit models with and without weights
foreach var of local vars {
    logit success `var' if gender_bi == 0
    matrix f_unweighted = nullmat(f_unweighted) \ e(p)'
   
    logit success `var' if gender_bi == 0 [pweight = psweight_female]
	matrix f_weighted = nullmat(f_weighted) \ e(p)'
}

svmat f_unweighted
svmat f_weighted

gen model = ""
local names "Age BAME GT Publications Citations AvgCR AvgFCR AvgAlt Income Past_Applications Clinical"

local i = 1
foreach name in `names' {
    replace model = "`name'" in `i'
    local i = `i' + 1
}

list model f_unweighted f_weighted in 1/11, separator(0)


**************PSW**************
*************Information used for Tables 5-7************
teffects ipw (z2pubs_post_pa_post) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store p1, title(Publications per year after submission)
teffects ipw (z2citations_pa_post) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store p2, title(Citations per year after submission)
teffects ipw (z2avercr_post) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store p3, title(Average RCR)
teffects ipw (z2avefcr_post) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store p4, title(Average FCR)
teffects ipw (z2avealt_post) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store p5, title(Average Altmetric)
estout p1 p2 p3 p4 p5, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2 df_r bic, fmt(3 0 1))

teffects ipw (h_income_post_pa) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store q1, title(Total Income Post per year)
teffects ipw (h_pi_income_post_pa) (success age_sub gender_bi bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) 
estimate store q2, title(PI Income post per year)
estout q1 q2, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2 df_r bic, fmt(3 0 1))

********balance test used for Table 9*******
tebalance overid 

*************overlap test used for Figure 2*******************
teoverlap
   

**********************subgroup analysis****************************************
****female****************
****Information used for Table 6****************
teffects ipw (z2pubs_post_pa_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store p1_f, title(Publications per year after submission)
teffects ipw (z2citations_pa_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store p2_f, title(Citations per year after submission)
teffects ipw (z2avercr_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store p3_f, title(Average RCR)
teffects ipw (z2avefcr_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store p4_f, title(Average FCR)
teffects ipw (z2avealt_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store p5_f, title(Average Altmetric)
estout p1_f p2_f p3_f p4_f p5_f, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2 df_r bic, fmt(3 0 1))

****Information used for Table 7****************
teffects ipw (h_income_post_pa) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store q1_f, title(Total Income Post per year)
teffects ipw (h_pi_income_post_pa) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==0
estimate store q2_f, title(PI Income post per year)
estout q1_f q2_f, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2 df_r bic, fmt(3 0 1))


************Information used for Table 9*********************************
tebalance overid 

*************overlap test used for Figure 2*******************
teoverlap

****male******************
****Information used for Table 6****************
teffects ipw (z2pubs_post_pa_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store p1_m, title(Publications per year after submission)
teffects ipw (z2citations_pa_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store p2_m, title(Citations per year after submission)
teffects ipw (z2avercr_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store p3_m, title(Average RCR)
teffects ipw (z2avefcr_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store p4_m, title(Average FCR)
teffects ipw (z2avealt_post) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store p5_m, title(Average Altmetric)
estout p1_m p2_m p3_m p4_m p5_m, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2 df_r bic, fmt(3 0 1))

****Information used for Table 7****************
teffects ipw (h_income_post_pa) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store q1_m, title(Total Income Post per year)
teffects ipw (h_pi_income_post_pa) (success age_sub bame gt_sub z2pubs_pre z2cit_pre z2avercr_pre z2avefcr_pre z2avealt_pre income_pre_bi past_appl_bi clinical) if gender_bi==1
estimate store q2_m, title(PI Income post per year)
estout q1_m q2_m, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2 df_r bic, fmt(3 0 1))

************Information used for Table 9*********************************
tebalance overid 

*************overlap test used for Figure 2*******************
teoverlap

*********Figure 1************
coefplot p1_f p2_f p3_f p4_f p5_f, bylabel(Female) || (p1_m, label(Publications per year)) (p2_m, label(Citations per year)) (p3_m, label(Average Relative Citation Ratio)) (p4_m, label(Average Field Citation Ratio)) (p5_m, label(Average Altmetrics)), bylabel(Male) xline(0) nolabels









