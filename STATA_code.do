clear all
cls

/****************************************************************************/
/****************************************************************************/
/*------------  Socioeconomic inequalities of long COVID -------------------*/
/****************************************************************************/
/****************************************************************************/


/*------------ Import WP31 data ------------------*/
import delimited "long_covid_symptoms.txt"

/*------------ Encode variables ------------------*/
encode sex, gen(sex_n) 
tab sex_n, nolab
encode ethnicityg, gen(ethnicityg_n)
tab ethnicityg_n, nolab
encode rural_urban, gen(rural_urban_n)
tab rural_urban_n, nolab
encode country, gen(country_n)
tab country_n, nolab

encode patient_client_contact, gen(patient_client_contact_n)
tab patient_client_contact_n, nolab
encode patient_facing, gen(patient_facing_n)
tab patient_facing_n, nolab

*------------ Define Ethnicity ------------------*/
gen eth = "0.White" if ethnicityg_n == 1
replace eth = "1.Non_White" if ethnicityg_n==2 | ethnicityg_n==3 | ethnicityg_n==4
tab eth ethnicityg_n, m
encode eth, gen(eth_n)
tab eth_n ethnicityg_n

*------------ Define Household size ------------------*/
tab hhsize,m
gen hhsize_cat = "1" if hhsize == "1"
replace hhsize_cat = "2" if hhsize == "2" 
replace hhsize_cat = ">=3" if hhsize == "3" | hhsize=="4" | hhsize=="5+"
tab hhsize_cat hhsize, m
encode hhsize_cat, gen(hhsize_n)
tab hhsize_n hhsize 

*------------ Define Time period quarter ------------------*/

gen 	index_date_n = date(index_date, "DMY")
format 	index_date_n %td 

gen year = year(index_date_n)
tab year, m
gen quarter = quarter(index_date_n)
tab quarter, m
tab year quarter, m
gen yr_q = yq(year, quarter)
tab yr_q, m

/*------------ Define Healthcare and patient facing work ------------------*/
gen work_healthcare = (work_sector == "health")
label define yesno 0 "0.No" 1 "1.Yes"
lab values work_healthcare yesno
*lab drop  pt_client_healthc_comb
egen healthc_clientfacing_comb = group(work_healthcare patient_client_contact_n), nolab
tab healthc_clientfacing_comb work_healthcare, m
replace healthc_clientfacing_comb = 1 if mi(healthc_clientfacing_comb) 
* merging non-health non-client and missing/unknown
label define healthcare_ptface 1 "Other or Unknown" 2 "Non-healthcare Client facing" 3 "Healthcare Non-client facing" 4 "Healthcare Client facing", replace
lab val healthc_clientfacing_comb healthcare_ptface


/****************************************************************************/
/*------------         Baseline Table             --------------------------*/
/****************************************************************************/	

baselinetable									/*
	*/ age(cts)									/*
	*/ age(cts tab("p50 (p25-p75)"))			/*
	*/ sex_n(cat)								/*
	*/ eth_n(cat)						        /*
	*/ rural_urban_n(cat)						/*
	*/ hhsize_n(cat)							/*
	*/ health_conditions(cat)					/*
	*/ healthc_clientfacing_comb(cat)			/*
	*/ country_n(cat)							/*
	*/ long_covid_any(cat)						/*
	*/ , by(imd_decile_all, totalcolumn) exportexcel("baseline_table", sheet(1) sheetmodify)
	
baselinetable									/*
	*/ age(cts)									/*
	*/ age(cts tab("p50 (p25-p75)"))			/*
	*/ sex_n(cat)								/*
	*/ eth_n(cat)						/*
	*/ rural_urban_n(cat)						/*
	*/ hhsize_n(cat)							/*
	*/ health_conditions(cat)					/*
	*/ healthc_clientfacing_comb(cat)					/*
	*/ country_n(cat)							/*
	*/ long_covid_any(cat)							/*
	*/ , by(work_sector_n, totalcolumn) exportexcel("Pbaseline_table_4", sheet(1) sheetmodify)
	
	

/*------------ Analysis -------------------------------*/
* UNADJUSTED MODEL
putexcel set long_covid, sheet(unadj) replace
logit long_covid_any ib(10).imd_decile_all, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar, sheet(unadj) modify
margins i.imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


* ADJUSTED MODEL  
putexcel set long_covid, sheet(adj) modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb i.country_n, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar, sheet(adj) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


putexcel set long_covid, sheet(adj_melogit) modify
melogit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb i.country_n, offset(ln_followup_time) || country_n:, vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


* Subgroup: Male
putexcel set long_covid, sheet(male) modify
logit long_covid_any ib(10).imd_decile_all age ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb i.country_n if sex == "0.Male", offset(ln_followup_time) vce(robust) or 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar, sheet(male) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

* Subgroup: Female
putexcel set long_covid, sheet(female) modify
logit long_covid_any ib(10).imd_decile_all age ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb i.country_n if sex == "1.Female", offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar, sheet(female) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

* subgroup: healthcare patient facing
putexcel set long_covid, sheet(health_client) modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.country_n if healthc_clientfacing_comb == 4, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar, sheet(health_client) modify
margins imd_decile_all
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


foreach work in health teaching construc finance info retail civil transport hosp social {
putexcel set long_covid, sheet(`work') modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.country_n if work_sector == "`work'", offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar, sheet(`work') modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

}

	
/*------------ Supplementary Analysis only for ENGLAND----------------------*/
tab country
drop if country != "0.England"

* UNADJUSTED MODEL
putexcel set E_long_covid, sheet(unadj) replace
logit long_covid_any ib(10).imd_decile_all, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set E_long_covid_mar, sheet(unadj) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


* ADJUSTED MODEL  
putexcel set E_long_covid, sheet(adj) modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set E_long_covid_mar, sheet(adj) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


* Subgroup: Male
putexcel set E_long_covid, sheet(male) modify
logit long_covid_any ib(10).imd_decile_all age ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb if sex == "0.Male", offset(ln_followup_time) vce(robust) or 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set E_long_covid_mar, sheet(male) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

* Subgroup: Female
putexcel set E_long_covid, sheet(female) modify
logit long_covid_any ib(10).imd_decile_all age ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb if sex == "1.Female", offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set E_long_covid_mar, sheet(female) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

* subgroup: healthcare patient facing
putexcel set E_long_covid, sheet(health_client) modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q if healthc_clientfacing_comb == 4, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set E_long_covid_mar, sheet(health_client) modify
margins imd_decile_all
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


foreach work in health teaching construc finance info retail civil transport hosp social {
putexcel set E_long_covid, sheet(`work') modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q if work_sector == "`work'", offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set E_long_covid_mar, sheet(`work') modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

}
	
/****************************************************************************/
/****************************************************************************/
/*----------   Sensitivity without self report -----------------------------*/
/****************************************************************************/
/****************************************************************************/

clear all
cls

/*------------ Import WP31 data ------------------*/
import delimited "long_covid_symptoms_wo_selfcovid.txt"

/*------------ Encode variables ------------------*/
encode sex, gen(sex_n) 
tab sex_n, nolab
encode ethnicityg, gen(ethnicityg_n)
tab ethnicityg_n, nolab
encode rural_urban, gen(rural_urban_n)
tab rural_urban_n, nolab
encode country, gen(country_n)
tab country_n, nolab
encode patient_client_contact, gen(patient_client_contact_n)
tab patient_client_contact_n, nolab
encode patient_facing, gen(patient_facing_n)
tab patient_facing_n, nolab


*------------ Define Ethnicity ------------------*/
gen eth = "0.White" if ethnicityg_n == 1
replace eth = "1.Non_White" if ethnicityg_n==2 | ethnicityg_n==3 | ethnicityg_n==4
tab eth ethnicityg_n, m
encode eth, gen(eth_n)
tab eth_n ethnicityg_n

*------------ Define Household size ------------------*/
tab hhsize,m
gen hhsize_cat = "1" if hhsize == "1"
replace hhsize_cat = "2" if hhsize == "2" 
replace hhsize_cat = ">=3" if hhsize == "3" | hhsize=="4" | hhsize=="5+"
tab hhsize_cat hhsize, m
encode hhsize_cat, gen(hhsize_n)
tab hhsize_n hhsize 

*------------ Define Time period quarter ------------------*/

gen 	index_date_n = date(index_date, "DMY")
format 	index_date_n %td 

gen year = year(index_date_n)
tab year, m
gen quarter = quarter(index_date_n)
tab quarter, m
tab year quarter, m
gen yr_q = yq(year, quarter)
tab yr_q, m

/*------------ Define Healthcare and patient facing work ------------------*/
gen work_healthcare = (work_sector == "health")
label define yesno 0 "0.No" 1 "1.Yes"
lab values work_healthcare yesno
*lab drop  pt_client_healthc_comb
egen healthc_clientfacing_comb = group(work_healthcare patient_client_contact_n), nolab
tab healthc_clientfacing_comb work_healthcare, m
replace healthc_clientfacing_comb = 1 if mi(healthc_clientfacing_comb) 
* merging non-health non-client and missing/unknown
label define healthcare_ptface 1 "Other or Unknown" 2 "Non-healthcare Client facing" 3 "Healthcare Non-client facing" 4 "Healthcare Client facing", replace
lab val healthc_clientfacing_comb healthcare_ptface




/*------------ Analysis -------------------------------*/
* UNADJUSTED MODEL
putexcel set long_covid_wo_selfcovid, sheet(unadj) replace
logit long_covid_any ib(10).imd_decile_all, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar_wo_selfcovid, sheet(unadj) modify
margins i.imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable


* ADJUSTED MODEL  
putexcel set long_covid_wo_selfcovid, sheet(adj) modify
logit long_covid_any ib(10).imd_decile_all age ib(1).sex_n ib(first).eth_n i.rural_urban_n i.hhsize_n i.health_conditions yr_q i.healthc_clientfacing_comb i.country_n, offset(ln_followup_time) vce(robust) or
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable

putexcel set long_covid_mar_wo_selfcovid, sheet(adj) modify
margins imd_decile_all 
scal N = e(N)
putexcel (J1) = "sample_size"
putexcel (J4) = N	
putexcel (A1) = etable
