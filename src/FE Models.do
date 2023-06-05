clear

global outDir "CHANGE THIS TO YOUR OUTDIR FILEPATH"

// Import the data that was exported from FS_02_quant analyses.R
use "$outDir/femodels.dta" 


// Confirming logit models in paper:
	eststo clear
	local controls = "i.relinc   i.organize   i.mar  i.child   i.dur i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age"
	logit idum perI `controls' i.item [pweight=weight]
		eststo: margins, dydx(perI relinc) post
	logit adum perA `controls' i.activity i.order [pweight=weight]
		eststo: margins, dydx(perA relinc) post
	logit idum i.perI `controls' i.item i.perI#i.relinc [pweight=weight]
		eststo: margins perI, dydx(relinc) post
	logit adum perA `controls' i.activity i.order i.perA#i.relinc [pweight=weight]
		eststo: margins perA, dydx(relinc) post
	esttab, se r2 compress nogap

				
// Confirming models if we just do vacay versus weekend activity
preserve
	keep if item==3 & activity==1
	eststo clear
	local controls = "i.relinc   i.organize   i.mar  i.child   i.dur i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age"
	logit idum perI `controls' i.item [pweight=weight]
		eststo: margins, dydx(perI relinc) post
	logit adum perA `controls' i.activity i.order [pweight=weight]
		eststo: margins, dydx(perA relinc) post
	logit idum i.perI `controls' i.item i.perI#i.relinc [pweight=weight]
		eststo: margins perI, dydx(relinc) post
	logit adum perA `controls' i.activity i.order i.perA#i.relinc [pweight=weight]
		eststo: margins perA, dydx(relinc) post
	esttab, se r2 compress nogap
restore
	
//FE or HLMs

	//Reshaping and coding
		rename perI per1
		rename idum dum1
		rename perA per2
		rename adum dum2

		reshape long per dum, i(CaseID) j(decision)
		tab per
		
		destring CaseID, generate(CaseIDnum)

		label define per 0 "Anthony" 1 "Michelle" 
		label values per per
		label define dum 0 "Disagree" 1 "Agree"
		label values dum dum
		label define decision 1 "Items" 2 "Activity"
		label values decision decision
		tab1 per dum decision

	//can only include controls with within-respondent variation
		xtset CaseIDnum
		xtsum  relinc organize mar child dur gender relate parent raceeth educ employ incdum age dum per decision
			//only "dum", "per", "dec" have within-respondent variation for FE models

		
	//logit FEs. Note that sample size is way smaller becuase keeps only resopndents with variaiton in outcome.
	xtlogit dum i.per##i.decision [iweight=weight], fe
		margins decision, dydx(per)
		test 1.decision=2.decision
					
	//LPM FEs
	xtset CaseIDnum
	xtreg dum i.per##i.decision [aweight=weight], fe
		margins decision, dydx(per)
		test 1.decision=2.decision
		

	//does this hold for vacay versus weekend activity?
		keep if item==3
		keep if activity==1
		bys CaseIDnum: egen idN = count(CaseIDnum)
		keep if idN==2
		tab2 item activity
		
		
			//LPM FEs
			xtset CaseIDnum
			xtreg dum i.per##i.decision [aweight=weight], fe
				margins decision, dydx(per)
				
			//logit FEs. Note that sample size is way smaller becuase keeps only resopndents with variaiton in outcome.
			xtlogit dum i.per##i.decision [iweight=weight], fe
				margins decision, dydx(per)
				