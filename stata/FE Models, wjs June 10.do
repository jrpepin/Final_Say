clear

//global outDir "CHANGE THIS TO YOUR OUTDIR FILEPATH"
global outDir "C:/Users/wjsca/OneDrive - UNT System/Final_Say-main/output"
//global outDir "C:/Users/wjs0079/OneDrive - UNT System/Final_Say-main/output"

// Import the data that was exported from FS_02_quant analyses.R
use "$outDir/femodels.dta" , clear

foreach var in ifair afair {
replace `var' = 5-`var'
label drop `var'
label define `var'lab 1 "very unfair" 2 "seomwhat unfair" 3 "somehwat fair" 4 "very fair"
label values `var' `var'lab
}
tab2 idum ifair, nol
tab2 adum afair, nol

	//Reshaping and coding
		rename perI per1
		rename idum dum1
		rename ifair fair1
		rename perA per2
		rename adum dum2
		rename afair fair2

		reshape long per dum fair, i(CaseID) j(decision)
		tab per
		
		destring CaseID, generate(CaseIDnum)

		label define per 0 "Anthony" 1 "Michelle" 
		label values per per
		label define dum 0 "Disagree" 1 "Agree"
		label values dum dum
		label define decision 1 "Items" 2 "Activity"
		label values decision decision
		tab1 per dum decision

		
save "$outDir/quantdatalong", replace

//------------------------------------------------------------------------------------------------------------
//Linear Probability Model using Fair/Unfair Outcome
			
///LPMs
use "$outDir/quantdatalong", clear

//Balanced data
bys CaseID: egen caseN = count(CaseID)
keep if caseN==2

	//Overall and By Gender
			eststo clear
			xtset CaseIDnum
			xtreg dum i.per##i.decision  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender == 1  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender == 2  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision	
			esttab, se r2 b(3) se(3) compress nogap replace
			esttab using "$outDir/lpmbygender.csv", b(3) se(3) compress nogap replace	
	
			//Testing Gender differnces in coefficients (note, this model takes forever to converge....)
			quietly: reg dum i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) 
				margins i.decision, dydx(per) over(gender) post
					test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
					test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision

							
			
	
	//By Relative Income
			eststo clear
			xtset CaseIDnum
			xtreg dum i.per##i.decision if relinc == 1  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if relinc == 2  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if relinc == 3  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision	
			esttab, se r2 b(3) se(3) compress nogap replace
			esttab using "$outDir/lpmbyrelativeincome.csv", b(3) se(3) compress nogap replace
	
	//By gender and relative income
			eststo clear
			xtset CaseIDnum
			xtreg dum i.per##i.decision if gender==1	& relinc == 1  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender==1	& relinc == 2  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender==1	& relinc == 3  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender==2	& relinc == 1 , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender==2	& relinc == 2  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision
			xtreg dum i.per##i.decision if gender==2	& relinc == 3 , fe
				eststo: margins decision, dydx(per) post			
				test [1.per]1.decision=[1.per]2.decision

				esttab, se r2 b(3) se(3) compress nogap replace
				esttab using "$outDir/temp.csv", b(3) se(3) compress nogap replace
	
	
			//Testing Gender differnces in coefficients
			quietly: reg dum i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) if relinc==1
				margins i.decision, dydx(per) over(gender) post
					test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
					test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision
			quietly: reg dum i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) if relinc==2
				margins i.decision, dydx(per) over(gender) post
					test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
					test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision
			quietly: reg dum i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) if relinc==3
				margins i.decision, dydx(per) over(gender) post
					test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
					test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision
							
		
		
	//visualization
			xtset CaseIDnum
			xtreg dum i.per##i.decision if gender==1	& relinc == 1 , fe
				mgen, at(per=(0 1) decision=(1 2)) stub(g1r1)
			xtreg dum i.per##i.decision if gender==2	& relinc == 1 , fe
				mgen, at(per=(0 1) decision=(1 2)) stub(g2r1)
			xtreg dum i.per##i.decision if gender==1	& relinc == 2 , fe
				mgen, at(per=(0 1) decision=(1 2)) stub(g1r2)
			xtreg dum i.per##i.decision if gender==2	& relinc == 2 , fe
				mgen, at(per=(0 1) decision=(1 2)) stub(g2r2)
			xtreg dum i.per##i.decision if gender==1	& relinc == 3 , fe
				mgen, at(per=(0 1) decision=(1 2)) stub(g1r3)
			xtreg dum i.per##i.decision if gender==2	& relinc == 3, fe
				mgen, at(per=(0 1) decision=(1 2)) stub(g2r3)
				
			replace g2r1per = g2r1per+3
			replace g2r2per = g2r2per+3
			replace g2r3per = g2r3per+3			
			
			
			
			label define g1r1decision 1 "High Stakes" 2 "Low Stakes"
			foreach val in g1r1 g1r2 g1r3 g2r1 g2r2 g2r3 {
			    label values `val'decision g1r1decision
			}
		format 	*xb %3.2f
			
			twoway bar 	g1r1xb g1r1per if g1r1per==0, barw(1)  bcolor(gs2)  || ///
				   bar 	g1r1xb g1r1per if g1r1per==1, barw(1)  bcolor(gs7)  || ///
				   bar 	g2r1xb g2r1per if g2r1per==3, barw(1)  bcolor(gs2)  || ///
				   bar 	g2r1xb g2r1per if g2r1per==4, barw(1)  bcolor(gs7)  || ///
				   scatter g1r1xb g1r1per, msym(none) mlabel(g1r1xb) mlabcolor(black) mlabpos(12) mlabsize(small) || ///
				   scatter g2r1xb g2r1per, msym(none) mlabel(g2r1xb) mlabcolor(black) mlabpos(12) mlabsize(small) || ///
				   , by(g1r1decision, ///
					col(1) ///
					title("Man High Earner", pos(12) color(black) size(medsmall)) ///
					graphregion(color(white)) ///
					note(" ") ///
					) ///
					legend(order(1 "Anthony Decides" 2 "Michelle Decides") symxsize(*.25) row(1) pos(6) region(lcolor(white))) ///
					///yscale(range(1 4)) ///
					xlabel(.5 "Men" 3.5 "Women", angle(horizontal))  ///
					ylabel(0 .5 1, angle(horizontal)) ///
					ytitle(" ") xtitle(" ") ///
					name(r1, replace)
			
			twoway bar 	g1r2xb g1r2per if g1r2per==0, barw(1)  bcolor(gs2)  || ///
				   bar 	g1r2xb g1r2per if g1r2per==1, barw(1)  bcolor(gs7)  || ///
				   bar 	g2r2xb g2r2per if g2r2per==3, barw(1)  bcolor(gs2)  || ///
				   bar 	g2r2xb g2r2per if g2r2per==4, barw(1)  bcolor(gs7)  || ///
				   scatter g1r2xb g1r2per, msym(none) mlabel(g1r2xb) mlabcolor(black) mlabpos(12) mlabsize(small) || ///
				   scatter g2r2xb g2r2per, msym(none) mlabel(g2r2xb) mlabcolor(black) mlabpos(12) mlabsize(small) || ///
				   , by(g1r2decision, ///
					col(1) ///
					title("Woman High Earner", pos(12) color(black) size(medsmall)) ///
					graphregion(color(white)) ///
					note(" ") ///
					legend(off) ///
					) ///
					legend(off) ///
					legend(order(1 "Anthony Decides" 2 "Michelle Decides") row(1) pos(6) region(lcolor(white))) ///
					///yscale(range(1 4)) ///
					xlabel(.5 "Men" 3.5 "Women", angle(horizontal))  ///
					ylabel(0 .5 1, angle(horizontal)) ///
					ytitle(" ") xtitle(" ") ///
					name(r2, replace)
			
			
		twoway     bar 	g1r3xb g1r3per if g1r3per==0, barw(1)  bcolor(gs2)  || ///
				   bar 	g1r3xb g1r3per if g1r3per==1, barw(1)  bcolor(gs7)  || ///
				   bar 	g2r3xb g2r3per if g2r3per==3, barw(1)  bcolor(gs2)  || ///
				   bar 	g2r3xb g2r3per if g2r3per==4, barw(1)  bcolor(gs7)  || ///
				   scatter g1r3xb g1r3per, msym(none) mlabel(g1r3xb) mlabcolor(black) mlabpos(12) mlabsize(small) || ///
				   scatter g2r3xb g2r3per, msym(none) mlabel(g2r3xb) mlabcolor(black) mlabpos(12) mlabsize(small) || ///
				   , by(g1r3decision, ///
					col(1) ///
					title("Equal Earner", pos(12) color(black) size(medsmall)) ///
					graphregion(color(white)) ///
					note(" ") ///
					legend(off) ///
					) ///
					legend(off) ///
					///legend(order(1 "Anthony Decides" 2 "Michelle Decides") row(1) pos(6) region(lcolor(white))) ///
					///yscale(range(1 4)) ///
					xlabel(.5 "Men" 3.5 "Women", angle(horizontal))  ///
					ylabel(0 .5 1, angle(horizontal)) ///
					ytitle(" ") xtitle(" ") ///
					name(r3, replace)
					
			grc1leg r1 r2 r3, row(1) graphregion(color(white)) title("Probability of Viewing Decision as Fair", color(black) size(medium))
			 graph display, xsize(8) ysize(5)
			
			



//------------------------------------------------------------------------------------------------------------
//FE models with continous scale of fairness as outcome
use "$outDir/quantdatalong", clear
	//can only include controls with within-respondent variation
		xtset CaseIDnum
		xtsum  relinc organize mar child dur gender relate parent raceeth educ employ incdum age dum per decision fair
			//only "dum", "per", "dec", and "fair" have within-respondent variation for FE models

		
		//by Relative Income
			eststo clear
			xtset CaseIDnum
			xtreg fair i.per##i.decision if relinc == 1  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision //*
			xtreg fair i.per##i.decision if relinc == 2  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision //*
			xtreg fair i.per##i.decision if relinc == 3  , fe
				eststo: margins decision, dydx(per) post		
				test [1.per]1.decision=[1.per]2.decision //*
			esttab, se r2 b(3) se(3) compress nogap replace
			esttab using "$outDir/fullsample.csv", b(3) se(3) compress nogap replace
	
		
		//By Gender & Relative Income
			eststo clear
			xtset CaseIDnum
			xtreg fair i.per##i.decision if gender==1	& relinc == 1  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision //*
			xtreg fair i.per##i.decision if gender==1	& relinc == 2  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision //*
			xtreg fair i.per##i.decision if gender==1	& relinc == 3  , fe
				eststo: margins decision, dydx(per) post				
				test [1.per]1.decision=[1.per]2.decision //*
			xtreg fair i.per##i.decision if gender==2	& relinc == 1  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision //n.s.
			xtreg fair i.per##i.decision if gender==2	& relinc == 2  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision //*
			xtreg fair i.per##i.decision if gender==2	& relinc == 3  , fe
				eststo: margins decision, dydx(per) post
				test [1.per]1.decision=[1.per]2.decision //*		
			esttab, se r2 b(3) se(3) compress nogap replace
			esttab using "$outDir/temp.csv", b(3) se(3) compress nogap replace
	

		//Testing Gender differnces in coefficients
		quietly: reg fair i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) if relinc==1
			margins i.decision, dydx(per) over(gender) post
				test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
				test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision
		quietly: reg fair i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) if relinc==2
			margins i.decision, dydx(per) over(gender) post
				test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
				test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision
		quietly: reg fair i.gender##(i.per i.decision i.per#i.decision i.CaseIDnum) if relinc==3
			margins i.decision, dydx(per) over(gender) post
				test [1.per]1.gender#1.decision = [1.per]2.gender#1.decision
				test [1.per]1.gender#2.decision = [1.per]2.gender#2.decision
						


			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			


			
