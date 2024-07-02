//Predicting topics used to explain fairness decision


//------------------------------------------------------------------------------------------------------------
//Importing and shaping data
	use "$outDir/lcadataMultinomTopics", clear

//structure of data = 2 obs per respondent, but still in wide form. need to fix that...
	destring CaseID, replace


	collapse (firstnm) t_1_item (firstnm) t_2_item (firstnm) t_3_item (firstnm) t_4_item (firstnm) t_5_item (firstnm) t_6_item (firstnm) t_7_item ///
			(firstnm) t_1_act (firstnm) t_2_act (firstnm) t_3_act (firstnm) t_4_act (firstnm) t_5_act (firstnm) t_6_act (firstnm) t_7_act ///
			(firstnm) top_i (firstnm) top_a ///
			, by(CaseID)
			
	generate wide = 1
			
	save "$outDir/wideTopics.dta", replace


	use "$outDir/lcadataMultinomTopics", clear

	destring CaseID, replace

	merge m:1 CaseID using "$outDir/wideTopics.dta", update replace
	drop longid //(used to identify respondent-decision units

	xtset CaseID
	xtsum * //no within-person variation in any items

	egen tag = tag(CaseID)

	keep if tag==1
	

	save "$outDir/wideTopicsFull.dta", replace

//Reshaping to long form.
	use "$outDir/wideTopicsFull.dta", clear

	generate perI = 2-iperson
	generate perA = 2-aperson
	label define michelleref 0 "Anthony" 1 "Michelle"
	label values perI michelleref
	label values perA michelleref


	foreach var in ifair afair {
	replace `var' = 5-`var'
	label drop `var'
	label define `var'lab 1 "very unfair" 2 "seomwhat unfair" 3 "somehwat fair" 4 "very fair"
	label values `var' `var'lab
	}
	tab2 idum ifair, nol
	tab2 adum afair, nol


	rename t_* t*
	rename *_item *_1
	rename *_act *_2
	rename top_i top_1
	rename top_a top_2
	rename perI per1
	rename idum dum1
	rename ifair fair1
	rename perA per2
	rename adum dum2
	rename afair fair2
	encode ipref, generate(pref1)
	encode apref, generate(pref2)


			reshape long per dum fair qual t1_ t2_ t3_ t4_ t5_ t6_ t7_ top_ pref, i(CaseID) j(decision)
			tab per
			
			label define dum 0 "Disagree" 1 "Agree"
			label values dum dum
			label define decision 1 "Items" 2 "Activity"
			label values decision decision
			tab1 per dum decision

	foreach topic in t1 t2 t3 t4 t5 t6 t7 {
	    rename `topic'_ `topic'
	}

	save "$outDir/longTopicsFull.dta", replace

//------------------------------------------------------------------------------------------------------------
//Models	
//By Relinc
use "$outDir/longTopicsFull.dta", clear

//keeping only balanced data
bys CaseID: egen caseN = count(CaseID)
keep if caseN==2

program define relincAll3
	args dv
	xtset CaseID
			foreach relinc in 1 2 3 {
			xtreg `dv' i.dum i.per i.decision i.dum#i.per i.dum#i.decision i.per#i.decision i.dum#i.per#i.decision if relinc==`relinc', fe
				eststo, title(`dv'): margins i.decision#i.per, dydx(dum) post
			}	
	end			

		eststo clear
		relincAll3 t1 
		relincAll3 t2 
		relincAll3 t3
		relincAll3 t4
		relincAll3 t5 
		relincAll3 t6 
		relincAll3 t7
		esttab using topicmodelsrelincAll3.csv , se compress nogap mtitles replace		
		
		
		
	
//By gender and relinc
use "$outDir/longTopicsFull.dta", clear
		
program define relinc1
	args dv
	xtset CaseID
			foreach gender in 1 2 {
			xtreg `dv' i.dum i.per i.decision i.dum#i.per i.dum#i.decision i.per#i.decision i.dum#i.per#i.decision if gender==`gender' & relinc==1, fe
				eststo, title(`dv'): margins i.decision#i.per, dydx(dum) post
			}	
	end	
program define relinc2
	args dv
	xtset CaseID
			foreach gender in 1 2 {
			xtreg `dv' i.dum i.per i.decision i.dum#i.per i.dum#i.decision i.per#i.decision i.dum#i.per#i.decision if gender==`gender' & relinc==2, fe
				eststo, title(`dv'): margins i.decision#i.per, dydx(dum) post
			}	
	end			
program define relinc3
	args dv
	xtset CaseID
			foreach gender in 1 2 {
			xtreg `dv' i.dum i.per i.decision i.dum#i.per i.dum#i.decision i.per#i.decision i.dum#i.per#i.decision if gender==`gender' & relinc==3, fe
				eststo, title(`dv'): margins i.decision#i.per, dydx(dum) post
			}	
	end	
		
	
		eststo clear
		relinc1 t1 
		relinc1 t2 
		relinc1 t3
		relinc1 t4
		relinc1 t5 
		relinc1 t6 
		relinc1 t7
		esttab using topicmodelsrelinc1.csv , se compress nogap mtitles replace		
			
		eststo clear
		relinc2 t1 
		relinc2 t2 
		relinc2 t3
		relinc2 t4
		relinc2 t5 
		relinc2 t6 
		relinc2 t7
		esttab using topicmodelsrelinc2.csv , se compress nogap mtitles replace		
						
		eststo clear
		relinc3 t1 
		relinc3 t2 
		relinc3 t3
		relinc3 t4
		relinc3 t5 
		relinc3 t6 
		relinc3 t7
		esttab using topicmodelsrelinc3.csv , se compress nogap mtitles replace		

			
			
//Testing differences between Coefficients. Cannot use suest in xtreg so using factor variable notation		
	program define Btest
		args dv relinc
				quietly: reg `dv' i.gender##(c.fair i.per i.decision c.fair#i.per c.fair#i.decision i.per#i.decision c.fair#i.per#i.decision i.CaseID) if relinc==`relinc', 
					margins i.per#i.decision, dydx(fair) over(gender) post	
						test 1.gender#0.per#1.decision = 2.gender#0.per#1.decision
						test 1.gender#0.per#2.decision = 2.gender#0.per#2.decision
						test 1.gender#1.per#1.decision = 2.gender#1.per#1.decision
						test 1.gender#1.per#2.decision = 2.gender#1.per#2.decision	
					
		end				
			
			Btest t1 1
			Btest t1 2
			Btest t1 3
			
			Btest t2 1
			Btest t2 2
			Btest t2 3
			
			Btest t3 1
			Btest t3 2
			Btest t3 3
			
			Btest t4 1
			Btest t4 2
			Btest t4 3
			
			Btest t5 1
			Btest t5 2
			Btest t5 3
			
			Btest t6 1
			Btest t6 2
			Btest t6 3
			
			Btest t7 1
			Btest t7 2
			Btest t7 3
			

//Data Visualization
	use "$outDir/longTopicsFull.dta", clear


program mgenpoints
	args gender relinc
	use "$outDir/longTopicsFull.dta", clear
	xtset CaseID
		foreach dv in t1 t2 t3 t4 t5 t6 t7 {
			xtreg `dv' i.dum i.per i.decision i.dum#i.per i.dum#i.decision i.per#i.decision i.dum#i.per#i.decision if gender==`gender' & relinc==`relinc', fe
				mgen, at(dum=(0 1) per=(0 1) decision=(1 2)) stub(`dv'P)
		}
		
	keep *xb *per *dum *decision
	drop decision incdum dum per
	

generate cat = .
replace cat = 1 if t1Pper==0 & t1Pdecision==1 & t1Pdum==1 //antony, high stakes, fair
replace cat = 2 if t1Pper==0 & t1Pdecision==1 & t1Pdum==0 //antony, high stakes, unfair
replace cat = 3 if t1Pper==1 & t1Pdecision==1 & t1Pdum==1 //michelle, high stakes, fair
replace cat = 4 if t1Pper==1 & t1Pdecision==1 & t1Pdum==0 //michelle, high stakes, unfair

replace cat = 5 if t1Pper==0 & t1Pdecision==2 & t1Pdum==1 //antony, low stakes, fair
replace cat = 6 if t1Pper==0 & t1Pdecision==2 & t1Pdum==0 //antony, low stakes, unfair
replace cat = 7 if t1Pper==1 & t1Pdecision==2 & t1Pdum==1 //michelle, low stakes, fair
replace cat = 8 if t1Pper==1 & t1Pdecision==2 & t1Pdum==0 //michelle, low stakes, unfair

	
save gender`gender'relinc`relinc'MgenPoints.dta, replace
end				
	
	mgenpoints 1 1
	mgenpoints 1 2
	mgenpoints 1 3
	mgenpoints 2 1
	mgenpoints 2 2
	mgenpoints 2 3	
	

	
	
	
	
//Men respondents---------------------------------------------------------------
	//man higher earner	
		use gender1relinc1MgenPoints, clear
			
		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==1, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(vsmall) row(1) region(lcolor(white))) ///
				title("High Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(highstakes, replace)	

		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==2, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(small) row(1) region(lcolor(white))) ///
				title("Low Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(lowstakes, replace)	
				
		grc1leg highstakes lowstakes, col(1) graphregion(color(white)) ///
			xsize(5) ysize(7) scale(*.9) ///
			title("Man Higher Earner", color(black))
		graph display, xsize(5) ysize(7) name(g1r1men, replace)
		
	//woman higher earner
		use gender1relinc2MgenPoints, clear
			
		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==1, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(vsmall) row(1) region(lcolor(white))) ///
				title("High Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(highstakes, replace)	

		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==2, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(small) row(1) region(lcolor(white))) ///
				title("Low Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(lowstakes, replace)	
				
		grc1leg highstakes lowstakes, col(1) graphregion(color(white)) ///
			xsize(5) ysize(7) scale(*.9) ///
			title("Woman Higher Earner", color(black))
		graph display, xsize(5) ysize(7) name(g1r2men, replace)
								
	
	//Equal earner
		use gender1relinc3MgenPoints, clear
			
		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==1, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(vsmall) row(1) region(lcolor(white))) ///
				title("High Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(highstakes, replace)	

		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==2, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(small) row(1) region(lcolor(white))) ///
				title("Low Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(lowstakes, replace)	
				
		grc1leg highstakes lowstakes, col(1) graphregion(color(white)) ///
			xsize(5) ysize(7) scale(*.9) ///
			title("Equal Earner", color(black))
		graph display, xsize(5) ysize(7) name(g1r3men, replace)
									
	
	grc1leg g1r1men g1r2men g1r3men, row(1) graphregion(color(white)) title("Men Respondents", color(black))
		graph display, xsize(9) ysize(5)
	
	
//Women respondents-------------------------------------------------------------
	//man higher earner	
		use gender2relinc1MgenPoints, clear
			
		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==1, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(vsmall) row(1) region(lcolor(white))) ///
				title("High Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(highstakes, replace)	

		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==2, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(small) row(1) region(lcolor(white))) ///
				title("Low Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(lowstakes, replace)	
				
		grc1leg highstakes lowstakes, col(1) graphregion(color(white)) ///
			xsize(5) ysize(7) scale(*.9) ///
			title("Man Higher Earner", color(black))
		graph display, xsize(5) ysize(7) name(g1r1women, replace)
		
	//woman higher earner
		use gender2relinc2MgenPoints, clear
			
		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==1, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(vsmall) row(1) region(lcolor(white))) ///
				title("High Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(highstakes, replace)	

		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==2, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(small) row(1) region(lcolor(white))) ///
				title("Low Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(lowstakes, replace)	
				
		grc1leg highstakes lowstakes, col(1) graphregion(color(white)) ///
			xsize(5) ysize(7) scale(*.9) ///
			title("Woman Higher Earner", color(black))
		graph display, xsize(5) ysize(7) name(g1r2women, replace)
								
	
	//Equal earner
		use gender2relinc3MgenPoints, clear
			
		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==1, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(vsmall) row(1) region(lcolor(white))) ///
				title("High Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(highstakes, replace)	

		graph hbar t1Pxb t2Pxb t3Pxb t4Pxb t5Pxb t6Pxb t7Pxb if t1Pdecision==2, stack blabel(bar, pos(center) format(%3.2f)) ///
				over(cat, relabel(1 `""Fair, Anthony""Decides""' 2 `""Unair, Anthony""Decides""' 3 `""Fair, Michelle""Decides""' 4 `""Unair, Michelle""Decides""')) ///
				legend(order(1 "Acquies" 2 "Man Final Say" 3 "Pract. Effic." 4 "Happy Wife" 5 "Turns" 6 "Money" 7 "Work2Geth") ///
					symxsize(*.25) size(small) row(1) region(lcolor(white))) ///
				title("Low Stakes Decisions", color(black)) ///
				graphregion(color(white)) ///
				xsize(7.5) ysize(4) ///
				name(lowstakes, replace)	
				
		grc1leg highstakes lowstakes, col(1) graphregion(color(white)) ///
			xsize(5) ysize(7) scale(*.9) ///
			title("Equal Earner", color(black))  
		graph display, xsize(5) ysize(7) name(g1r3women, replace)
									
	
	grc1leg g1r1women g1r2women g1r3women, row(1) graphregion(color(white)) title("Women Respondents", color(black))
		graph display, xsize(9) ysize(5)
	
		
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	