use "C:\Users\wjsca\OneDrive - UNT System\Final_Say-main\output\lcadataMultinomTopics.dta" , clear
//use "C:\Users\wjs0079\OneDrive - UNT System\Final_Say-main\output\lcadataMultinomTopics.dta" , clear

encode ipref, generate(iprefnum)
encode apref, generate(aprefnum)

tab1 top_i top_a

egen topicImax = rowmax(t_1_item t_2_item t_3_item t_4_item t_5_item t_6_item t_7_item)
egen topicAmax = rowmax(t_1_act t_2_act t_3_act t_4_act t_5_act t_6_act t_7_act)

sum *max

//Descriptives
tab top_i [aweight=weight]
tabstat t_1_item t_2_item t_3_item t_4_item t_5_item t_6_item t_7_item  [aweight=weight], stats(mean sd min max)
tabstat t_1_item t_2_item t_3_item t_4_item t_5_item t_6_item t_7_item  [aweight=weight], by(ipref) stats(mean sd min max)
tab top_a [aweight=weight]
tabstat t_1_act t_2_act t_3_act t_4_act t_5_act t_6_act t_7_act  [aweight=weight], stats(mean sd min max)
tabstat t_1_act t_2_act t_3_act t_4_act t_5_act t_6_act t_7_act  [aweight=weight], by(ipref) stats(mean sd min max)

//Mlogits on topic assignment---------------------------------------------------------------------------------------------------------

//ITems
eststo clear
mlogit  top_i i.iprefnum i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age idum [pweight=weight], base(1)
eststo: margins, dydx(iprefnum) post
mlogit  top_i i.iprefnum i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age idum if gender==1 [pweight=weight], base(1)
eststo: margins, dydx(iprefnum) post
mlogit  top_i i.iprefnum i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age idum if gender==2  [pweight=weight], base(1)
eststo: margins, dydx(iprefnum) post
esttab, se r2 compress nogap
esttab using mlogitITEMbygender.csv, b(3) se(3) r2 compress nogap replace

eststo clear
mlogit  top_i i.iprefnum##i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age idum [pweight=weight], base(1)
eststo: margins, dydx(iprefnum) at(relinc=(1(1)3)) post
esttab, b(3) se(3) r2 compress nogap
esttab using mlogitITEM.csv, b(3) se(3) r2 compress nogap replace
	
	
	//visualization
	mlogit  top_i i.iprefnum##i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age [pweight=weight], base(1)
	mgen, at(iprefnum=(1 2) relinc=(1 2 3)) stub(item)
	
		
	foreach t in 1 2 3 4 5 6 7 {
	generate tpos`t' = `t'-.1 if itemiprefnum==1
	replace tpos`t' =  `t'+.1 if itemiprefnum==2	
	}
	fre relinc
	label values itemrelinc relinc
		twoway scatter tpos1 itempr1 if itemiprefnum==1, msym(O) mcolor(black) ||   rcap itemll1 itemul1 tpos1 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos2 itempr2 if itemiprefnum==1, msym(O) mcolor(black)  ||   rcap itemll2 itemul2 tpos2 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos3 itempr3 if itemiprefnum==1, msym(O) mcolor(black)  ||   rcap itemll3 itemul3 tpos3 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos4 itempr4 if itemiprefnum==1, msym(O) mcolor(black)  ||   rcap itemll4 itemul4 tpos4 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos5 itempr5 if itemiprefnum==1, msym(O) mcolor(black)  ||   rcap itemll5 itemul5 tpos5 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos6 itempr6 if itemiprefnum==1, msym(O) mcolor(black)  ||   rcap itemll6 itemul6 tpos6 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos7 itempr7 if itemiprefnum==1, msym(O) mcolor(black) ||   rcap itemll7 itemul7 tpos7 if itemiprefnum==1, horizontal lcolor(black) || ///
			   scatter tpos1 itempr1 if itemiprefnum==2, msym(D) mcolor(gs8)  ||   rcap itemll1 itemul1 tpos1 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			   scatter tpos2 itempr2 if itemiprefnum==2, msym(D) mcolor(gs8) ||   rcap itemll2 itemul2 tpos2 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			   scatter tpos3 itempr3 if itemiprefnum==2, msym(D) mcolor(gs8) ||   rcap itemll3 itemul3 tpos3 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			   scatter tpos4 itempr4 if itemiprefnum==2, msym(D) mcolor(gs8) ||   rcap itemll4 itemul4 tpos4 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			   scatter tpos5 itempr5 if itemiprefnum==2, msym(D) mcolor(gs8) ||   rcap itemll5 itemul5 tpos5 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			   scatter tpos6 itempr6 if itemiprefnum==2, msym(D) mcolor(gs8) ||   rcap itemll6 itemul6 tpos6 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			   scatter tpos7 itempr7 if itemiprefnum==2, msym(D) mcolor(gs8) ||   rcap itemll7 itemul7 tpos7 if itemiprefnum==2, horizontal lcolor(gs8) || ///
			|| , by(itemrelinc, legend(order(1 "Prefer Anthony" 8 "Prefer Michelle")) row(3) graphregion(color(white)) title("{bf:Items:}Predicted Probability of Using topic" "by Preferred Decision Maker and Relative Income", size(medium) color(black))) ///
			ylabel(1 "Give & Take" 2 "Man Final Say" 3 "Accept choice" 4 "Happy Wife" 5 "Taking Turns" 6 "Money Matters" 7 "Work Together", angle(horizontal) ) ///
			xsize(5) ysize(9)  graphregion(color(white)) legend(order(1 "Prefer Anthony" 17 "Prefer Michelle") region(lcolor(white)))
	
//Activity
eststo clear
mlogit  top_a i.aprefnum i.relinc i.organize i.mar i.child i.dur i.activity  i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order adum [pweight=weight], base(1)
eststo: margins, dydx(aprefnum) post
mlogit  top_a i.aprefnum i.relinc i.organize i.mar i.child i.dur i.activity  i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order adum if gender==1 [pweight=weight], base(1)
eststo: margins, dydx(aprefnum) post
mlogit  top_a i.aprefnum i.relinc i.organize i.mar i.child i.dur i.activity  i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order adum if gender==2 [pweight=weight], base(1)
eststo: margins, dydx(aprefnum) post
esttab, se r2 compress nogap
esttab using mlogitACTbygender.csv, b(3) se(3) r2 compress nogap replace

eststo clear
mlogit  top_a i.aprefnum##i.relinc i.organize i.mar i.child i.dur i.activity  i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order i.adum [pweight=weight], base(1)
eststo: margins, dydx(aprefnum) at(relinc=(1(1)3)) post
esttab, b(3) se(3) r2 compress nogap 
esttab using mlogitACT.csv, b(3) se(3) r2 compress nogap replace
	
	//Visualization
	mlogit  top_a i.aprefnum##i.relinc i.organize i.mar i.child i.dur i.activity  i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order [pweight=weight], base(1)
	mgen, at(aprefnum=(1 2) relinc=(1 2 3)) stub(act)
	
	
	foreach t in 1 2 3 4 5 6 7 {
	generate atpos`t' = `t'-.1 if actaprefnum==1
	replace atpos`t' =  `t'+.1 if actaprefnum==2	
	}
	fre relinc
	label values actrelinc relinc
		twoway scatter atpos1 actpr1 if actaprefnum==1, msym(O) mcolor(black) ||   rcap actll1 actul1 atpos1 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos2 actpr2 if actaprefnum==1, msym(O) mcolor(black)  ||   rcap actll2 actul2 atpos2 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos3 actpr3 if actaprefnum==1, msym(O) mcolor(black)  ||   rcap actll3 actul3 atpos3 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos4 actpr4 if actaprefnum==1, msym(O) mcolor(black)  ||   rcap actll4 actul4 atpos4 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos5 actpr5 if actaprefnum==1, msym(O) mcolor(black)  ||   rcap actll5 actul5 atpos5 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos6 actpr6 if actaprefnum==1, msym(O) mcolor(black)  ||   rcap actll6 actul6 atpos6 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos7 actpr7 if actaprefnum==1, msym(O) mcolor(black) ||   rcap actll7 actul7 atpos7 if actaprefnum==1, horizontal lcolor(black) || ///
			   scatter atpos1 actpr1 if actaprefnum==2, msym(D) mcolor(gs8)  ||   rcap actll1 actul1 atpos1 if actaprefnum==2, horizontal lcolor(gs8) || ///
			   scatter atpos2 actpr2 if actaprefnum==2, msym(D) mcolor(gs8) ||   rcap actll2 actul2 atpos2 if actaprefnum==2, horizontal lcolor(gs8) || ///
			   scatter atpos3 actpr3 if actaprefnum==2, msym(D) mcolor(gs8) ||   rcap actll3 actul3 atpos3 if actaprefnum==2, horizontal lcolor(gs8) || ///
			   scatter atpos4 actpr4 if actaprefnum==2, msym(D) mcolor(gs8) ||   rcap actll4 actul4 atpos4 if actaprefnum==2, horizontal lcolor(gs8) || ///
			   scatter atpos5 actpr5 if actaprefnum==2, msym(D) mcolor(gs8) ||   rcap actll5 actul5 atpos5 if actaprefnum==2, horizontal lcolor(gs8) || ///
			   scatter atpos6 actpr6 if actaprefnum==2, msym(D) mcolor(gs8) ||   rcap actll6 actul6 atpos6 if actaprefnum==2, horizontal lcolor(gs8) || ///
			   scatter atpos7 actpr7 if actaprefnum==2, msym(D) mcolor(gs8) ||   rcap actll7 actul7 atpos7 if actaprefnum==2, horizontal lcolor(gs8) || ///
			|| , by(actrelinc, legend(order(1 "Prefer Anthony" 8 "Prefer Michelle")) row(3) graphregion(color(white)) title("{bf:Activity: }Predicted Probability of Using topic" "by Preferred Decision Maker and Relative Income", size(medium) color(black))) ///
			ylabel(1 "Give & Take" 2 "Man Final Say" 3 "Accept choice" 4 "Happy Wife" 5 "Taking Turns" 6 "Money Matters" 7 "Work Together", angle(horizontal) ) ///
			xsize(5) ysize(9)  graphregion(color(white)) legend(order(1 "Prefer Anthony" 17 "Prefer Michelle") region(lcolor(white)))

			
//Robustness Test, OLS on topic proportions---------------------------------------------------------------------------------------------------------

//Item
eststo clear
foreach t in 1 2 3 4 5 6 7 {
quietly: reg t_`t'_item i.iprefnum i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order [pweight=weight]
	eststo: margins, dydx(iprefnum)  post
}
esttab, se r2 compress nogap

eststo clear
foreach t in 1 2 3 4 5 6 7 {
reg t_`t'_item i.iprefnum##i.relinc i.organize i.mar i.child i.dur i.item i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order [pweight=weight]
	eststo: margins, dydx(iprefnum) at(relinc=(1(1)3))  post
}
esttab, se r2 compress nogap
esttab using item_topic_OLS.csv, b(3) se(3)  r2 compress nogap replace
			
//Activity			
eststo clear
foreach t in 1 2 3 4 5 6 7 {
quietly: reg t_`t'_act i.iprefnum i.relinc i.organize i.mar i.child i.dur i.activity i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age i.order [pweight=weight]
	eststo: margins, dydx(iprefnum)  post
}
esttab, se r2 compress nogap
						
						
eststo clear
foreach t in 1 2 3 4 5 6 7 {
reg t_`t'_act i.iprefnum##i.relinc i.organize i.mar i.child i.dur i.activity i.gender i.relate i.parent i.raceeth i.educ i.employ i.incdum age  i.order[pweight=weight]
	eststo: margins, dydx(iprefnum) at(relinc=(1(1)3))  post
}
esttab, se r2 compress nogap
esttab using act_topic_OLS.csv, b(3) se(3)  r2 compress nogap replace
						
			
			
			
			
			
//linking topics to full qual responses

use "C:\Users\wjs0079\OneDrive - UNT System\Final_Say-main\output\lcadataMultinomTopics.dta" , clear


keep t_* CaseID qual*

export excel using "C:\Users\wjs0079\OneDrive - UNT System\Final_Say-main\output\qualresponses.xlsx", firstrow(variables) replace			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			