//////////////////////////////////////////////////
//Getting quotes for paper

global outDir "C:/Users/wjsca/OneDrive - UNT System/Final_Say-main/output"
//Practical efficiency
use "$outDir/lcadataMultinomTopics", clear

keep if idum==1 |adum==1 //fair

order t_3_item qual1 iperson
gsort- t_3_item

//high-stakes, anthony deicdes, man final say
use "$outDir/lcadataMultinomTopics", clear


keep if iperson==2 //anthony
keep if idum==1 //fair

order t_2_item qual1 iperson
gsort- t_2_item



//low-stakes, michelle deicdes, man final say
use "$outDir/lcadataMultinomTopics", clear


keep if aperson==1 //michelle
keep if adum==1 //fair

order t_2_act qual2 aperson
gsort- t_2_act


//happy wife happy life, low-stakes
use "$outDir/lcadataMultinomTopics", clear

keep if idum==1 |adum==1 //fair

keep if aperson==1 //michelle

order t_4_item t_4_act qual1 qual2 iperson
gsort- t_4_act

//hapy wife happy life, high stakes
use "$outDir/lcadataMultinomTopics", clear

keep if idum==1 //fair

keep if iperson==1 //michelle

order t_4_item t_2_item qual1 qual2 iperson
gsort- t_4_item -t_2_item

keep if t_4_item>.2 & t_2_item>.2

//Work together, high stakes

use "$outDir/lcadataMultinomTopics", clear

keep if idum==0 //unfair

order t_7_item t_7_act qual1 qual2 iperson
gsort- t_7_item 

//taking turns
order activity
gsort- t_5_act

//Give & Take
//Man's decision on high-stakes unfair
use "$outDir/lcadataMultinomTopics", clear

keep if idum==0 //unfair
keep if iperson==2 //anthony
keep if relinc==1

order t_1_item  qual1 qual2 iperson
gsort- t_1_item 


//Give & Take
//Woman's decision on low-stakes unfair, man higher earner
use "$outDir/lcadataMultinomTopics", clear

keep if adum==0 //unfair
keep if aperson==1 //michelle
keep if relinc==1

order relinc iperson activity t_1_act   qual2 iperson
gsort- t_1_act


//Woman's decision on low-stakes unfair, woman higher earner
use "$outDir/lcadataMultinomTopics", clear

keep if adum==0 //unfair
keep if aperson==1 //michelle
keep if relinc==2

order relinc aperson activity t_1_act   qual2 iperson
gsort- t_1_act

//mans's decision on low-stakes unfair, woman higher earner
use "$outDir/lcadataMultinomTopics", clear

keep if adum==0 //unfair
keep if aperson==2 //antohny
keep if relinc==2

order relinc aperson activity t_1_act   qual2 iperson
gsort- t_1_act




//money matters, michelle decides, unfair, man higher earner, high stakes
use "$outDir/lcadataMultinomTopics", clear

keep if idum==0 //unfair
keep if iperson==1 //michelle
keep if relinc==1

order relinc iperson item t_6_item   qual1 iperson
gsort- t_6_item


//money matters, anthony decides, unfair, woman higher earner, high stakes
use "$outDir/lcadataMultinomTopics", clear

keep if idum==0 //unfair
keep if iperson==2 //anthony
keep if relinc==2

order relinc iperson item t_6_item   qual1 iperson
gsort- t_6_item



//money matters, unfair, equal earner, high stakes
use "$outDir/lcadataMultinomTopics", clear

keep if idum==0 //unfair
keep if relinc==3

order relinc iperson item t_6_item   qual1 iperson
gsort- t_6_item


//money matters, anthony decides, unfair, man higher earner, low stakes
use "$outDir/lcadataMultinomTopics", clear

keep if aperson==2 //anthony
keep if adum==1 //fair
keep if relinc==1

order relinc aperson activity t_6_act   qual2 iperson
gsort- t_6_act





















































