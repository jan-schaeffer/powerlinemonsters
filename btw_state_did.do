clear all
capture log close
set more off

global AVZ "/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters"
global data $AVZ/data
global tables $AVZ/tables/stata

set maxvar 32000, perm


import delimited $data/btw_treat_state.csv, clear
*import delimited "https://media.githubusercontent.com/media/jan-schaeffer/powerlinemonsters/dev/data/btw_treat.csv", clear
keep if first_vote == 1

cd $tables

//Simple DiD
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2013, cluster(state_id)
	}	
	#delimit;
	esttab using "state_2013_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant") 
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(3 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}


//Placebo DiD
preserve 
keep if year < 2017

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009, cluster(state_id)
	}	
	#delimit;
	esttab using "state_2009_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "Placebo DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant") 
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore


// Placebo DiD with FE
xtset state_id year
preserve 
keep if year < 2017

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui xtreg `party' i.`treatment'##i.post_2009 i.year, fe cluster(state_id)
	}	
	#delimit;
	esttab using "state_2009_fe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "Placebo DiD with fixed effects for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_* 1.`treatment')
	coeflabels(1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant") 
	indicate(Year FE = *year)
	addnote("Note: These are linear models with municipality and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore


// DiD with FE
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui xtreg `party' i.`treatment'##i.post_2013 i.year, fe cluster(state_id)
	}	
	#delimit;
	esttab using "state_2013_fe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "Placebo DiD with fixed effects for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_* 1.`treatment')
	coeflabels(1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant") 
	indicate(Year FE = *year)
	addnote("Note: These are linear models with municipality and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}



import delimited $data/btw_treat_district.csv, clear
*import delimited "https://media.githubusercontent.com/media/jan-schaeffer/powerlinemonsters/dev/data/btw_treat.csv", clear
keep if first_vote == 1

cd $tables

//Simple DiD
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2013, cluster(wahlkreis)
	}	
	#delimit;
	esttab using "wk_2013_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant") 
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(3 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}


//Placebo DiD
preserve 
keep if year < 2017

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009, cluster(wahlkreis)
	}	
	#delimit;
	esttab using "wk_2009_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "Placebo DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant") 
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore

// Placebo DiD with FE
xtset wahlkreis year
preserve 
keep if year < 2017

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui xtreg `party' i.`treatment'##i.post_2009 i.year, fe cluster(wahlkreis)
	}	
	#delimit;
	esttab using "wk_2009_fe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "Placebo DiD with fixed effects for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_* 1.`treatment')
	coeflabels(1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant") 
	indicate(Year FE = *year)
	addnote("Note: These are linear models with municipality and year fixed effects. Standard errors in parentheses are clustered at the election district level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore


// DiD with FE
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui xtreg `party' i.`treatment'##i.post_2013 i.year, fe cluster(wahlkreis)
	}	
	#delimit;
	esttab using "wk_2013_fe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "Placebo DiD with fixed effects for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_* 1.`treatment')
	coeflabels(1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant") 
	indicate(Year FE = *year)
	addnote("Note: These are linear models with municipality and year fixed effects. Standard errors in parentheses are clustered at the election district level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
