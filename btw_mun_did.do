clear all
capture log close
set more off

global AVZ "/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters"
global data $AVZ/data
global tables $AVZ/tables/stata

set maxvar 32000, perm


import delimited $data/btw_treat.csv, clear
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
	esttab using "2013_`treatment'.rtf", 
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

//Placebo Simple DiD
preserve 
keep if year < 2017

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009, cluster(state_id)
	}	
	#delimit;
	esttab using "2009_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant") 
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore


// DiD with municipality and year FE
xtset ags year

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui xtreg `party' i.`treatment'##i.post_2013 i.year, fe cluster(state_id)
	}
	#delimit;
	esttab using "2013_fe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD with fixed effects for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_* 1.`treatment')
	coeflabels(1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant") 
	indicate(Year FE = *year)
	addnote("Note: These are linear models with municipality and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}


// Placebo DiD with municipality and year FE
preserve 
keep if year < 2017

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui xtreg `party' i.`treatment'##i.post_2009 i.year, fe cluster(state_id)
	}	
	#delimit;
	esttab using "2009_fe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD with fixed effects for `treatment' between 2009 and 2013"}) 
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



// DiD with kreis and year FE
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2013 i.kreis i.year, cluster(state_id)
	}	

	#delimit;
	esttab using "2013_kreisfe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant")
	indicate("Year FE = *year" "County FE = *kreis" )
	addnote("Note: These are linear models  with county and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}

// Placebo DiD with state and year FE
preserve 
keep if year < 2017
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009 i.kreis i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2009_kreisfe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant")
	indicate("Year FE = *year" "County FE = *kreis")
	addnote("Note: These are linear models  with county and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore



// DiD with wahlkreis and year FE
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2013 i.wahlkreis i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2013_wahlkreisfe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant")
	indicate("Year FE = *year" "Constituency FE = *wahlkreis" )
	addnote("Note: These are linear models  with constituency and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}

// Placebo DiD with wahlkreis and year FE
preserve 
keep if year < 2017
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009 i.wahlkreis i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2009_wahlkreisfe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant")
	indicate("Year FE = *year" "Constituency FE = *kreis")
	addnote("Note: These are linear models  with constituency and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore

// DiD with state and year FE
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2013 i.state_id i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2013_statefe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant")
	indicate("Year FE = *year" "State FE = *state_id" )
	addnote("Note: These are linear models  with state and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}

// Placebo DiD with state and year FE
preserve 
keep if year < 2017
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009 i.state_id i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2009_statefe_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant")
	indicate("Year FE = *year" "State FE = *state_id")
	addnote("Note: These are linear models  with state and year fixed effects. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore





// DiD with controls and state year FE
import delimited $data/btw_control.csv, clear
*import delimited "https://media.githubusercontent.com/media/jan-schaeffer/powerlinemonsters/dev/data/btw_control.csv", clear
gen lavg_income = log(avg_income)
keep if first_vote == 1
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2013 pop_density female foreign unemployed lavg_income avg_age catholic i.state_id i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2013_ctrl_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2013 and 2017"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2013 "post_2013" 1.`treatment'#1.post_2013 "`treatment'*post_2013" _cons "Constant" ///
	pop_density "Population Density" female "% Female" foreign "% Foreign" unemployed "% Unemployed" lavg_income "Log(Avg. Income/Taxpayer)" ///
	avg_age "Avg. Age" catholic "% Catholic") 
	indicate("Year FE = *year" "State FE = *state_id")
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}


// Placebo DiD with controls
preserve 
keep if year < 2017
local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
local parties union spd fdp linke grüne andere
foreach treatment of local treatments {
	foreach party of local parties{
		eststo `treatment'_`party': qui reg `party' i.`treatment'##i.post_2009 pop_density female foreign unemployed lavg_income avg_age catholic i.state_id i.year, cluster(state_id)
	}	
	#delimit;
	esttab using "2009_ctrl_`treatment'.rtf", 
	star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
	modelwidth(7) varwidth(11) b(3) se(3)
	title ({\b "Table X."} {\i "DiD for `treatment' between 2009 and 2013"}) 
	mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
	drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
	coeflabels(1.`treatment' "`treatment'" 1.post_2009 "post_2009" 1.`treatment'#1.post_2009 "`treatment'*post_2009" _cons "Constant" ///
	pop_density "Population Density" female "% Female" foreign "% Foreign" unemployed "% Unemployed" lavg_income "Log(Avg. Income/Taxpayer)" ///
	avg_age "Avg. Age" catholic "% Catholic") 
	indicate("Year FE = *year" "State FE = *state_id")
	addnote("Note: These are linear models. Standard errors in parentheses are clustered at the state level.")
	stats(N_clust N, fmt(0 0) labels("Municipalities" "Observations")) replace;

	#delimit cr
	eststo clear

}
restore




// two sample ttest
import delimited $data/btw_control.csv, clear
keep if first_vote == 1

local treatments treated_0 treated_10 treated_20 treated_30 treated_50 treated_100
foreach var of local treatments {
	di "`var'"
	qui estpost ttest pop_density female foreign unemployed avg_income avg_age catholic, by(`var')
	esttab, wide nonumber mtitle("diff.")
}

/* 
Treatments groups are;
- more densely populated
- have a lower share of females
- a lower share of foreigners
- a higher share of unemployed 
- have a lower share of catholics 
- are older (30, 50) resp. younger (0, 100)
- have a lower avg income (0, 100)
*/


