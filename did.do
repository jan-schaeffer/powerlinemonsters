clear all
capture log close
set more off

global data "/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters/data"





import delimited $data/btw_treat.csv, clear

local parties union spd fdp linke grüne andere

foreach var of local parties {
	qui reg `var' i.(treated_0 treated_10 treated_20 treated_30 treated_50 treated_100)##i.post_2017, cluster(ags)
	estimates store `var'
}

#delimit;
esttab using "simpledid.rtf", 
star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
modelwidth(7) varwidth(11) b(3) se(3)
mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
stats(N_clust N, fmt(3 0) labels("Municipalities" "Observations")) replace;

#delimit cr


local parties union spd fdp linke grüne andere

foreach var of local parties {
	qui reg `var' i.(treated_0 treated_10 treated_20 treated_30 treated_50 treated_100)##i.(post_2013 post_2009 post_2005), cluster(ags)
	estimates store `var'
}

#delimit;
esttab using "placebodid.rtf", 
star(* 0.10 ** 0.05 *** 0.01) noconstant onecell
modelwidth(7) varwidth(11) b(3) se(3)
mti ("Union" "SPD" "FDP" "Linke" "Grüne" "Andere") 
drop(0.treated_* 0.post_* 1.treated_*#0.post_*)
stats(N_clust N, fmt(3 0) labels("Municipalities" "Observations")) replace;

#delimit cr
