# Clear the R environment and set working directory
rm(list = ls())
setwd("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters")

# Install and load additional packages needed
#install.packages("ivreg")
library(did)
library(rio)
library(ggplot2)

# read in the data set
btw <-import("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters/data/btw_control.csv")
summary(btw)

erst <- subset(btw, first_vote == 1)
by <- subset(erst, Land == "BY")
zweit <- btw[btw$second_vote == 1,]
summary(erst)

parties <- c('Union', 'SPD', 'FDP', 'Linke', 'Grüne', 'Andere')
treatments <-  c('treatment_30', 'treatment_60', 'treatment_100')

for(party in parties){
  for(treatment in treatments){
    out <- att_gt(yname = party,
                  gname = treatment,
                  idname = "AGS",
                  panel = TRUE,
                  tname = "year",
                  xformla = ~ C(state_id), # ~ pop_density + female + foreign + unemployed + avg_income + avg_age + catholic,
                  data = erst,
                  est_method = "reg",
                  anticipation = 0,
                  control_group = "nevertreated",
                  clustervars = c("AGS", "state_id"),
                  #bstrap = TRUE,
                  #cband = TRUE,
                  allow_unbalanced_panel = TRUE,
    )
    ggdid(out, ylim = c(floor(min(out$att - out$se * 2.345 - 1)), ceiling(max(out$att + out$se * 2.345 + 1))))
    ggsave(sprintf("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters/figures/R/%s_%s.png", party, treatment))
    es <- aggte(out, type = "dynamic")
    ggdid(es, ylim = c(floor(min(es$att.egt - es$se.egt * 2.345 - 1)), ceiling(max(es$att.egt + es$se.egt * 2.345 + 1))))
    ggsave(sprintf("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters/figures/R/ES_%s_%s.png", party, treatment))
  }
}


out <- att_gt(yname = 'Union',
              gname = 'treatment_15',
              idname = "AGS",
              panel = TRUE,
              tname = "year",
              xformla = ~ 1, #pop_density + female + avg_age,
              data = erst,
              est_method = "dr",
              anticipation = 1,
              control_group = "notyettreated",
              clustervars = c("AGS"),
              bstrap = TRUE,
              cband = TRUE,
              allow_unbalanced_panel = FALSE,
              #print_details = TRUE
)
summary(out)
es <- aggte(out, type = "group", bstrap = TRUE, clustervars = c("AGS"))
summary(es)
ggdid(es, ylim = c(floor(min(es$att.egt - es$se.egt * 2.345 - 1)), ceiling(max(es$att.egt + es$se.egt * 2.345 + 1))))

library(did)
trace(gplot, edit=TRUE)


group_effects <- aggte(out, type = "group")
summary(group_effects)


library(did)
data(mpdta)
View(mpdta)
out <- att_gt(yname = "lemp",
              gname = "first.treat",
              idname = "countyreal",
              tname = "year",
              xformla = ~1,
              data = mpdta,
              est_method = "reg"
)

summary(out)
