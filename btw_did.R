# Clear the R environment and set working directory
rm(list = ls())
setwd("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters")

# Install and load additional packages needed
#install.packages("ivreg")
library(did)
library(rio)

# read in the data set
data <-import("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters/data/btw_treat.csv")
summary(data)


out <- att_gt(yname = "Union",
              gname = "treatment_30",
              idname = "AGS",
              panel = TRUE,
              tname = "year",
              xformla = ~1,
              data = data,
              est_method = "ipw",
              anticipation = 0,
              control_group = "notyettreated",
              clustervars = c("AGS", "state_id"),
              bstrap = TRUE,
              cband = TRUE,
              allow_unbalanced_panel = TRUE,
)

summary(out)

ggdid(out, ylim = c(-1.5, 3.5))

es <- aggte(out, type = "dynamic")
summary(es)
ggdid(es)


group_effects <- aggte(out, type = "group")
summary(group_effects)