# Clear the R environment and set working directory
rm(list = ls())
setwd("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters")
path <- "/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters"
# Install and load additional packages needed
#install.packages("patchwork")
library(did)
library(rio)
library(ggplot2)
library(patchwork)
library(ggpubr)

# read in the data set
btw <-import("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters/data/btw_control.csv")
summary(btw)

erst <- subset(btw, first_vote == 1)

result <- att_gt(yname = 'Union',
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
results[[length(results)+1]] <- result
# calculate ATTs
att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
att <- c(att$att.egt)
names(att) <- c('2005', '2010', '2014')
att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
se <- c(att$se.egt)
names(se) <- c('2005', '2010', '2014')

sprintf('Group 2005 ATT(SE): %s (%s) \n Group 2010 ATT(SE): %s (%s) \n Group 2014 ATT(SE): %s (%s)', 
        att[['2005']], se[['2005']], att[['2010']], se[['2010']], att[['2014']], se[['2014']])
# plot results
result_fig <- ggdid(result, 
                    ylim = c(floor(min(result$att - result$se * 2.345 - 1)), ceiling(max(result$att + result$se * 2.345 + 1))),
                    ncol = 3, ax_text_size = 8, grtitle='') +
  labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-P: %.3f \n # Obs.: %s' ,
                         att$overall.att, att$overall.se, result$Wpval, result$n)) + 
theme(plot.caption = element_text(hjust=0.5, size=10),
      plot.margin=unit(c(0,0,0,0),"cm"))
result_fig


leg <- get_legend(result_fig)

# Convert to a ggplot and print
as_ggplot(leg)
# Plot ATT
att_fig <- ggdid(att, lab_size = 10, legend=F, x_lab='', ax_text_size = 5, ylab='', title="ATT") + 
  labs(caption = sprintf('Group 2005 ATT(SE): %.3f (%.3f) \n Group 2010 ATT(SE): %.3f (%.3f) \n Group 2014 ATT(SE): %.3f (%.3f)', 
                         att[['2005']], se[['2005']], att[["2010"]], se[['2010']], att[['2014']], se[['2014']])) + 
  theme(plot.caption = element_text(hjust=0.5, size=10), axis.title.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm"))

combined_fig <- ggarrange(result_fig, att_fig, widths = c(3, 1)) + #, heights=c(1, 0.7)) +  
  theme(plot.margin=unit(c(0,0,0,0),"cm"))
combined_fig
combined_fig_anno <- annotate_figure(combined_fig,
                                top = text_grob(sprintf('%s', 'treatment_0')))
figures[[length(figures)+1]] <- combined_fig_anno

arranged_fig <- ggarrange(plotlist=figures, nrow = 1, ncol = 1, common.legend = TRUE, legend = 'bottom')
arranged_fig
final_fig <- annotate_figure(arranged_fig,
                             top = text_grob(sprintf("Effect on %s's vote share", 'Union'), face = "bold", size = 14),
                             bottom = text_grob('Control Group: Never Treated. Estimation Method: Regression \n SE clustered on the municipality level.', size = 10))
final_fig

cap =sprintf('Group 2005 ATT(SE): %.3f (%.3f) \n Group 2010 ATT(SE): %.3f (%.3f) \n Group 2014 ATT(SE): %.3f (%.3f)', 
             as.numeric(att$att.egt[[1]]), as.numeric(att$se.egt[[1]]), as.numeric(att$att.egt[[2]]), as.numeric(att$att.se[[2]]), as.numeric(att$att.egt[[3]]), as.numeric(att$se.egt[[3]]))
cap

att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
sprintf('%.3f _ %.3f', att$att.egt[[1]], att$se.egt[[1]])


sprintf('%s', att[['2005']])
sprintf('%s %s', att[['2005']], att[["2010"]])


att <- c(att$att.egt)
names(att) <- c('2005', '2010', '2014')
att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
se <- c(att$se.egt)
names(se) <- c('2005', '2010', '2014')

do.call(sprintf, c(list("%s %s %s"), att$att.egt))

att$att.egt

stats = as.data.frame(att$att.egt)
names(stats) <- c('a', 'b', 'c')
stats$a
att = as.numeric(stats[1, 1])
att

View(stats)

cap =sprintf('Group 2005 ATT(SE): %.3f (%.3f)', as.numeric(stats[1, 1]), as.numeric(stats[1, 2]))
cap
                                                          
att
stats

stat <-  as.numeric(att$att.egt[1])
stat
as_numer

as.numeric(att$att.egt[1])

.rs.restartR() 
library(ggplot2)
library(patchwork)
library(ggpubr)

library(ggplot2)

# Very basic chart
ggplot( mtcars , aes(x=mpg, y=wt)) + 
  geom_point() + theme(axis.title.x = element_text(angle = 90, color="red", size=15, face=-100)) 

