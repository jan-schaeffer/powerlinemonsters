# Clear the R environment and set working directory
rm(list = ls())
setwd("/Users/jan/Dropbox/UP_EPQM/2222/MA/powerlinemonsters")
path <- getwd()
print(path)
options(warn=-1) # disable warnings
#options(warn=0) # enable warnings

# Install and load additional packages needed
#install.packages("stringr")
#install.packages("gridtext")
library(did)
library(rio)
library(ggplot2)
library(ggpubr)
library(hash)
library(gridtext)
library(grid)
.libPaths()

# Setup TeX Code to integrate figures
fig_tex <- r"(\renewcommand{\thefigure}{%s} \begin{figure} \centering \includegraphics[width=0.9\textwidth]{Figures/%s} \captionlistentry{%s} \label{fig:%s} \end{figure} \clearpage)"
figs_tex <- list()

# Define combine and save funcntion for figures
combine_and_save <- function(figures, result, party, title, caption, n, file_name, subpath, figs_tex) {
  # Combine Figures for all treatments
  arranged_fig <- ggarrange(plotlist=figures, nrow = 4, ncol = 1, common.legend = TRUE)
  # get legend
  result_fig <- ggdid(result) +
              theme(
              legend.background = element_rect(fill='transparent', color='transparent'),
              legend.box.background = element_rect(fill='transparent', color='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
  leg <- get_legend(result_fig)
  # add legend
  arranged_fig_leg <- ggarrange(arranged_fig, leg, nrow = 2, ncol = 1, heights = c(10, 1))
  # Annotate final figure
  final_fig <- annotate_figure(arranged_fig_leg,
                  top = text_grob(title, face = "bold", size = 14),
                  bottom = textbox_grob(caption, width = unit(20, "cm"), height = unit(3, "cm"), gp = gpar(fontsize = 7))
                  )
  # Add Figure number
  number <- sprintf("R%s", n)
  final_fig <- annotate_figure(final_fig, top = text_grob(sprintf("Fig.: %s", number), face = "bold", size = 10))
  # Save
  filename <- gsub('ü', 'ue', filename) #replace ü in Grüne
  ggsave(sprintf('%s.pdf', filename), plot = final_fig, path = sprintf('%s/figures/R/', path), units = 'cm', width = 21, height = 27, dpi="print")
  # Add Reference to figs_tex list
  title <- gsub("'s", '', title)
  figs_tex[[length(figs_tex)+1]] <- sprintf(fig_tex, number, filename, title, number)
  names(figs_tex)[length(figs_tex)] <- n
  return(figs_tex)
}

# Setup iterators
parties <- c('Union', 'SPD', 'FDP', 'Linke', 'Grüne', 'Andere')
treatments_dict <- hash('treatment_0'='Direct Line', 'treatment_15'='Within 0-15 km', 'treatment_30'='Within 15-30 km', 'treatment_50'='Within 30-50 km')
treatments <- keys(treatments_dict)
results  <- list()


# BTW no controls
# read in the data set
btw <- import(sprintf("%s/data/btw_treat.csv", path))
# set up Fig Number
n <- 1
for(party in parties){
  figures  <- list()
  for(treatment in treatments){
    result <- att_gt(yname = party,
                  gname = treatment,
                  idname = 'AGS',
                  panel = TRUE,
                  tname = 'year',
                  xformla = ~ 1,
                  data = btw,
                  est_method = 'reg',
                  anticipation = 0,
                  control_group = 'nevertreated',
                  clustervars = c('AGS'),
                  bstrap = TRUE,
                  cband = TRUE,
                  allow_unbalanced_panel = TRUE,
                  
    )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
      ylim = c(floor(min(result$att - result$se * 2.345 - 1)), ceiling(max(result$att + result$se * 2.345 + 1))),
      ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
      labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
      att$overall.att, att$overall.se, result$Wpval, format(format(result$n, big.mark=","), big.mark=","))) +
      theme(plot.caption = element_text(hjust=0.5, size=8),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
  }
  # Set-up and call combine and save
  title <- sprintf("Federal elections: Effects on the %s's vote share - unconditional model", party)
  caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the unconditional parallel trends assumption. 
  All models are estimated using a regression estimator that includes a constant. Never treated municipalities are used as control group.
  Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
  Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
  Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
  allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
  the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
  Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
  The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
  which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
  The overall ATT is given as the average of all group ATTs.
  Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
  filename <- sprintf('R%s_%s', n, party)
  subpath <- 'BTW/1_no_control' 
  figs_tex <- combine_and_save(figures, result, party, title, caption, n, file_name, subpath, figs_tex)
  n <- n + 1
  print(party)
}

# BTW controls
btw_c <- import(sprintf("%s/data/btw_control.csv", path))
n <- 7
for(party in parties){
  figures  <- list()
  for(treatment in treatments){
    result <- att_gt(yname = party,
                  gname = treatment,
                  idname = 'AGS',
                  panel = TRUE,
                  tname = 'year',
                  xformla = ~ east + south + pop_density + unemployed + female + avg_age,
                  data = btw_c,
                  est_method = 'reg',
                  anticipation = 0,
                  control_group = 'nevertreated',
                  clustervars = c('AGS'),
                  bstrap = TRUE,
                  cband = TRUE,
                  allow_unbalanced_panel = TRUE,
                  
    )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
  }
  # Set-up and call combine and save
  title <- sprintf("Federal elections: Effects on the %s's vote share - conditional model", party)
  caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the conditional parallel trends assumption. 
  All models are estimated using a regression estimator that includes the population density, share of unemployed, share of females, average age, as well as dummies for east and south Germany as control variables. 
  Never treated municipalities are used as control group.
  Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
  Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
  Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
  allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
  the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
  Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
  The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
  which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
  The overall ATT is given as the average of all group ATTs.
  Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
  filename <- sprintf('R%s_%s', n, party)
  subpath <- 'BTW/2_control' 
  figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
  n <- n + 1
  print(party)
}

# LTW no controls
ltw <- import(sprintf("%s/data/ltw_treat.csv", path))
states_dict <- hash('NI'='Lower Saxony', 'NW'='North-Rhine Westphalia', 'HE'='Hesse', 'BY'='Bavaria')
states <- keys(states_dict)
results  <- list()
n <- 13
for (state in states){
  for(party in parties){
    figures  <- list()
    reg_data <- subset(ltw, Land == state)
    for(treatment in treatments){
      result <- att_gt(yname = party,
                    gname = treatment,
                    idname = 'AGS',
                    panel = TRUE,
                    tname = 'year',
                    xformla = ~ 1,
                    data = reg_data,
                    est_method = 'reg',
                    anticipation = 0,
                    control_group = 'notyettreated',
                    clustervars = c('AGS'),
                    bstrap = TRUE,
                    cband = TRUE,
                    allow_unbalanced_panel = TRUE,
                    
      )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
    }
    # Set-up and call combine and save
    title <- sprintf("State election %s: Effect on the %s's vote share - unconditional model", state, party)
    caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the unconditional parallel trends assumption. 
    Not-yet treated municipalities are used as control group.
    Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
    Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
    Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
    allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
    the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
    Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
    The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
    which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
    The overall ATT is given as the average of all group ATTs.
    Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
    filename <- sprintf('R%s_%s_%s', n, state, party)
    subpath <- 'LTW/1_no_control' 
    figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
    n <- n + 2
    print(sprintf('%s: %s', state, party))
  }
}

# LTW controls
ltw_c <- import(sprintf("%s/data/ltw_control.csv", path))
states_dict <- hash('NI'='Lower Saxony', 'NW'='North-Rhine Westphalia', 'HE'='Hesse', 'BY'='Bavaria')
states <- keys(states_dict)
results  <- list()
n <- 14
for (state in states){
  for(party in parties){
    figures  <- list()
    reg_data <- subset(ltw_c, Land == state)
    for(treatment in treatments){
      result <- att_gt(yname = party,
                    gname = treatment,
                    idname = 'AGS',
                    panel = TRUE,
                    tname = 'year',
                    xformla = ~ pop_density + unemployed + avg_age + female,
                    data = reg_data,
                    est_method = 'reg',
                    anticipation = 0,
                    control_group = 'notyettreated',
                    clustervars = c('AGS'),
                    bstrap = TRUE,
                    cband = TRUE,
                    allow_unbalanced_panel = TRUE,
                    
      )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
    }
    # Set-up and call combine and save
    title <- sprintf("State election %s: Effect on the %s's vote share - conditional model", state, party)
    caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the conditional parallel trends assumption. 
    All models are estimated using a regression estimator that includes the population density, share of unemployed, share of females, and average age as  control variables. 
    Not-yet treated municipalities are used as control group.
    Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
    Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
    Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
    allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
    the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
    Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
    The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
    which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
    The overall ATT is given as the average of all group ATTs.
    Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
    filename <- sprintf('R%s_%s_%s', n, state, party)
    subpath <- 'LTW/2_control' 
    figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
    n <- n + 2
    print(sprintf('%s: %s', state, party))
  }
}

# BTW - no postal votes
btw_np <- import(sprintf("%s/data/btw_control_nopostal.csv", path))
results  <- list()
n <- 61
for(party in parties){
  figures  <- list()
  for(treatment in treatments){
    result <- att_gt(yname = party,
                  gname = treatment,
                  idname = 'AGS',
                  panel = TRUE,
                  tname = 'year',
                  xformla = ~ east + south + pop_density + unemployed + female + avg_age,
                  data = btw_np,
                  est_method = 'reg',
                  anticipation = 0,
                  control_group = 'nevertreated',
                  clustervars = c('AGS'),
                  bstrap = TRUE,
                  cband = TRUE,
                  allow_unbalanced_panel = TRUE,
                  
    )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
  }
  # Set-up and call combine and save
  title <- sprintf("Federal elections: Effects on the %s's vote share - no postal votes", party)
  caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the conditional parallel trends assumption. 
  All models are estimated using a regression estimator that includes the population density, share of unemployed, share of females, average age, as well as dummies for east and south Germany as control variables. 
  Never treated municipalities are used as control group.
  Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
  Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
  Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
  allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
  the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
  Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
  The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
  which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
  The overall ATT is given as the average of all group ATTs.
  Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
  filename <- sprintf('R%s_%s', n, party)
  subpath <- 'BTW/3_no_postal' 
  figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
  n <- n + 1
  print(party)
}

# BTW - not yet treated controls
results  <- list()
n <- 67
for(party in parties){
  figures  <- list()
  for(treatment in treatments){
    result <- att_gt(yname = party,
                  gname = treatment,
                  idname = 'AGS',
                  panel = TRUE,
                  tname = 'year',
                  xformla = ~ east + south + pop_density + unemployed + female + avg_age,
                  data = btw_c,
                  est_method = 'reg',
                  anticipation = 0,
                  control_group = 'notyettreated',
                  clustervars = c('AGS'),
                  bstrap = TRUE,
                  cband = TRUE,
                  allow_unbalanced_panel = TRUE,
                  
    )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
  }
  # Set-up and call combine and save
  title <- sprintf("Federal elections: Effects on the %s's vote share - not-yet treated control group", party)
  caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the conditional parallel trends assumption. 
  All models are estimated using a regression estimator that includes the population density, share of unemployed, share of females, average age, as well as dummies for east and south Germany as control variables. 
  Not-yet treated municipalities are used as control group.
  Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
  Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
  Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
  allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
  the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
  Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
  The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
  which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
  The overall ATT is given as the average of all group ATTs.
  Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
  filename <- sprintf('R%s_%s', n, party)
  subpath <- 'BTW/4_not_yet' 
  figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
  n <- n + 1
  print(party)
}

# BTW - one anticipation period
n <- 73
for(party in parties){
  figures  <- list()
  for(treatment in treatments){
    result <- att_gt(yname = party,
                  gname = treatment,
                  idname = 'AGS',
                  panel = TRUE,
                  tname = 'year',
                  xformla = ~ east + south + pop_density + unemployed + female + avg_age,
                  data = btw_c,
                  est_method = 'reg',
                  anticipation = 1,
                  control_group = 'nevertreated',
                  clustervars = c('AGS'),
                  bstrap = TRUE,
                  cband = TRUE,
                  allow_unbalanced_panel = TRUE,
                  
    )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
    labs(caption = cap) + 
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
  }
  # Set-up and call combine and save
  title <- sprintf("Federal elections: Effects on the %s's vote share - with one anticipation period", party)
  caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the conditional parallel trends assumption. 
  All models are estimated using a regression estimator that includes the population density, share of unemployed, share of females, average age, as well as dummies for east and south Germany as control variables. 
  Never treated municipalities are used as control group.
  Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
  Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
  Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
  allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
  the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
  Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
  The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
  which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
  The overall ATT is given as the average of all group ATTs.
  Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
  filename <- sprintf('R%s_%s', n, party)
  subpath <- 'BTW/5_anticipation' 
  figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
  n <- n + 1
  print(party)
}

# BTW - by state
states_dict <- hash('BB'='Brandenburg', 'BW'='Baden-Württemberg', 'BY'='Bavaria', 'HE'='Hesse', 'MV'='Mecklenburg-Vorpommern', 'NI'='Lower Saxony', 
'NW'='North-Rhine Westphalia', 'RP'='Rhineland-Palatinate', 'SH'='Schleswig-Holstein', 'ST'='Saxony-Anhalt', 'TH'='Thuringia') 
# all states - those never/always treated: BE, HH, HB, SL, SN, HE
states <- keys(states_dict)
results <- list()
n <- 79
for (state in states){
  for(party in parties){
    print(sprintf('%s: %s', state, party))
    reg_data <- subset(btw_c, Land == state)
    figures  <- list()
    for(treatment in treatments){
      result <- att_gt(yname = party,
                    gname = treatment,
                    idname = 'AGS',
                    panel = TRUE,
                    tname = 'year',
                    xformla = ~ pop_density + unemployed + female + avg_age,
                    data = reg_data,
                    est_method = 'reg',
                    anticipation = 0,
                    control_group = 'notyettreated',
                    clustervars = c('AGS'),
                    bstrap = TRUE,
                    cband = TRUE,
                    allow_unbalanced_panel = TRUE,
                    
      )
    results[[length(results)+1]] <- result
    # calculate ATT
    att <- aggte(result, type = "group", bstrap = TRUE, clustervars = c('AGS'))
    # plot results
    result_fig <- ggdid(result, 
              ylim = c(floor(min(result$att - result$se * 2.345 - 0.3)), ceiling(max(result$att + result$se * 2.345 + 0.3))),
              ncol = 3, ax_text_size = 8, grtitle='', title_size = 10, legend=F) +
              labs(caption = sprintf('Overall ATT (SE): %.3f (%.3f) \n Wald-p: %.3f; Municipalities: %s \n' ,
              att$overall.att, att$overall.se, result$Wpval, format(result$n, big.mark=","))) +
              theme(plot.caption = element_text(hjust=0.5, size=8),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Get caption for ATT plot
    cap = ''
      for(i in 1:length(att$DIDparams$glist)) {
        cap <- sprintf('%s ATT (SE): %.3f (%.3f)\n%s', att$DIDparams$glist[[i]], att$att.egt[[i]], att$se.egt[[i]], cap)
      }
    # Plot ATT
    att_fig <- ggdid(att, legend=F, x_lab='', ax_text_size = 8, ylab='', title='Group ATT', title_size = 10) + 
              labs(caption = cap) +
              theme(plot.caption = element_text(hjust=0.5, size=8), 
              axis.title.x = element_blank(),
              axis.text.y = element_text(angle=90, size=6, hjust=0.5),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              plot.margin=unit(c(0,0,0,0),"cm"))
    # Combine and annotate Group and ATT plot
    # Get number of results plots
    nr <- length(unique(result_fig$data$grtitle))
    combined_fig <- ggarrange(result_fig, att_fig, widths = c(nr, 1)) + 
                    theme(plot.margin=unit(c(0,0,0,0),"cm"))
    combined_fig_anno <- annotate_figure(combined_fig,
                  top = text_grob(sprintf('Treatment: %s', treatments_dict[[treatment]])))
    figures[[length(figures)+1]] <- combined_fig_anno
    }
    # Set-up and call combine and save
    title <- sprintf("Federal elections in %s: Effects on the %s's vote share", state, party)
    caption <- '**Note:** The effect of municipalities being affected by power line projects in 2005, 2010 and 2014 estimated under the conditional parallel trends assumption. 
    All models are estimated using a regression estimator that includes the population density, share of unemployed, share of females, and average age as control variables. 
    Not-yet treated municipalities are used as control group.
    Red lines give point estimates and simultaneous 95% confidence bands for pre-treatment periods allowing for clustering at the municipality level. 
    Under the null hypothesis of the parallel trends assumption holding in all periods, these should be equal to 0. 
    Blue lines provide point estimates and simultaneous 95% confidence bands for the treatment effect of municipalities being affected by a power line project 
    allowing for clustering at the municipality level. The first row includes municipalities directly affected by a power line project, 
    the second row municipalities within 0-15 km, the third row municipalities within 15-30 km, and the last row municipalities within 30-50 km distance of a power line project.
    Groups are not nested, i.e., do not contain municipalities already assigned to other groups. 
    The group-time average treatment effects (ATT) are shown in the left columns. The group-average treatment effects, 
    which are given as the average effect of the treatment for a given group across all post-treatment periods, are shown in the right column. 
    The overall ATT is given as the average of all group ATTs.
    Wald-p is the test statistic for the Wald pre-test of the parallel trends assumption.'
    filename <- sprintf('R%s_%s_%s', n, party, state)
    subpath <- 'BTW/6_by_state' 
    figs_tex <- combine_and_save(figures, result, party, title, caption, n, filename, subpath, figs_tex)
    n <- n + 1
    print(sprintf('%s: %s', state, party))
  }
}

# Order and print figs_tex
order <- c(1:144)
for (i in length(order)){
    order[i] <- as.character(order[i])
}
figs_tex <- figs_tex[order]
for (fig in figs_tex){
    print(fig)
}