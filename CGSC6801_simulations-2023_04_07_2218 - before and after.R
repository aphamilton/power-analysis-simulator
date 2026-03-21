#
# Libraries ----
#


library(easystats)
library(ggplot2)
library(tictoc)
library(Rmisc)
library(faux)
library(tidyverse)
library(readr)
library(interactions)
library(rstatix)
library(scales)
library(cowplot)


#
# Functions for t-test simulation (combined sample) ----
#
  
# Means and standard deviations are from Ożańska-Ponikwia (2019)
#   https://doi.org/10.1080/13670050.2016.1270893, Table 2
#   36 participants for long-stay and 36 participants for short-stay
#   All values are from a 10-point Likert scale
#   Long-stay:     mean (L1) = 9.44, SD (L1) = 1.13, mean (L2) = 8.02, SD (L2) = 2.46
#   Short-stay:    mean (L1) = 9.72, SD (L1) = 0.65, mean (L2) = 5.91, SD (L2) = 3.59
#   Merged sample: mean (L1) = 9.58, SD (L1) = 0.92, mean (L2) = 6.97, SD (L2) = 4.20
# The merged SDs were calculated with the formula here:
#   https://math.stackexchange.com/questions/2971315/how-do-i-combine-standard-deviations-of-two-groups
# The merged sample has 72 participants
# The simulations are performed using the means and SDs from the merged sample

# Create a data frame with a row for each iteration of the t-test simulation
create_t_test_params <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,    # Number of repetitions for each number of participants
    np = np,         # Range of numbers of participants we are simulating
    m_l1 = 9.58,     # Mean Likert-scale rating in L1
    sd_l1 = 0.92,    # SD of the Likert-scale rating in L1
    m_l2 = 6.97,     # Mean Likert-scale rating in L2
    sd_l2 = 4.20     # SD of the Likert-scale rating in L2
  )
}

# Create a function that generates data for L1 and L2 and compares them with a t-test
# The data is normally distributed and uses the above parameters
sim_t_test <- function(np, m_l1, sd_l1, m_l2, sd_l2) {
  data1 <- rnorm(np, m_l1, sd_l1)   # Generate L1 data
  data2 <- rnorm(np, m_l2, sd_l2)   # Generate L2 data
  
  # Get results for a t-test comparing L1 and L2 data
  test_results <- t.test(data1, data2)
  
  # Output a data frame with two cells: number of participants and the p-value from the t-test
  out <- as.data.frame(rbind(np, test_results$p.value))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_t_tests <- function(params) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_t_test,
                     np = params$np,
                     m_l1 = params$m_l1, sd_l1 = params$sd_l1,
                     m_l2 = params$m_l2, sd_l2 = params$sd_l2)
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_participants","p_value")
  # Return the data frame
  out <- p_values_df
}


#
# Functions for ANOVA simulation (anglophones vs francophones), equal samples ----
#

# The means and SDs here are adjusted from the ones used in the previous simulation (the merged ones)
# I adjusted the data based on our secondary hypothesis about the emotionality of French vs. English
#    I added 0.2 for the means for speaking French, and subtracted 0.2 for the means for speaking English
#    I did not change the standard deviations
# Means and SDs from above: mean (L1) = 9.58, SD (L1) = 0.92, mean (L2) = 6.97, SD (L2) = 4.20
# New means and SDs:
#    Francophones: mean (Fre) = 9.78, SD (Fre) = 0.92, mean (Eng) = 6.77, SD (Eng) = 4.20
#    Anglophones: mean (Eng) = 9.38, SD (Eng) = 0.92, mean (Fre) = 7.17, SD (Fre) = 4.20

# Make a data frame with a row for each iteration of the simulation
# This one is for a priori power analysis
create_anova_params_es_ap <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,      # Number of repetitions for each number of participants
    np = np,       # Range of numbers of participants we are simulating (anglophones and francophones combined)
    m_fra_fre = 9.78,    # Likert-scale ratings for francophones
    sd_fra_fre = 0.92,
    m_fra_eng = 6.77,
    sd_fra_eng = 4.20,
    m_ang_fre = 7.17,    # Likert-scale ratings for anglophones
    sd_ang_fre = 4.20,
    m_ang_eng = 9.38,
    sd_ang_eng = 0.92
  )
}

# Make a data frame with a row for each iteration of the simulation
# This one is for post hoc power analysis
create_anova_params_es_ph <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,      # Number of repetitions for each number of participants
    np = np,       # Range of numbers of participants we are simulating (anglophones and francophones combined)
    m_fra_fre = 6.55,    # Likert-scale ratings for francophones
    sd_fra_fre = 2.22,
    m_fra_eng = 6.64,
    sd_fra_eng = 1.98,
    m_ang_fre = 6.23,    # Likert-scale ratings for anglophones
    sd_ang_fre = 2.10,
    m_ang_eng = 6.38,
    sd_ang_eng = 2.13
  )
}

# Create a function that creates data for four conditions and compares them w/ a mixed-model ANOVA
# The data is normally distributed and uses the above parameters
sim_anova_es <- function(np, m_fra_fre, sd_fra_fre, m_fra_eng, sd_fra_eng,
                         m_ang_fre, sd_ang_fre, m_ang_eng, sd_ang_eng, which_effect) {
  #fra_indiv_diffs <- runif(np, min=-0.3, max=0.3)
  #ang_indiv_diffs <- runif(np, min=-0.3, max=0.3)
  data1 <- rnorm(np, m_fra_fre, sd_fra_fre) #+ fra_indiv_diffs   # Generate data for francophones in French
  data2 <- rnorm(np, m_fra_eng, sd_fra_eng) #+ fra_indiv_diffs   # Generate data for francophones in English
  data3 <- rnorm(np, m_ang_fre, sd_ang_fre) #+ ang_indiv_diffs   # Generate data for anglophones in French
  data4 <- rnorm(np, m_ang_eng, sd_ang_eng) #+ ang_indiv_diffs   # Generate data for anglophones in English

  data1_df <- data.frame(dv=data1, first_lang="fra", cur_lang="fre", pid=1:np)
  data2_df <- data.frame(dv=data2, first_lang="fra", cur_lang="eng", pid=1:np)
  data3_df <- data.frame(dv=data3, first_lang="ang", cur_lang="fre", pid=np + 1:np)
  data4_df <- data.frame(dv=data4, first_lang="ang", cur_lang="eng", pid=np + 1:np)

  combined_all_data <- rbind(data1_df, data2_df, data3_df, data4_df)

  # Get results for an ANOVA (effect of first language and current language)
  anova_results <- anova_test(data=combined_all_data, dv=dv, wid=pid,
                                between=first_lang, within=cur_lang)
    # Note that "1" indicates the first p-value, the effect of first language
    # Note that "2" indicates the second p-value, the effect of current language
    # Note that "3" indicates the third p-value, the interaction effect (i.e., L1 vs. L2)
  out <- as.data.frame(rbind(np*2, anova_results$p[which_effect]))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_anovas_es <- function(params, which_effect) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_anova_es,
                      np = params$np,
                      m_fra_fre = params$m_fra_fre, sd_fra_fre = params$sd_fra_fre,
                      m_fra_eng = params$m_fra_eng, sd_fra_eng = params$sd_fra_eng,
                      m_ang_fre = params$m_ang_fre, sd_ang_fre = params$sd_ang_fre,
                      m_ang_eng = params$m_ang_eng, sd_ang_eng = params$sd_ang_eng,
                      which_effect = which_effect)
  
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_participants","p_value")
  # Return the data frame
  out <- p_values_df
}


#
# Functions for ANOVA simulation (anglophones vs francophones), different samples ----
#

# The means and SDs are same as for the equal samples ANOVAs

# Make a data frame with a row for each iteration of the simulation
# This one is for an a priori power analysis
create_anova_params_ds_ap <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,      # Number of repetitions for each number of participants
    np_fra = np,        # Range of numbers of francophones we are simulating
    np_ang = np,        # Range of numbers of anglophones we are simulating
    m_fra_fre = 9.78,    # Likert-scale ratings for francophones
    sd_fra_fre = 0.92,
    m_fra_eng = 6.77,
    sd_fra_eng = 4.20,
    m_ang_fre = 7.17,    # Likert-scale ratings for anglophones
    sd_ang_fre = 4.20,
    m_ang_eng = 9.38,
    sd_ang_eng = 0.92
  )
}

# Make a data frame with a row for each iteration of the simulation
# This one is for a post-hoc power analysis
create_anova_params_ds_ph <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,      # Number of repetitions for each number of participants
    np_fra = np,        # Range of numbers of francophones we are simulating
    np_ang = np,        # Range of numbers of anglophones we are simulating
    m_fra_fre = 6.55,    # Likert-scale ratings for francophones
    sd_fra_fre = 2.22,
    m_fra_eng = 6.64,
    sd_fra_eng = 1.98,
    m_ang_fre = 6.23,    # Likert-scale ratings for anglophones
    sd_ang_fre = 2.10,
    m_ang_eng = 6.38,
    sd_ang_eng = 2.13
  )
}

# Create a function that creates data for four conditions and compares them w/ a mixed-model ANOVA
# The data is normally distributed and uses the above parameters
sim_anova_ds <- function(np_fra, np_ang, m_fra_fre, sd_fra_fre, m_fra_eng, sd_fra_eng,
                         m_ang_fre, sd_ang_fre, m_ang_eng, sd_ang_eng, which_effect) {
  #fra_indiv_diffs <- runif(np_fra, min=0, max=0)
  #ang_indiv_diffs <- runif(np_ang, min=0, max=0)
  data1 <- rnorm(np_fra, m_fra_fre, sd_fra_fre) #+ fra_indiv_diffs   # Generate L1 data for francophones
  data2 <- rnorm(np_fra, m_fra_eng, sd_fra_eng) #+ fra_indiv_diffs   # Generate L2 data for francophones
  data3 <- rnorm(np_ang, m_ang_fre, sd_ang_fre) #+ ang_indiv_diffs   # Generate L1 data for anglophones
  data4 <- rnorm(np_ang, m_ang_eng, sd_ang_eng) #+ ang_indiv_diffs   # Generate L2 data for anglophones
  
  data1_df <- data.frame(dv=data1, first_lang="fra", cur_lang="fre", pid=1:np_fra)
  data2_df <- data.frame(dv=data2, first_lang="fra", cur_lang="eng", pid=1:np_fra)
  data3_df <- data.frame(dv=data3, first_lang="ang", cur_lang="fre", pid=np_fra + 1:np_ang)
  data4_df <- data.frame(dv=data4, first_lang="ang", cur_lang="eng", pid=np_fra + 1:np_ang)
  
  combined_all_data <- rbind(data1_df, data2_df, data3_df, data4_df)
  
  # Get results for an ANOVA (effect of first language and current language)
  anova_results <- anova_test(data=combined_all_data, dv=dv, wid=pid,
                              between=first_lang, within=cur_lang)
  # Note that "1" indicates the first p-value, the effect of first language
  # Note that "2" indicates the second p-value, the effect of current language
  # Note that "3" indicates the third p-value, the interaction effect (i.e., L1 vs. L2)
  out <- as.data.frame(rbind(np_fra, np_ang, anova_results$p[which_effect]))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_anovas_ds <- function(params, which_effect) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_anova_ds,
                          np_fra = params$np_fra, np_ang = params$np_ang,
                          m_fra_fre = params$m_fra_fre, sd_fra_fre = params$sd_fra_fre,
                          m_fra_eng = params$m_fra_eng, sd_fra_eng = params$sd_fra_eng,
                          m_ang_fre = params$m_ang_fre, sd_ang_fre = params$sd_ang_fre,
                          m_ang_eng = params$m_ang_eng, sd_ang_eng = params$sd_ang_eng,
                          which_effect = which_effect)
  
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_francophones","Number_of_anglophones","p_value")
  # Add a column for total number of participants
  out <- p_values_df %>% mutate(Number_of_participants=Number_of_francophones+Number_of_anglophones)
}


#
# Shared functions for analyzing and visualizing simulation results ----
#

plot_means <- function(p_values_df) {
  # Make a data frame with the mean across simulations for each number of participants
  means <- summarySE(data=p_values_df, measurevar="p_value", groupvars="Number_of_participants")
  
  # Create a plot of the mean results
  ggplot(data=means, aes(x = Number_of_participants, y = p_value))+geom_point(color='darkblue')+
    geom_hline(yintercept = 0.05)
}

prepare_50_80_95 <- function(p_values_df) {
  # Determine the 50th, 80th, and 95th percentile 
  quantile50 <- p_values_df %>% 
    group_by(Number_of_participants) %>% 
    summarise(x = quantile(p_value, 0.5), q = 0.5)
  quantile80 <- p_values_df %>% 
    group_by(Number_of_participants) %>% 
    summarise(x = quantile(p_value, 0.8), q = 0.8)
  quantile95 <- p_values_df %>% 
    group_by(Number_of_participants) %>% 
    summarise(x = quantile(p_value, 0.95), q = 0.95)
  
  quantiles_df <- rbind(quantile50, quantile80, quantile95)
  quantiles_df <- quantiles_df %>% rename(Percentile = q)
  quantiles_df$Percentile[quantiles_df$Percentile==0.5]<-"50th"
  quantiles_df$Percentile[quantiles_df$Percentile==0.8]<-"80th"
  quantiles_df$Percentile[quantiles_df$Percentile==0.95]<-"95th"
  out <- quantiles_df
}

plot_50_80_95 <- function(percentiles_df, my_title, np) {
  # Create a plot of the 50th, 80th, and 95th percentiles of p-values at each number of participants
  ggplot(data=percentiles_df, aes(x = Number_of_participants, y = x, color = Percentile)) + 
    #geom_rect(xmin=0, xmax=max(np)*2, ymin=0, ymax=0.05, fill="gold") +
    geom_hline(yintercept = 0.05, color = "red") + geom_point() + geom_line() + 
    expand_limits(x = 0, y = 0) +
    scale_color_manual(values = c("50th"="black", "80th"="blue", "95th"="deepskyblue")) +
    xlab("Total Participants") + ylab("P-value") + ggtitle(my_title) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5))
}

plot_power <- function(p_values_df, np) {
  # Create data frame of percentiles at each number of participants
  percentiles <- data.frame(num_of_part = np)
  for (i in 1:99) {
    new_df <- p_values_df %>% 
      group_by(Number_of_participants) %>% 
      summarise(x = quantile(p_value, i*0.01), q = i*0.01)
    percentiles[i+1] <- new_df$x
    colnames(percentiles)[i+1] <- paste("P", i, sep = "")
  } 
  
  # Create another data frame showing the first value in each row that achieved statistical significance
  parts_for_power <- data.frame(stat_power = c(1:99), num_of_part = NA)
  for (percent in 1:99) {
    for (n_part in 1:nrow(percentiles)) {
      if (percentiles[n_part,percent+1] < 0.05) {
        parts_for_power[percent,"num_of_part"] <- percentiles[n_part,"num_of_part"]
        break
      }
    }
  } 
  
  # Create a plot showing number of participants needed as a function of desired statistical power
  ggplot(data=parts_for_power, aes(x = stat_power, y = num_of_part)) +
    scale_y_continuous(limits = c(0, tail(np, 1))) +
    geom_point(color='darkred', na.rm = TRUE) +
    geom_vline(xintercept = 80) + geom_vline(xintercept = 100)
}


#
# Functions for heatmap plots----
#

plot_means_heatmap <- function(p_values_df, np) {
  # Make a data frame with the mean across simulations for each number of participants
  means <- summarySE(data=p_values_df, measurevar="p_value",
                     groupvars=c("Number_of_francophones", "Number_of_anglophones"))
  
  # Create a heatmap of the mean results
  ggplot(data=means, aes(x = Number_of_francophones, y = Number_of_anglophones, fill = p_value)) +
    xlab("Participants with L1 = French") + ylab("Participants with L1 = English") + labs(fill="P-value\n(mean)") +
    ggtitle("Post-hoc power analysis (mean p-values)") +
    # geom_tile() + scale_fill_gradientn(colours=c("red","yellow","green", "cyan", "blue", "darkblue"),
    #                                    values=scales::rescale(c(0, 0.05, 0.1, 0.2, 0.5, 1)), limits=c(0,1),
    #                                    guide="colorbar") +
    geom_tile() +
    scale_fill_gradientn(colours=c("red", "yellow", "cyan", "blue", "black"),
                         values=scales::rescale(c(0, 0.049999, 0.05, 0.2, 1)),
                         limits=c(0,1), guide=guide_colorbar(ticks=FALSE)) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5), legend.key.height = unit(0.15, "snpc"))
}

prepare_quantile80 <- function(p_values_df) {
  # Determine the 80th percentile
  out <- p_values_df %>%
    group_by(Number_of_francophones, Number_of_anglophones) %>%
    summarise(p_value = quantile(p_value, probs = 0.8))
}

plot_80_heatmap <- function(percentile80_df, my_title) {
  # Create a heatmap of the 80th percentile results
  out <- ggplot(data=percentile80_df, aes(x = Number_of_francophones, y = Number_of_anglophones, fill = p_value)) +
    xlab("Participants with L1 = French") + ylab("Participants with L1 = English") +
    labs(fill="P-value") + ggtitle(my_title) +
    geom_tile() +
    scale_fill_gradientn(colours=c("red", "yellow", "cyan", "blue", "black"),
                         values=scales::rescale(c(0, 0.049999, 0.05, 0.2, 1)),
                         limits=c(0,1), guide=guide_colorbar(ticks=FALSE)) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5), legend.key.height = unit(0.05, "npc"))
}


#
# Run the simulations ----
#

# tic("T-test simulation")
# 
# # Simulate the t-tests and create a data frame of the p-values and number of participants
# np1 <- 2:50
# params1 <- create_t_test_params(np1, 10000)
# p_values1 <- sim_all_t_tests(params1)
# 
# # Create plots based on the simulation results
# plot_means(p_values1)
# plot_50_80_95(p_values1)
# plot_power(p_values1, np1)
# 
# toc()


tic("equal samples a priori simulation")

# Simulate the ANOVAs (equal samples) and create a data frame of the p-values and number of participants
# Remember that np1 is the number of participants PER GROUP
np1 <- seq(2, 20, 1)  # Sequence of numbers (min, max, increment)
params1 <- create_anova_params_es_ap(np1, 2000)
p_values1 <- sim_all_anovas_es(params1, 3)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
#plot_means(p_values1)
percentiles1 <- prepare_50_80_95(p_values1)
my_plot1 <- plot_50_80_95(percentiles1, "Balanced Groups", np1)
#plot_power(p_values1, np1*2)


tic("different samples a priori simulation")

# Simulate the ANOVAs (different samples) and create a data frame of the p-values and number of participants
# Remember that np2 is the number of participants PER GROUP
np2 <- seq(2, 20, 2)  # Sequence of numbers (min, max, increment)
params2 <- create_anova_params_ds_ap(np2, 250)
p_values2 <- sim_all_anovas_ds(params2, 3)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
#plot_means_heatmap(p_values2, np2)
percentile_80_2 <- prepare_quantile80(p_values2)
my_plot2 <- plot_80_heatmap(percentile_80_2, "Unbalanced Groups")

# #Create a canvas with the post-hoc power analysis plots
# ggdraw() +
#   draw_plot(my_plot1, 0, 0, .5, 1) +
#   draw_plot(my_plot2, .5, 0, .5, 1) +
#   draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 1), size = 15)

plot_grid(my_plot1, my_plot2, ncol = 2, nrow = 1, labels = c('A', 'B'))


tic("equal samples post hoc simulation")

# Simulate the ANOVAs (equal samples) and create a data frame of the p-values and number of participants
# Remember that np3 is the number of participants PER GROUP
np3 <- seq(25, 600, 25)  # Sequence of numbers (min, max, increment)
params3 <- create_anova_params_es_ph(np3, 2000)
p_values3 <- sim_all_anovas_es(params3, 1)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
#plot_means(p_values3)
percentiles3 <- prepare_50_80_95(p_values3)
my_plot3 <- plot_50_80_95(percentiles3, "Balanced Groups", np3)
#plot_power(p_values3, np3*2)


tic("different samples post-hoc simulation")

# Simulate the ANOVAs (different samples) and create a data frame of the p-values and number of participants
# Remember that np4 is the number of participants PER GROUP
np4 <- seq(50, 600, 50)  # Sequence of numbers (min, max, increment)
params4 <- create_anova_params_ds_ph(np4, 250)
p_values4 <- sim_all_anovas_ds(params4, 1)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
#plot_means_heatmap(p_values4, np4)
percentile_80_4 <- prepare_quantile80(p_values4)
my_plot4 <- plot_80_heatmap(percentile_80_4, "Unbalanced Groups")

plot_grid(my_plot3, my_plot4, ncol = 2, nrow = 1, labels = c('A', 'B'))

# #Create a canvas with the post-hoc power analysis plots
# ggdraw() +
#   draw_plot(my_plot3, 0, 0, .5, 1) +
#   draw_plot(my_plot4, .5, 0, .5, 1) +
#   draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 1), size = 15)


