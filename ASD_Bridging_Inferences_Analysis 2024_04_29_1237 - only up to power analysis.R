# Data analysis for ASD and Bridging Inferences project

# Preliminary ----
# Loading libraries
library(janitor)
library(tictoc)
library(Rmisc)          # Includes summarySE function
#library(cowplot)       # Includes plot_grid function, but it's causing problems with dplyr
library(tidyverse)
library(rstatix)        # Includes anova_test function
library(pwr)
library(effsize)

# Setting working directory
setwd("C:/Users/Arthur/OneDrive - Carleton University/Documents/R/ASD_Bridging_Inferences_Analysis/Data")


# Loading and cleaning data for inference comprehension ----
df_inf_comp <- read.csv(file = "InfComp_Data_LongFormat.csv", encoding="UTF-8")#,
                        #colClasses=c("X.U.FEFF.ID"="character"))
#df_inf_comp <- df_inf_comp %>% rename("ID" = X.U.FEFF.ID)
df_inf_comp <- df_inf_comp %>% rename("TOM" = ToM)
df_inf_comp <- df_inf_comp %>% rename("ASD_group" = Group)    # 1 = ASD, 0 = neurotypical
df_inf_comp <- clean_names(df_inf_comp)
df_inf_comp <- df_inf_comp %>% group_by(id, kbit, asq) %>%
  mutate(participant_id = cur_group_id()) %>% ungroup()       # Creating unique participant ID because some IDs are repeated
df_inf_comp <- df_inf_comp %>% select(participant_id, everything())
df_inf_comp <- df_inf_comp[order(df_inf_comp$participant_id),]

# Extracting summary statistics for power analysis ----

# Adding columns by condition and accuracy
df_inf_comp <- df_inf_comp %>% mutate(inf0_tom0_corr = 0, inf0_tom0_incorr = 0,
                                      inf0_tom1_corr = 0, inf0_tom1_incorr = 0,
                                      inf1_tom0_corr = 0, inf1_tom0_incorr = 0,
                                      inf1_tom1_corr = 0, inf1_tom1_incorr = 0)

tic()

# Populating the condition by accuracy columns
for (i in 1:nrow(df_inf_comp)) {
  
  if ((df_inf_comp[i, "inference"] == 0) & (df_inf_comp[i, "tom"] == 0) & (df_inf_comp[i, "accuracy"] == 1))
    df_inf_comp[i, "inf0_tom0_corr"] <- 1
  else
    df_inf_comp[i, "inf0_tom0_incorr"] <- 1
  
  if ((df_inf_comp[i, "inference"] == 0) & (df_inf_comp[i, "tom"] == 1) & (df_inf_comp[i, "accuracy"] == 1))
    df_inf_comp[i, "inf0_tom1_corr"] <- 1
  else
    df_inf_comp[i, "inf0_tom1_incorr"] <- 1
  
  if ((df_inf_comp[i, "inference"] == 1) & (df_inf_comp[i, "tom"] == 0) & (df_inf_comp[i, "accuracy"] == 1))
    df_inf_comp[i, "inf1_tom0_corr"] <- 1
  else
    df_inf_comp[i, "inf1_tom0_incorr"] <- 1
  
  if ((df_inf_comp[i, "inference"] == 1) & (df_inf_comp[i, "tom"] == 1) & (df_inf_comp[i, "accuracy"] == 1))
    df_inf_comp[i, "inf1_tom1_corr"] <- 1
  else
    df_inf_comp[i, "inf1_tom1_incorr"] <- 1
  
}

toc()


# Creating data frame with totals (number correct) by participant by condition
df_inf_comp_totals <- df_inf_comp[,c("participant_id","asd_group")] %>%
                      group_by(participant_id) %>% filter(row_number() == 1)
df_inf_comp_totals <- df_inf_comp_totals[order(df_inf_comp_totals$participant_id),]
df_inf_comp_totals <- df_inf_comp_totals %>%
                      add_column("inf0_all_corr_total"  = NA, "inf1_all_corr_total" = NA,
                                 "inf0_tom0_corr_total" = NA, "inf0_tom1_corr_total" = NA,
                                 "inf1_tom0_corr_total" = NA, "inf1_tom1_corr_total" = NA)
for (i in 1:nrow(df_inf_comp_totals)) {
  cur_participant <- as.numeric(df_inf_comp_totals[i,"participant_id"])
  df_inf_comp_totals[cur_participant,]$inf0_all_corr_total  <- 
      sum(df_inf_comp[df_inf_comp$participant_id == cur_participant,c("inf0_tom0_corr","inf0_tom1_corr")])
  df_inf_comp_totals[cur_participant,]$inf1_all_corr_total  <- 
      sum(df_inf_comp[df_inf_comp$participant_id == cur_participant,c("inf1_tom0_corr","inf1_tom1_corr")])
  df_inf_comp_totals[cur_participant,]$inf0_tom0_corr_total <-
      sum(df_inf_comp[df_inf_comp$participant_id == cur_participant,"inf0_tom0_corr"])
  df_inf_comp_totals[cur_participant,]$inf0_tom1_corr_total <-
      sum(df_inf_comp[df_inf_comp$participant_id == cur_participant,"inf0_tom1_corr"])
  df_inf_comp_totals[cur_participant,]$inf1_tom0_corr_total <-
      sum(df_inf_comp[df_inf_comp$participant_id == cur_participant,"inf1_tom0_corr"])
  df_inf_comp_totals[cur_participant,]$inf1_tom1_corr_total <-
      sum(df_inf_comp[df_inf_comp$participant_id == cur_participant,"inf1_tom1_corr"])
}

# Create data frame with summary statistics for power analysis
df_inf_comp_summaries <- 
    data.frame(asd_group =
                  c(0,1,NA),
               sample_size =
                  c(nrow(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]),
                    nrow(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]),
                    nrow(df_inf_comp_totals)),
               inf0_all_corr_mean = 
                  c(mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf0_all_corr_total),
                    mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf0_all_corr_total),
                    mean(df_inf_comp_totals$inf0_all_corr_total)),
               inf0_all_corr_sd = 
                  c(sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf0_all_corr_total),
                    sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf0_all_corr_total),
                    sd(df_inf_comp_totals$inf0_all_corr_total)),
               inf1_all_corr_mean = 
                  c(mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf1_all_corr_total),
                    mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf1_all_corr_total),
                    mean(df_inf_comp_totals$inf1_all_corr_total)),
               inf1_all_corr_sd = 
                  c(sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf1_all_corr_total),
                    sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf1_all_corr_total),
                    sd(df_inf_comp_totals$inf1_all_corr_total)),
               inf0_tom0_corr_mean = 
                  c(mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf0_tom0_corr_total),
                    mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf0_tom0_corr_total),
                    mean(df_inf_comp_totals$inf0_tom0_corr_total)),
               inf0_tom0_corr_sd = 
                  c(sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf0_tom0_corr_total),
                    sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf0_tom0_corr_total),
                    sd(df_inf_comp_totals$inf0_tom0_corr_total)),
               inf0_tom1_corr_mean = 
                  c(mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf0_tom1_corr_total),
                    mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf0_tom1_corr_total),
                    mean(df_inf_comp_totals$inf0_tom1_corr_total)),
               inf0_tom1_corr_sd = 
                  c(sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf0_tom1_corr_total),
                    sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf0_tom1_corr_total),
                    sd(df_inf_comp_totals$inf0_tom1_corr_total)),
               inf1_tom0_corr_mean = 
                  c(mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf1_tom0_corr_total),
                    mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf1_tom0_corr_total),
                    mean(df_inf_comp_totals$inf1_tom0_corr_total)),
               inf1_tom0_corr_sd = 
                  c(sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf1_tom0_corr_total),
                    sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf1_tom0_corr_total),
                    sd(df_inf_comp_totals$inf1_tom0_corr_total)),
               inf1_tom1_corr_mean = 
                  c(mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf1_tom1_corr_total),
                    mean(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf1_tom1_corr_total),
                    mean(df_inf_comp_totals$inf1_tom1_corr_total)),
               inf1_tom1_corr_sd = 
                  c(sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 0,]$inf1_tom1_corr_total),
                    sd(df_inf_comp_totals[df_inf_comp_totals$asd_group == 1,]$inf1_tom1_corr_total),
                    sd(df_inf_comp_totals$inf1_tom1_corr_total))
    )

stop()


# Functions for t-test power simulation ----

# Note: Code for power analysis are adapted from 7 Apr 2023 version of CGSC 6801 project

# Create a data frame with a row for each iteration of the t-test simulation
create_t_test_params <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,    # Number of repetitions for each number of participants
    np = np,         # Range of numbers of participants we are simulating
    m_inf0  = df_inf_comp_summaries[3,]$inf0_all_corr_mean,
    sd_inf0 = df_inf_comp_summaries[3,]$inf0_all_corr_sd,
    m_inf1  = df_inf_comp_summaries[3,]$inf1_all_corr_mean,
    sd_inf1 = df_inf_comp_summaries[3,]$inf1_all_corr_sd
  )
}

# Create a function that generates data for inference vs. non-inference questions and compares them with a t-test
# The data is normally distributed and uses the above parameters
sim_t_test <- function(np, m_inf0, sd_inf0, m_inf1, sd_inf1) {
  data1 <- rnorm(np, m_inf0, sd_inf0)   # Generate L1 data
  data2 <- rnorm(np, m_inf1, sd_inf1)   # Generate L2 data
  
  # Get results for a t-test comparing L1 and L2 data
  test_results <- t.test(data1, data2, paired=TRUE)
  
  # Output a data frame with two cells: number of participants and the p-value from the t-test
  out <- as.data.frame(rbind(np, test_results$p.value))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_t_tests <- function(params) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_t_test,
                     np = params$np,
                     m_inf0 = params$m_inf0, sd_inf0 = params$sd_inf0,
                     m_inf1 = params$m_inf1, sd_inf1 = params$sd_inf1)
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_participants","p_value")
  # Return the data frame
  out <- p_values_df
}


# Functions for ANOVA simulation (NT vs. ASD), equal samples ----

# Make a data frame with a row for each iteration of the simulation
create_anova_params_es <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,        # Number of repetitions for each number of participants
    np = np,             # Range of numbers of participants we are simulating (both groups combined)
    m_nt_inf0   = df_inf_comp_summaries[1,]$inf0_all_corr_mean,
    sd_nt_inf0  = df_inf_comp_summaries[1,]$inf0_all_corr_sd,
    m_nt_inf1   = df_inf_comp_summaries[1,]$inf1_all_corr_mean,
    sd_nt_inf1  = df_inf_comp_summaries[1,]$inf1_all_corr_sd,
    m_asd_inf0  = df_inf_comp_summaries[2,]$inf0_all_corr_mean,
    sd_asd_inf0 = df_inf_comp_summaries[2,]$inf0_all_corr_sd,
    m_asd_inf1  = df_inf_comp_summaries[2,]$inf1_all_corr_mean,
    sd_asd_inf1 = df_inf_comp_summaries[2,]$inf1_all_corr_sd
  )
}


# Create a function that creates data for four conditions and compares them w/ a mixed-model ANOVA
# The data is normally distributed and uses the above parameters
sim_anova_es <- function(np, m_nt_inf0, sd_nt_inf0, m_nt_inf1, sd_nt_inf1,
                         m_asd_inf0, sd_asd_inf0, m_asd_inf1, sd_asd_inf1, which_effect) {
  
  data1 <- rnorm(np, m_nt_inf0,  sd_nt_inf0)
  data2 <- rnorm(np, m_nt_inf1,  sd_nt_inf1)
  data3 <- rnorm(np, m_asd_inf0, sd_asd_inf0)
  data4 <- rnorm(np, m_asd_inf1, sd_asd_inf1)

  data1_df <- data.frame(dv=data1, my_group="nt",  has_inf="inf0", pid=1:np)
  data2_df <- data.frame(dv=data2, my_group="nt",  has_inf="inf1", pid=1:np)
  data3_df <- data.frame(dv=data3, my_group="asd", has_inf="inf0", pid=np + 1:np)
  data4_df <- data.frame(dv=data4, my_group="asd", has_inf="inf1", pid=np + 1:np)

  combined_all_data <- rbind(data1_df, data2_df, data3_df, data4_df)

  # Get results for an ANOVA (effect of group and inference)
  anova_results <- anova_test(data=combined_all_data, dv=dv, wid=pid,
                                between=my_group, within=has_inf)
    # Note that "1" indicates the first p-value, the effect of group
    # Note that "2" indicates the second p-value, the effect of inference
    # Note that "3" indicates the third p-value, the interaction effect (i.e., group*inference)
  out <- as.data.frame(rbind(np*2, anova_results$p[which_effect]))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_anovas_es <- function(params, which_effect) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_anova_es,
                      np = params$np,
                      m_nt_inf0  = params$m_nt_inf0,  sd_nt_inf0  = params$sd_nt_inf0,
                      m_nt_inf1  = params$m_nt_inf1,  sd_nt_inf1  = params$sd_nt_inf1,
                      m_asd_inf0 = params$m_asd_inf0, sd_asd_inf0 = params$sd_asd_inf0,
                      m_asd_inf1 = params$m_asd_inf1, sd_asd_inf1 = params$sd_asd_inf1,
                      which_effect = which_effect)
  
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_participants","p_value")
  # Return the data frame
  out <- p_values_df
}


# Functions for ANOVA simulation (1 B-S var. and 2 W-S vars.), equal samples ----

# Make a data frame with a row for each iteration of the simulation
create_anova_params_es_3var <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,        # Number of repetitions for each number of participants
    np = np,             # Range of numbers of participants we are simulating (both groups combined)
    m_nt_inf0_tom0   = df_inf_comp_summaries[1,]$inf0_tom0_corr_mean,
    sd_nt_inf0_tom0  = df_inf_comp_summaries[1,]$inf0_tom0_corr_sd,
    m_nt_inf0_tom1   = df_inf_comp_summaries[1,]$inf0_tom1_corr_mean,
    sd_nt_inf0_tom1  = df_inf_comp_summaries[1,]$inf0_tom1_corr_sd,
    m_nt_inf1_tom0   = df_inf_comp_summaries[1,]$inf1_tom0_corr_mean,
    sd_nt_inf1_tom0  = df_inf_comp_summaries[1,]$inf1_tom0_corr_sd,
    m_nt_inf1_tom1   = df_inf_comp_summaries[1,]$inf1_tom1_corr_mean,
    sd_nt_inf1_tom1  = df_inf_comp_summaries[1,]$inf1_tom1_corr_sd,
    m_asd_inf0_tom0  = df_inf_comp_summaries[2,]$inf0_tom0_corr_mean,
    sd_asd_inf0_tom0 = df_inf_comp_summaries[2,]$inf0_tom0_corr_sd,
    m_asd_inf0_tom1  = df_inf_comp_summaries[2,]$inf0_tom1_corr_mean,
    sd_asd_inf0_tom1 = df_inf_comp_summaries[2,]$inf0_tom1_corr_sd,
    m_asd_inf1_tom0  = df_inf_comp_summaries[2,]$inf1_tom0_corr_mean,
    sd_asd_inf1_tom0 = df_inf_comp_summaries[2,]$inf1_tom0_corr_sd,
    m_asd_inf1_tom1  = df_inf_comp_summaries[2,]$inf1_tom1_corr_mean,
    sd_asd_inf1_tom1 = df_inf_comp_summaries[2,]$inf1_tom1_corr_sd
  )
}


# Create a function that creates data for four conditions and compares them w/ a mixed-model ANOVA
# The data is normally distributed and uses the above parameters
sim_anova_es_3var <- function(np,
                              m_nt_inf0_tom0,  sd_nt_inf0_tom0,  m_nt_inf0_tom1,  sd_nt_inf0_tom1, 
                              m_nt_inf1_tom0,  sd_nt_inf1_tom0,  m_nt_inf1_tom1,  sd_nt_inf1_tom1,
                              m_asd_inf0_tom0, sd_asd_inf0_tom0, m_asd_inf0_tom1, sd_asd_inf0_tom1, 
                              m_asd_inf1_tom0, sd_asd_inf1_tom0, m_asd_inf1_tom1, sd_asd_inf1_tom1,
                              which_effect) {
  
  data1 <- rnorm(np, m_nt_inf0_tom0,  sd_nt_inf0_tom0)
  data2 <- rnorm(np, m_nt_inf0_tom1,  sd_nt_inf0_tom1)
  data3 <- rnorm(np, m_nt_inf1_tom0,  sd_nt_inf1_tom0)
  data4 <- rnorm(np, m_nt_inf1_tom1,  sd_nt_inf1_tom1)
  data5 <- rnorm(np, m_asd_inf0_tom0, sd_asd_inf0_tom0)
  data6 <- rnorm(np, m_asd_inf0_tom1, sd_asd_inf0_tom1)
  data7 <- rnorm(np, m_asd_inf1_tom0, sd_asd_inf1_tom0)
  data8 <- rnorm(np, m_asd_inf1_tom1, sd_asd_inf1_tom1)

  data1_df <- data.frame(dv=data1, my_group="nt",  has_inf="inf0", has_tom="tom0", pid=1:np)
  data2_df <- data.frame(dv=data2, my_group="nt",  has_inf="inf0", has_tom="tom1", pid=1:np)
  data3_df <- data.frame(dv=data3, my_group="nt",  has_inf="inf1", has_tom="tom0", pid=1:np)
  data4_df <- data.frame(dv=data4, my_group="nt",  has_inf="inf1", has_tom="tom1", pid=1:np)
  data5_df <- data.frame(dv=data5, my_group="asd", has_inf="inf0", has_tom="tom0", pid=np + 1:np)
  data6_df <- data.frame(dv=data6, my_group="asd", has_inf="inf0", has_tom="tom1", pid=np + 1:np)
  data7_df <- data.frame(dv=data7, my_group="asd", has_inf="inf1", has_tom="tom0", pid=np + 1:np)
  data8_df <- data.frame(dv=data8, my_group="asd", has_inf="inf1", has_tom="tom1", pid=np + 1:np)

  combined_all_data <- rbind(data1_df, data2_df, data3_df, data4_df,
                             data5_df, data6_df, data7_df, data8_df)

  # Get results for an ANOVA (effect of group and inference)
  anova_results <- anova_test(data=combined_all_data, dv=dv, wid=pid,
                              between=my_group, within=c(has_inf, has_tom))
    # Note that "1" indicates the first p-value, the effect of group
    # Note that "2" indicates the second p-value, the effect of inference
    # "3" indicates ToM
    # "4" indicates group*inf
    # "5" indicates group*ToM
    # "6" indicates inf*ToM
    # "7" indicates group*ToM*inf
  out <- as.data.frame(rbind(np*2, anova_results$p[which_effect]))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_anovas_es_3var <- function(params, which_effect) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_anova_es_3var,
                      np = params$np,
                      m_nt_inf0_tom0  = params$m_nt_inf0_tom0,  sd_nt_inf0_tom0  = params$sd_nt_inf0_tom0,
                      m_nt_inf0_tom1  = params$m_nt_inf0_tom1,  sd_nt_inf0_tom1  = params$sd_nt_inf0_tom1,
                      m_nt_inf1_tom0  = params$m_nt_inf1_tom0,  sd_nt_inf1_tom0  = params$sd_nt_inf1_tom0,
                      m_nt_inf1_tom1  = params$m_nt_inf1_tom1,  sd_nt_inf1_tom1  = params$sd_nt_inf1_tom1,
                      m_asd_inf0_tom0 = params$m_asd_inf0_tom0, sd_asd_inf0_tom0 = params$sd_asd_inf0_tom0,
                      m_asd_inf0_tom1 = params$m_asd_inf0_tom1, sd_asd_inf0_tom1 = params$sd_asd_inf0_tom1,
                      m_asd_inf1_tom0 = params$m_asd_inf1_tom0, sd_asd_inf1_tom0 = params$sd_asd_inf1_tom0,
                      m_asd_inf1_tom1 = params$m_asd_inf1_tom1, sd_asd_inf1_tom1 = params$sd_asd_inf1_tom1,
                      which_effect = which_effect)
  
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_participants","p_value")
  # Return the data frame
  out <- p_values_df
}



# Functions for analyzing and visualizing simulation results (t-test and equal samples ANOVA) ----

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

plot_power <- function(p_values_df, my_title, np) {
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
  
  # # Create a plot showing number of participants needed as a function of desired statistical power
  # ggplot(data=parts_for_power, aes(x = stat_power, y = num_of_part)) +
  #   scale_y_continuous(limits = c(0, tail(np, 1))) +
  #   geom_point(color='darkred', na.rm = TRUE) +
  #   geom_vline(xintercept = 80) + geom_vline(xintercept = 100)
  
  ggplot(data=parts_for_power, aes(x = stat_power, y = num_of_part)) + 
    geom_vline(xintercept = 80, color = "red") + geom_point(color="blue", na.rm = TRUE) + geom_line() + 
    expand_limits(x = 0, y = 0) + 
    xlab("Desired Statistical Power (%)") + ylab("Participants Needed") + ggtitle(my_title) +
    theme_bw() + theme(plot.title=element_text(hjust=0.5))  
}


# Run the simulations ----

tic("T-test simulation")

# Simulate the t-tests and create a data frame of the p-values and number of participants
np1 <- seq(50, 1250, 50)    # Sequence of numbers (min, max, increment)
params1 <- create_t_test_params(np1, 10000)
p_values1 <- sim_all_t_tests(params1)

toc()

# Create plots based on the simulation results
# plot1a        <- plot_means(p_values1)
percentiles1  <- prepare_50_80_95(p_values1)
plot1b        <- plot_50_80_95(percentiles1, "Inference vs. No Inference", np1)
plot1c        <- plot_power(p_values1, "Inference vs. No Inference", np1)

# Print plots
# plot1a
plot1b
plot1c



tic("equal samples ANOVA simulation")

# Simulate the ANOVAs (equal samples) and create a data frame of the p-values and number of participants
# Remember that np1 is the number of participants PER GROUP
np2 <- seq(100, 1500, 100)
params2 <- create_anova_params_es(np2, 300)
p_values2 <- sim_all_anovas_es(params2, 3)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
# plot2a       <- plot_means(p_values2)
percentiles2 <- prepare_50_80_95(p_values2)
plot2b       <- plot_50_80_95(percentiles2, "Group-Inference Interaction", np2)
plot2c       <- plot_power(p_values2, "Group-Inference Interaction", np2*2)

# Print plots
# plot2a
plot2b
plot2c




tic("equal samples ANOVA simulation 3 variables")

# Simulate the ANOVAs (equal samples) and create a data frame of the p-values and number of participants
# Remember that np1 is the number of participants PER GROUP
np3 <- seq(100, 1000, 100)
params3 <- create_anova_params_es_3var(np3, 100)
p_values3 <- sim_all_anovas_es_3var(params3, 7)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
# plot3a       <- plot_means(p_values3)
percentiles3 <- prepare_50_80_95(p_values3)
plot3b       <- plot_50_80_95(percentiles3, "Three-Way Interaction", np3)
plot3c       <- plot_power(p_values3, "Three-Way Interaction", np3*2)

# Print plots
# plot3a
plot3b
plot3c

