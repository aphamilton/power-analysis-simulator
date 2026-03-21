# Data analysis for ASD and Bridging Inferences project

# Preliminary ----
# Load libraries
library(janitor)
library(tictoc)
library(Rmisc)          # Includes summarySE function
#library(cowplot)       # Includes plot_grid function, but it's causing problems with dplyr
library(tidyverse)
library(rstatix)        # Includes anova_test function
library(pwr)
library(effsize)

# Set working directory
setwd("C:/Users/Arthur/OneDrive - Carleton University/Documents/R/PhD_Thesis/SZ_Lexis_Power_Analysis/Data")


# Load data ----

# Load df of relevant cols
df_nt <- read.csv(file = "AH-Thesis-Power-Sim.csv", encoding="UTF-8")
# Remove unnecessary first col
df_nt <- df_nt[,-1]
# Add col for MSS-B total
df_nt$mss_b_total <- NA
for (my_row in 1:nrow(df_nt))
  df_nt[my_row,"mss_b_total"] <- sum(df_nt[my_row, 1:3], na.rm=TRUE)

# Create hypothetical SZ df
df_sz <- df_nt[df_nt$mss_b_total >= 20,]

# Add group col to dfs
df_nt$group <- 0
df_sz$group <- 1

# Create df with NTs and SZ
df_nt_sz <- rbind(df_nt, df_sz)

# Correlation tests (original data) ----

cor.test(df_nt$lt_l1_l2_difference, df_nt$mss_b_total)
cor.test(df_nt$lt_l1_l2_difference, df_nt$prof_l1_l2_diff)


# Multiple regression (original data) ----

lm_prof <-  lm(data = df_nt,
               formula = lt_l1_l2_difference ~ prof_l1_l2_diff)
summary(lm_prof)

lm_1sz <-  lm(data = df_nt,
               formula = lt_l1_l2_difference ~ mss_b_total)
summary(lm_1sz)

lm_3sz <-  lm(data = df_nt,
               formula = lt_l1_l2_difference ~ mss_b_pos + mss_b_neg + mss_b_dis)
summary(lm_3sz)

lm_4pred <- lm(data = df_nt,
               formula = lt_l1_l2_difference ~ mss_b_pos + mss_b_neg + mss_b_dis + prof_l1_l2_diff)
summary(lm_4pred)


# Power analysis (based on original data) ----

# For correlation test
pwr.r.test(r=0.15, sig.level=0.05, power=0.8)
# n = 345

# For simple regression (just MSS-B total score)
pwr.f2.test(u=1, f2=0.022/(1-0.022), sig.level=0.05, power=0.8)
# n = 351

# For multiple regression w/ three predictors
pwr.f2.test(u=3, f2=0.06/(1-0.06), sig.level=0.05, power=0.8)
# n = 175

# For multiple regression w/ four predictors
pwr.f2.test(u=4, f2=0.18/(1-0.18), sig.level=0.05, power=0.8)
# n = 59


# Comparing NT and SZ groups ----

# T-test
t.test(df_nt$lt_l1_l2_difference, df_sz$lt_l1_l2_difference, paired=FALSE)
cohens_d(data=df_nt_sz, formula=lt_l1_l2_difference ~ group, paired=FALSE)
# d = 0.228
pwr.t.test(d=0.228, sig.level=0.05, power=0.8)

#pwr.t2n.test(n2=100, d=0.228, sig.level=0.05, power=0.8)

# GLM
lm_group <- lm(data = df_nt_sz,
               formula = lt_l1_l2_difference ~ group)
summary(lm_group)

lm_group_lt <- lm(data = df_nt_sz,
                  formula = lt_l1_l2_difference ~ group + prof_l1_l2_diff + group:prof_l1_l2_diff)
summary(lm_group_lt)

lm_group_dis <- lm(data = df_nt_sz,
                   formula = lt_l1_l2_difference ~ group + mss_b_dis + group:mss_b_dis)
summary(lm_group_dis)
# multiple R-squared = 0.01

# Power analysis for the above with pwr
# However, this is for the significance of the overall model
pwr.f2.test(u=3, f2=0.01/(1-0.01), sig.level=0.05, power=0.8)
# Yield 1083 participants


# Create data frame with parameters for power analysis ----
df_parameters <- 
    data.frame(group = c(0,1),
               mss_b_total_mean = 
                  c(mean(df_nt$mss_b_total),
                    mean(df_sz$mss_b_total)),
               mss_b_total_sd = 
                  c(sd(df_nt$mss_b_total),
                    sd(df_sz$mss_b_total)),
               mss_b_pos_mean = 
                  c(mean(df_nt$mss_b_pos),
                    mean(df_sz$mss_b_pos)),
               mss_b_pos_sd = 
                  c(sd(df_nt$mss_b_pos),
                    sd(df_sz$mss_b_pos)),
               mss_b_neg_mean = 
                  c(mean(df_nt$mss_b_neg),
                    mean(df_sz$mss_b_neg)),
               mss_b_neg_sd = 
                  c(sd(df_nt$mss_b_neg),
                    sd(df_sz$mss_b_neg)),
               mss_b_dis_mean = 
                  c(mean(df_nt$mss_b_dis),
                    mean(df_sz$mss_b_dis)),
               mss_b_dis_sd = 
                  c(sd(df_nt$mss_b_dis),
                    sd(df_sz$mss_b_dis)),
               prof_diff_mean = 
                  c(mean(df_nt$prof_l1_l2_diff),
                    mean(df_sz$prof_l1_l2_diff)),
               prof_diff_sd = 
                  c(sd(df_nt$prof_l1_l2_diff),
                    sd(df_sz$prof_l1_l2_diff)),
               lt_diff_mean = 
                  c(mean(df_nt$lt_l1_l2_difference),
                    mean(df_sz$lt_l1_l2_difference)),
               lt_diff_sd = 
                  c(sd(df_nt$lt_l1_l2_difference),
                    sd(df_sz$lt_l1_l2_difference))
              )


# Functions for t-test power simulation ----

# Note: Code for power analysis are adapted from 7 Apr 2023 version of CGSC 6801 project
# And then adapted from version I presented in Olessia's lab in Nov 2023

# Create a data frame with a row for each iteration of the t-test simulation
create_t_test_params <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,     # Number of repetitions for each number of participants
    np  = np,         # Range of numbers of participants we are simulating
    m_nt  = df_parameters[1,"lt_diff_mean"],
    sd_nt = df_parameters[1,"lt_diff_sd"],
    m_sz  = df_parameters[2,"lt_diff_mean"],
    sd_sz = df_parameters[2,"lt_diff_sd"]
  )
}

# Create a function that generates data for inference vs. non-inference questions and compares them with a t-test
# The data is normally distributed and uses the above parameters
sim_t_test <- function(np, m_nt, sd_nt, m_sz, sd_sz) {
  data1 <- rnorm(np, m_nt, sd_nt)   # Generate L1 data
  data2 <- rnorm(np, m_sz, sd_sz)   # Generate L2 data
  
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
                     m_nt = params$m_nt, sd_nt = params$sd_nt,
                     m_sz = params$m_sz, sd_sz = params$sd_sz)
  # Save the list as a data frame
  p_values_df <- do.call(rbind.data.frame, p_values_list)
  # Name the columns of the data frame
  colnames(p_values_df) <- c("Number_of_participants","p_value")
  # Return the data frame
  out <- p_values_df
}


# Functions for ANOVA simulation (NT vs. ASD), equal samples ----

# Make a data frame with a row for each iteration of the simulation
create_mr_params_es <- function(np, reps) {
  out <- tidyr::crossing(
    rep = 1:reps,        # Number of repetitions for each number of participants
    np = np,             # Range of numbers of participants we are simulating (both groups combined)
    m_nt_lt   = df_parameters[1,"lt_diff_mean"],
    sd_nt_lt  = df_parameters[1,"lt_diff_sd"],
    m_nt_dis  = df_parameters[1,"mss_b_dis_mean"],
    sd_nt_dis = df_parameters[1,"mss_b_dis_sd"],
    m_sz_lt   = df_parameters[2,"lt_diff_mean"],
    sd_sz_lt  = df_parameters[2,"lt_diff_sd"],
    m_sz_dis  = df_parameters[2,"mss_b_dis_mean"],
    sd_sz_dis = df_parameters[2,"mss_b_dis_sd"]
  )
}


# Create a function that creates data for a regression with two predictors
# The data is normally distributed and uses the above parameters
sim_mr_es <- function(np, m_nt_lt, sd_nt_lt, m_nt_dis, sd_nt_dis,
                          m_sz_lt, sd_sz_lt, m_sz_dis, sd_sz_dis, which_effect) {
  
  data1 <- rnorm(np, m_nt_lt,  sd_nt_lt)
  data2 <- rnorm(np, m_nt_dis, sd_nt_dis)
  data3 <- rnorm(np, m_sz_lt,  sd_sz_lt)
  data4 <- rnorm(np, m_sz_dis, sd_sz_dis)

  nt_data <- data.frame(lt_diff=data1, dis=data2, my_group="nt")
  sz_data <- data.frame(lt_diff=data3, dis=data4, my_group="sz")

  combined_data <- rbind(nt_data, sz_data)

  # Get results for an ANOVA (effect of group and inference)
  mr_results <- lm(data=combined_data,
                   formula=lt_diff ~ my_group + dis + my_group:dis)

    # Note that for "which_effect":
    # "2" indicates the p-value for group
    # "3" indicates the p-value for mss_b_dis
    # "4" indicates the p-value for group:mss_b_dis (i.e., the interaction effect)
  out <- as.data.frame(rbind(np, summary(mr_results)$coefficients[which_effect,4]))
}

# Run the function created above for all numbers of participants w/in the specified range
# For each number of participants, it runs for the number of repetitions given above
sim_all_mrs_es <- function(params, which_effect) {
  # Run the simulations and produce a list
  p_values_list <- mapply(sim_mr_es,
                          np = params$np,
                          m_nt_lt   = params$m_nt_lt,  sd_nt_lt   = params$sd_nt_lt,
                          m_nt_dis  = params$m_nt_dis, sd_nt_dis  = params$sd_nt_dis,
                          m_sz_lt   = params$m_sz_lt,  sd_sz_lt   = params$sd_sz_lt,
                          m_sz_dis  = params$m_sz_dis, sd_sz_dis  = params$sd_sz_dis,
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
  geom_hline(yintercept = 0.05, color = "red") + geom_point() + geom_line() + 
  expand_limits(x = 0, y = 0) +
  scale_color_manual(values = c("50th"="black", "80th"="blue", "95th"="deepskyblue")) +
  xlab("Participants Per Group") + ylab("P-value") + ggtitle(my_title) +
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
  xlab("Desired Statistical Power (%)") + ylab("Participants Needed Per Group") + ggtitle(my_title) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5))  
}


# Run the simulations ----

tic("T-test simulation")

# Simulate the t-tests and create a data frame of the p-values and number of participants
np1 <- seq(10, 600, 10)    # Sequence of numbers (min, max, increment)
params1 <- create_t_test_params(np1, 20000)
p_values1 <- sim_all_t_tests(params1)

toc()

# Create plots based on the simulation results
# plot1a        <- plot_means(p_values1)
percentiles1  <- prepare_50_80_95(p_values1)
plot1b        <- plot_50_80_95(percentiles1, "Neurotypical vs. Schizophrenia (T-Test)", np1)
plot1c        <- plot_power(p_values1, "Neurotypical vs. Schizophrenia (T-Test)", np1)

# Print plots
# plot1a
plot1b
plot1c


tic("multiple regression simulation")

# Simulate the ANOVAs (equal samples) and create a data frame of the p-values and number of participants
# Remember that np1 is the number of participants PER GROUP
np2 <- seq(100, 3000, 100)
params2 <- create_mr_params_es(np2, 2500)
p_values2 <- sim_all_mrs_es(params2, 2)  # Second parameter is which effect

toc()

# Create plots based on the simulation results
# plot2a       <- plot_means(p_values2)
percentiles2 <- prepare_50_80_95(p_values2)
plot2b       <- plot_50_80_95(percentiles2, "Neurotypical vs. Schizophrenia (Regr.)", np2)
plot2c       <- plot_power(p_values2, "Neurotypical vs. Schizophrenia (Regr.)", np2)

# Print plots
# plot2a
plot2b
plot2c

stop()
