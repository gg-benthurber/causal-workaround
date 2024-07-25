rm(list = ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# get directory
getwd()

################################################################################
### USER INPUTS ################################################################
################################################################################

# Pull in the parameters of the run
  #source("input/parms_wh_5oz_value_jar.R")
  #source("input/parms_wh_3oz_night.R")
  #source("input/parms_wh_7oz_tube.R")

source("input/parms_gorilla_hd_heavy_duty_mountt_vs_tc_mountt.R")


# input csvs
causal_input <- read.csv(paste0('output/',target_customer,'_', project_name,'_test_and_control.csv'))


################################################################################
################################################################################
################################################################################

# create dataframe with all the control values labeled X1,X2,X3... and test as X0
causal_input_x <- causal_input %>%
  dplyr::select(updated_week_ending_date, random_group, pos_sales) %>%
  pivot_wider(names_from = random_group, values_from =pos_sales, names_prefix = "X") %>%
  dplyr::rename(Y=X0) %>%
  arrange(updated_week_ending_date)

# Create a vector of all the weeks in pre-period and post-period
weeks <- causal_input_x %>% dplyr::select(updated_week_ending_date) %>% distinct() %>% pull()

# drop date from dataframe
causal_input_no_date <- causal_input_x %>%
  dplyr::select(-updated_week_ending_date) %>%
  dplyr::select(Y, X1)

# create a vector of the dates
time.points <- seq.Date(as.Date(pre_period_start_dt), by = 7, length.out = length(weeks))


# Bind the data for causal data
data <- zoo(causal_input_no_date, time.points)

# print visual
head(data)

print(data)

# establish the pre and post-period
pre.period <- as.Date(c(pre_period_start_dt[1],pre_period_end_dt[1]))
post.period <- as.Date(c(intervention_dt[1],test_cutoff_dt[1]))

# fit the model
impact <- CausalImpact(data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 52))
# plot the model
plot(impact)

# visualize summary
summary(impact)
# verbalize summary
summary(impact, "report")



######################################################
## print some output
######################################################

write.csv(impact$summary, paste0('output/',target_customer,'_', project_name,'_ci_summary.csv'))


