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

source("input/parms_wm_drywhite.R")


################################################################################
## Pull all sales, weeks, stores, parts
## it's a little big, want to drop asap
##
## find test and control store groups
################################################################################

target_customer_s <- paste0("'",as.character(target_customer),"'")
pre_period_start_dt_s <- paste0("'",as.character(pre_period_start_dt),"'")
test_cutoff_dt_s <- paste0("'",as.character(test_cutoff_dt),"'")
part_numbers_s <- paste0("'",as.character(part_numbers),"'",collapse=",")

sql <- read_file("snowflake_sql_pulls/pull_custom_stores_sales.sql")
query <- sprintf(sql, target_customer_s,pre_period_start_dt_s, test_cutoff_dt_s, part_numbers_s)

detailed_data <-  DBI::dbGetQuery (con_snowflake, 
                                   statement = query) %>%
  clean_names()

## QA stores
stores <- detailed_data %>% 
  summarize(stores=n_distinct(store_number))
stores
##
## walmart 4637

week_lkp <- detailed_data %>%
  filter(updated_week_ending_date >= intervention_dt) %>%
  select(updated_week_ending_date) %>%
  distinct() %>%
  count() %>%
  pull()

# Filter part numbers that have a max(updated_week_ending_date) of 2024-07-21
filtered_data <- detailed_data %>%
  group_by(part_number) %>%
  filter(max(updated_week_ending_date) == as.Date("2024-07-21"))

control_stores_lkp <- filtered_data %>%
  filter(updated_week_ending_date >= intervention_dt) %>%
  filter(part_number == target_part_numbers & pos_sales >0) %>%
  group_by(store_number) %>%
  summarise(weeks = n(), round(sum(pos_sales)/n(),2), sum(pos_sales))


######################
## Ben edits above ##
#####################

control_stores <- detailed_data %>% 
  select('store_number') %>%
  distinct() %>% 
  anti_join(cant_be_control_stores) %>% 
  mutate(test_control = 'C')

test_stores <- detailed_data %>% 
  inner_join(cant_be_control_stores) %>% 
  filter(part_number == target_part_numbers & 
           updated_week_ending_date >= as.Date(intervention_dt) & updated_week_ending_date <= as.Date(intervention_dt) +28) %>%  
  select('store_number')%>% 
  distinct() %>%
  mutate(test_control = 'T') 

stores <- rbind(control_stores, test_stores)
stores$randoms=runif(length(unique(stores$store_number)), min=0, max=1)

## QA - test and control stores
stores %>% 
  group_by(test_control) %>% 
  summarise(n_stores=n_distinct(store_number))

#walmart - all weather only
#test_control n_stores
# C                358
# T                 3275
#walmart - all tape
#test_control n_stores
# C                1006
# T                 3275

rm(control_stores, test_stores, cant_be_control_stores)


# test and control df
## different paths based on whether or not the control stores should be split into different control groups
## fixed a problem with code for the TRUE option. a control with a random number ending in 0 could have been placed in test group

test_and_control <- detailed_data %>% 
  inner_join (stores, by=c("store_number")) %>% 
  mutate(pre_post = case_when(updated_week_ending_date >= pre_period_start_dt &  updated_week_ending_date <= pre_period_end_dt ~ 'PRE',
                              updated_week_ending_date >= intervention_dt &  updated_week_ending_date <= test_cutoff_dt ~ 'POST',
                                TRUE ~ 'BLKOUT'))%>% 
  mutate(randoms_char = if_else(test_control == 'T',0,1)) %>%
  mutate(random_group = str_sub(randoms_char,-1,-1)) %>%
  group_by(customer, test_control, random_group, updated_week_ending_date, pre_post) %>%
  summarise(pos_sales = sum(pos_sales), store_count = n_distinct(store_number))

# visualize control groups
test_and_control %>%
  filter(random_group == 1) %>%
  mutate(date = as.Date(updated_week_ending_date)) %>%
  ggplot(aes(x=date, y=pos_sales, color=random_group, group = random_group)) + 
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

###################################################################
# compare post-period pos for cannibalization
###################################################################

### Post-period store coverage

post_store_pos <- detailed_data %>% 
  inner_join(stores, by="store_number") %>% 
  filter(updated_week_ending_date >= intervention_dt & updated_week_ending_date <= test_cutoff_dt &
           test_control == 'T') %>%
  group_by(customer, part_number) %>%
  summarise(sales = round(sum(pos_sales),0), stores=n_distinct(store_number)) %>%
  mutate(total_sales = round(sum(sales),0)) %>%
  mutate(perc_sales = round(sales/total_sales,4)*100)
post_store_pos


# output csv
write.csv(test_and_control, paste0('output/',target_customer,'_', project_name,'_test_and_control.csv'))
write.csv(post_store_pos, paste0('output/',target_customer,'_', project_name,'_actual_test_stores.csv'))




