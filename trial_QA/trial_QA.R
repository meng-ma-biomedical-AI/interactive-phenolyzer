library(dplyr)
library(tidyr)
library(xlsx)
library(janitor)
library(data.table)

setwd(getwd())
#wb = read.xlsx2((paste(pwd,"RecruitmentOptimizat_DATA_LABELS_2018-01-27_2135.csv",sep="/")),sheetIndex = 1)
input_csv = read.csv("RecruitmentOptimizat_DATA_LABELS_2018-02-09_0931.csv")

## define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

input_csv_clean <- input_csv %>%
  clean_names() %>%
  mutate_all(funs(empty_as_na)) 

# compile question set.

record_id = 
  input_csv_clean %>%
  select(record_id) %>%
  unique()

# questions for age - continues variable.
# var_name = "age_years_minimum"
questionForAge <- function(input_csv_clean,var_name){
  # find unique values.
  unique_value = input_csv_clean %>%
    filter(!is.na(var_name)) %>%
    select(var_name) %>%
    unique()
  class = sort(unique_value[,1])
  class_min = c(-999999999,class)
  class_max = c(class,999999999)
  question = data.frame(selection=c(1:length(class_min)),class_min,class_max) %>%
    mutate(choice = paste(class_min, class_max, sep = "-"))
  question$choice[1] = paste("<",class_min[2])
  question$choice[length(question$choice)] = paste(">",class_max[length(class_max)-1])
  question = question %>% 
    mutate(q = "What is your Age?") %>%
    mutate(class = NA) %>%
  select(q,choice,selection,class_min,class_max,class)
  return(question)
}

age_question <- questionForAge(input_csv_clean, "age_years_minimum")

# questions for age - continues variable.
# var_name = "condition"

clean_condition = input_csv_clean %>%
  filter(repeat_instrument == 'Conditions' & complete_2 == 'Complete') %>%
  select(record_id,condition,severity,duration,inclusion) %>%
  mutate(YES = (inclusion_or_exclusion == "Inclusion")) %>%
  mutate(NO = (inclusion_or_exclusion == "Exclusion")) %>%
  mutate(UK = TRUE)

clean_condition %>%
  group_by(.dots=c("condition","severity","duration_of_condition")) %>%
  summarise(YES_exclude = sum(YES==FALSE),
            NO_exclude = sum(NO==FALSE),
            UK_exclude = sum(UK==FALSE)) %>%
  arrange(-YES_exclude,-NO_exclude,UK_exclude)

questionForCondition <- function(input_csv_clean,condition,severity,duration,inclusion){
  # find unique trial
  record_id = 
    input_csv_clean %>%
    select(record_id) %>%
    unique()
  # find unique condition values.
  unique_value = input_csv_clean %>%
    filter(!is.na(condition)) %>%
    select(var_name,severity,duration) %>%
    unique()
  class = sort(unique_value[,1])
  
  
  
  p <- 
  
  question = 
    
  
  trial_question = expand.grid(record_id = record_id[,1],condition = unique_value) %>%
    left_join(input_csv_clean,by = c("record_id","condition")) %>%
    select(record_id,condition,inclusion_or_exclusion)
  
  return(trial_question)
}
  
condition_question <- questionForCondition(input_csv_clean, "condition")

conditionQuestionSelection <-



T_X_matrix = data.frame(trial = c(1:100), x1 = sample(c(0,1,NA),100,replace = T), 
                        x2 = sample(c(0,1,NA),100,replace = T), 
                        x3 = sample(c(0,1,2,3,NA),100,replace = T), 
                        x4 = sample(c(0,1,2,3,4,5,6,NA),100,replace = T), 
                        x5 = sample(c(0,1,NA),100,replace = T))


eRemain <- function(p,T_X_matrix_start){
  x_candidate = data.frame(x_candidate =   colnames(T_X_matrix_start)[-1])
  x_value_n = T_X_matrix_start %>% 
    gather(key = "x",value = 'value', -trial) %>%
    group_by(.dots = c('x','value')) %>%
    summarise(n = n())
  
  T_X_matrix_NA = x_value_n %>%
    filter(is.na(value)) %>%
    mutate(n_NA = n) %>%
    select(c("x","n_NA"))
  
  X_E <- x_value_n %>% 
    left_join(T_X_matrix_NA,by = "x") %>%
    filter(!is.na(value)) %>%
    mutate(n_remain = n + n_NA) %>%
    left_join(p,by = c("x","value")) %>%
    group_by(x) %>%
    summarise(E = sum(p*n_remain)) %>%
    arrange(E)
  
  return(X_E)

}


T_X_matrix_start = T_X_matrix[which(T_X_matrix$x1 == 1 | is.na(T_X_matrix$x1)),-2]
p = data.frame(x = rep("x2",2),value = c(0:1), p=rep(1/2,2)) %>%
  bind_rows(data.frame(x = rep("x3",4),value = c(0:3),p=rep(1/4,4))) %>%
  bind_rows(data.frame(x = rep("x4",7),value = c(0:6),p=rep(1/7,7))) %>%
  bind_rows(data.frame(x = rep("x5",2),value = c(0:1),p=rep(1/2,2)))
X_E <- eRemain(p,T_X_matrix_start = T_X_matrix_start)
X_E
