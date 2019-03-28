# Purpose: To determine if the treatment, 'likedem', has an effect on the growth based upon year over year pattern of like demos
# Variables:    
# -- Outcome variable = growth i.e. zeed - paela, or, zeed - pamath
# -- Treatment variable = demographic matchup at each year i.e. 'likedem'
# -- Additional considerations: Age at start of tracking

# Setup    
rm(list=ls())

require(tidyverse)
require(dplyr)
require(haven)
require(purrr)
require(stringi)

path = "//es02cifs03/ord$/BumbleB/Ratings and Demographics Research/Like Demos/"


# Read in tables
sdemos = read.csv(file.path(path, "student_demos.csv")) %>%
  filter(!is.na(ell)) %>%
  mutate(paela = round(paela, digits = 6),
         pamath = round(pamath, digits = 6))
tdemos = read.csv(file.path(path, "teacher_demos.csv")) %>%
  filter(!is.na(ethnicity) & !is.na(overall_rating))
scores = read.csv(file.path(path, "achievement.csv")) %>%
  mutate(ela = ifelse(substr(asmtid_mod, 3,3)=="E", 1, 0),
         zeed = round(zeed, digits = 6))


# Align var type         
sdemos$fiscal_year = as.numeric(sdemos$fiscal_year)
tdemos$fiscal_year = as.numeric(tdemos$fiscal_year)
scores$fiscal_year = as.numeric(scores$fiscal_year)
scores$student_id = as.numeric(scores$student_id)


# Convert ratings to ordinal var
tdemos$overall_rating = recode(tdemos$overall_rating, 'Highly Effective' = 4, 'Effective' = 3, 'Developing' = 2, 'Ineffective' = 1)
tdemos$motp_rating = recode(tdemos$motp_rating, 'Highly Effective' = 4, 'Effective' = 3, 'Developing' = 2, 'Ineffective' = 1)
tdemos$mosl_rating = recode(tdemos$mosl_rating, 'Highly Effective' = 4, 'Effective' = 3, 'Developing' = 2, 'Ineffective' = 1) 


# Create input dataset
joined = 
  inner_join(sdemos, scores, by=c("student_id", "fiscal_year")) %>%
  inner_join(., tdemos, by=c("employeeid", "fiscal_year"), suffix = c(".s", ".t")) %>%
  mutate(likedem = ifelse(hispanic == 1 & ethnicity == "HIS" | black == 1 & ethnicity == "BLK" | asian == 1 & ethnicity == "ASN" | white == 1 & ethnicity == "WHT", 1, 0),
         likegen = ifelse(male.s ==1 & male.t == 1, 1, 0)) %>%
  arrange(student_id, subject, fiscal_year) %>%
  group_by(student_id, subject, fiscal_year) %>%
  dplyr::mutate(likedem_reduced = sum(likedem)/n(),
                likegen_reduced = sum(likegen)/n())


# Keep where there are at least 3 years
three <- joined %>%
  group_by(student_id, ela) %>%
  filter(n_distinct(fiscal_year)>2 &
           !student_id==224000000) # Hard code out student with demographics that change every year...


# Divide out by subject & dedupe at student level
ela <- three %>% 
  filter(ela == 1) %>%
  mutate(growth = zeed - paela,
         likedem_reduced_flag = ifelse(likedem_reduced>0, 1, 0)) %>%
  select(-employeeid, -ends_with(".t"), -ends_with("rating"), -ends_with("pct"), -dob, -ethnicity, -likedem, -likegen, -calendar_days) %>%
  distinct() 

ela %>%
  group_by(student_id) %>% 
  dplyr::summarise(n=n()) %>%
  filter(n>4)

View(ela %>% filter(student_id == 203606934 & fiscal_year==2018))


# Examine like demos occurence pattern
pattern_list <- c()
pattern_list <- map(unique(ela$student_id),
               ~ paste0(ela$likedem_reduced_flag[ela$student_id == .x])) %>%
  set_names(unique(ela$student_id)) 

pattern_df <- ldply(pattern_list, rbind) 
pattern_df$likedem_pattern <- paste0(pattern_df[, c(2:18)])


  mutate(likedem_pattern = paste0(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`))

troubleshoot <- pattern_df %>% filter(!is.na(`18`))


ggplot(aes(x = likedem_pattern, stat="count")) +
  geom_bar(aes(y=100*(..count..)/sum(..count..)))

