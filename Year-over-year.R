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
require(gridExtra)

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
  dplyr::mutate(growth = ifelse(zeed >= paela, abs(zeed - paela), abs(zeed - paela)*-1),
         growth_z = (growth-mean(growth, na.rm=TRUE))/n(), 
         likedem_reduced_flag = ifelse(likedem_reduced>0, 1, 0)) %>%
  select(-employeeid, -ends_with(".t"), -ends_with("rating"), -ends_with("pct"), -dob, -ethnicity, -likedem, -likegen, -calendar_days) %>%
  arrange(student_id, fiscal_year)

ela <- ela[!duplicated(ela[c("student_id","fiscal_year")]),]

ela %>%
  group_by(student_id) %>% 
  dplyr::summarise(n=n()) %>%
  filter(n>4)

View(ela %>% filter(student_id == 215111469 & fiscal_year == 2017))


# Examine like demos occurence pattern + PLOT
pattern_list <- c()
pattern_list <- map(unique(ela$student_id),
               ~ paste0(ela$likedem_reduced_flag[ela$student_id == .x])) %>%
  set_names(unique(ela$student_id)) 

pattern_df <- plyr::ldply(pattern_list, rbind) %>% 
  dplyr::rename("student_id" = 1, "fy1" = 2, "fy2" = 3, "fy3" = 4, "fy4" = 5) %>%
  mutate(likedem_pattern = gsub("NA", "", paste0(fy1, fy2, fy3, fy4)))

yoy_4 <- 
ggplot(pattern_df[!is.na(pattern_df$fy4), ], aes(x = likedem_pattern, stat="count")) +
  geom_bar(aes(y=100*(..count..)/sum(..count..))) 

yoy_3 <- 
ggplot(pattern_df[is.na(pattern_df$fy4), ], aes(x = likedem_pattern, stat="count")) +
  geom_bar(aes(y=100*(..count..)/sum(..count..)))

grid.arrange(yoy_3, yoy_4, nrow = 2)
ggsave(plot = yoy_3, filename = "D:/GitHub/Like-Demographics/out0.png")

# Examine effect of different patterns on total growth
growth <- 
ela %>%
  group_by(student_id) %>%
  # mutate(total_growth = sum(growth_z),
  mutate(total_growth = sum(growth),
         age_start = min(age.s),
         grade_start = min(grade)) %>%
  right_join(pattern_df %>% mutate(student_id = as.numeric(student_id)), by=c("student_id")) %>%
  distinct(student_id, black, white, hispanic, asian, ell, ed, sped, d75, d79, retained, grade_start, age_start, male.s, ela, total_growth, likedem_pattern) %>%
  filter(nchar(likedem_pattern)==3) %>%
  mutate(col = ifelse(likedem_pattern %in% c("000", "111"), "coral",
                        ifelse(likedem_pattern %in% c("001", "010", "100"), "chartreuse", "dodgerblue")))

plot <- 
function(x){
  
  df <- growth[growth$likedem_pattern %in% c(x) & !is.na(growth$total_growth), ]
  
  mean = round(mean(df$total_growth, na.rm=TRUE), digits=3)
  n = prettyNum(nrow(df[!is.na(df$total_growth), ]), big.mark=",")
  
  label = paste0("Mean = ", mean)
    
  ggplot(df, aes(total_growth, fill=col)) +
    geom_histogram(binwidth = .1, fill="slateblue", alpha = .70, aes(y=100*(..count..)/sum(..count..))) +
    geom_vline(xintercept = mean) +
    annotate(geom = "text", label = label, x = mean+1, y = 5) +
    ggtitle(paste0(x, " (n = ", n, ")"))+
    
    labs(x = "Total Growth", y = "Percent") + 
    
    theme(
      panel.background = element_rect(fill = "white"), 
      axis.line = element_line(),
      legend.key=element_blank(),
      legend.position = "none"
    )
}


p000 <- plot("000")
p111 <- plot("111")

p001 <- plot("001")
p010 <- plot("010")
p100 <- plot("100")

p011 <- plot("011")
p101 <- plot("101")
p110 <- plot("110")

out1 <- grid.arrange(p000, p111, nrow=1)
out2 <- grid.arrange(p001, p010, p100, nrow=1)
out3 <- grid.arrange(p011, p101, p110, nrow=1)

ggsave(plot = out1, filename = "D:/GitHub/Like-Demographics/out1.png")
ggsave(plot = out2, filename = "D:/GitHub/Like-Demographics/out2.png")
ggsave(plot = out3, filename = "D:/GitHub/Like-Demographics/out3.png")

growth %>%
  group_by(likedem_pattern) %>%
  summarise(mean = mean(total_growth, na.rm=TRUE))

summary(aov(total_growth~likedem_pattern, growth))
