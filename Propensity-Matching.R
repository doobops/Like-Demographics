# Purpose: To determine if the treatment, 'likedem', has an effect on the outcome, 'zeed, after controlling for covariates
# Variables:    
      # -- Outcome variable = posttest score i.e. 'zeed'
      # -- Treatment variable = demographic matchup i.e. 'likedem'
      # -- Covariates = predictors of demographic matchup i.e. "age.s", "pamath", "paela", "likegen", "ell", "sped", "ed", "retained", "d75", "d79"  

# Setup    
    rm(list=ls())
    
    require(tidyverse)
    require(dplyr)
    require(haven)
    require(MatchIt)
    require(purrr)
    
    path = "//es02cifs03/ord$/BumbleB/Ratings and Demographics Research/Like Demos/"

    
# Read in tables
    sdemos = read.csv(file.path(path, "student_demos.csv")) %>%
      filter(!is.na(ell))
    tdemos = read.csv(file.path(path, "teacher_demos.csv")) %>%
      filter(!is.na(ethnicity) & !is.na(overall_rating))
    scores = read.csv(file.path(path, "achievement.csv")) %>%
      mutate(ela = ifelse(substr(asmtid_mod, 3,3)=="E", 1, 0))

    
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
             likegen = ifelse(male.s ==1 & male.t == 1, 1, 0))


# 1. Check if dataset is unbalanced
    mean(joined$likedem)
      # -- 31.18% like demo
      # -- 68.82% diff demo
      # -- Fairly unbalanced, 1:2 ratio. Let's go ahead and generate propensity scores for likedem

    
# 2. Check if likedem is a predictor of achievement
    diff_mean_target = 
      with(joined, t.test(zeed ~ likedem))

    print(diff_mean_target)
      # -- Mean_zeed_likedem =  -0.04788199 
      # -- Mean_zeed_diffdem = -0.12573805
      # -- p-value < 2.2e-16
      # -- Basically, yes. Likedem is potentially a significant predictor of achievement. Let's go ahead and model achievemtn against likedem
    
    
# Select RHS variables for estimating likedem propensity score 
    cov = c("age.s", "age.t", "pamath", "paela", "likegen", "ell", "sped", "ed",
            "retained", "d75", "d79", "overall_rating", "mosl_rating", "motp_rating",
            "black", "white", "asian", "hispanic")
    
    fx_ttest = function(x){
      t.test(joined[,x] ~ joined[,'likedem'])
    }
    
    diff_mean_cov = 
      lapply(cov, fx_ttest)
    
    names(diff_mean_cov) = cov

    for(i in 1:length(cov)){
      if(diff_mean_cov[[cov[i]]]$p.value < 0.05){
        print(paste("Significant differences in", cov[[i]], "by likedem"))
      } else{
        print(paste("No significant differences in", cov[[i]], "by likedem"))        
      }
    }
        # -- Not significant: d79, overall_rating
        # -- Significant: all others


# Estimate likelihood by running logit model, where response variable is likedem and RHS are those we found were significantly related to likedem 
    mod_p = glm(likedem ~ age.s + age.t + pamath + paela + likegen + ell + sped + ed + retained + d75 + motp_rating + mosl_rating + fiscal_year + ela,
                family = binomial(), data = joined)

    summary(mod_p)
        # -- All coefficients are significant except sped. Drop d79 from RHS

    mod_p_reduced = glm(likedem ~ age.s + age.t + pamath + paela + likegen + ell + ed + retained + d75 + motp_rating + mosl_rating + fiscal_year + ela,
                 family = binomial(), data = joined)
    
    summary(mod_p_reduced)
        # -- All coefficients significant
    
    anova(mod_p, mod_p_reduced)
        # -- Barely any difference between the two models, so choosing new reduced model for parsimony
    
    df_p = data.frame(prpscore = predict(mod_p_reduced, type = "response"),
                      likedem = mod_p_reduced$model$likedem)

    
# Plot histogram of propensity scores by likedem
    ggplot(data = df_p, aes(x = prpscore)) +
      geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.01) +
      facet_wrap(~likedem) +
      labs(y = "Percent", x = "Probability of Having Like-Demos") # About the same distribution. yay!
    

# Matching algorithm
    joined_nomiss = joined %>%  # MatchIt does not allow missing values
      select(zeed, likedem, fiscal_year, ela, one_of(cov)) %>%
      na.omit()
    
    matched = 
      matchit(likedem ~ age.s + age.t + pamath + paela + likegen + ell + ed + retained + d75 + motp_rating + mosl_rating + fiscal_year + ela, 
                     method = "nearest", data = joined_nomiss)

    summary(test)  
        