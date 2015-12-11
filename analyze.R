# analyze.R
# 
# make covariates to use for prediction model
# 

library(stringr)

low.perform <- mutate(lor, low = str_count(comment, regex("(need|room)(.*)improv", ignore_case = TRUE))) %>%
    filter(low > 0)
