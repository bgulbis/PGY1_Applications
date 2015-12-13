# analyze.R
# 
# make covariates to use for prediction model
# 

library(stringr)
library(dplyr)

if (!exists("lor")) {
    lor <- readRDS("lor.Rds")
}

low.perform <- lor %>%
    mutate(length = str_length(comment),
           improve = str_count(comment, regex("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improve(ment)?", ignore_case = TRUE))) 


test <- grep("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improve(ment)?", lor$comment, value = TRUE, ignore.case = TRUE)
test
# did/continue to/consistently improve; big/huge/great/significant improvement; improve(d) greatly/quickly
# will succeed; self-motivated; without prompt(ing/ed); seek(s/ing) out ways; independent

# will benefit from residency; adequate

# unfortunately; never/not consistent; did not/could not
# needed/required guidance

assess <- low.perform %>%
    group_by(cas_id) %>%
    summarize(avg.length = mean(length),
              neg.improve = sum(improve, na.rm = TRUE))
    # summarise_each(funs(sum(., na.rm = TRUE)), improve:difficult)