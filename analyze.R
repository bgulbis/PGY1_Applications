# analyze.R
# 
# make covariates to use for prediction model
# 

library(stringr)
library(dplyr)

low.perform <- lor %>%
    mutate(length = str_length(comment),
           improve = str_count(comment, regex("(need|room|could|area)(s|ed|ing)?( +[^ ]+){0,5} improv", ignore_case = TRUE)), 
           struggle = str_count(comment, regex("struggle", ignore_case = TRUE)),
           concern = str_count(comment, regex("concern", ignore_case = TRUE)),
           difficult = str_count(comment, regex("difficult", ignore_case = TRUE))) 

test <- grep("improv", lor$comment, ignore.case = TRUE, value = TRUE)
text <- str_extract(lor$comment, "([^ ]+ +){0,5}improv([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text[temp == FALSE]

# did/continue to/consistently improve; big/huge/great/significant improvement; improve(d) greatly/quickly
# will succeed; self-motivated; without prompt(ing/ed); seek(s/ing) out ways; independent

# will benefit from residency; adequate

# unfortunately; never/not consistent; did not/could not
# needed/required guidance

assess <- low.perform %>%
    group_by(cas_id) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), improve:difficult)