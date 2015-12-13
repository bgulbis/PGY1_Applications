# analyze.R
# 
# make covariates to use for prediction model
# 

library(stringr)
library(dplyr)

if (!exists("lor")) {
    lor <- readRDS("lor.Rds")
}

# get the scores entered by the reviewers for each applicant; take the average
# of the two scores
applicants <- select(download.data, cas_id, contains("assignments")) %>%
    rename(total.score0 = assignments_letters_of_recommendation_score_0,
           total.score1 = assignments_letters_of_recommendation_score_1,
           rec.score0 = assignments_letters_of_recommendation_question_review_of_the_recommendations_.6_to_9_score_0,
           rec.score1 = assignments_letters_of_recommendation_question_review_of_the_recommendations_.6_to_9_score_1) %>%
    gather(score, value, total.score0:rec.score1) %>%
    extract(score, c("score.type", "score.num"), "(.*)([0-3])$") %>%
    group_by(cas_id, score.type) %>%
    summarize(avg.score = mean(value, na.rm = TRUE)) %>%
    spread(score.type, avg.score) %>%
    filter(!is.na(rec.score),
           !is.na(total.score))

assess <- lor %>%
    mutate(length = str_length(comment),
           neg.improve = str_count(comment, regex("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improve(ment)?", ignore_case = TRUE)),
           pos.concern = str_count(comment, regex("(not|no)( +[^ ]+){0,5} concern", ignore_case = TRUE)),
           neg.struggle = str_count(comment, regex("struggle", ignore_case = TRUE)),
           pos.difficult = str_count(comment, regex("difficult", ignore_case = TRUE))) %>%
    group_by(cas_id) %>%
    summarize(avg.length = mean(length),
              neg.improve = sum(neg.improve, na.rm = TRUE),
              pos.concern = sum(pos.concern, na.rm = TRUE),
              neg.struggle = sum(neg.struggle, na.rm = TRUE)) %>%
    inner_join(applicants, by="cas_id")
# summarise_each(funs(sum(., na.rm = TRUE)), improve:difficult)

test <- grep("difficult[^y]", lor$comment, value = TRUE, ignore.case = TRUE)
test

# did/continue to/consistently improve; big/huge/great/significant improvement; improve(d) greatly/quickly
# will succeed; self-motivated; without prompt(ing/ed); seek(s/ing) out ways; independent

# will benefit from residency; adequate

# unfortunately; never/not consistent; did not/could not
# needed/required guidance
