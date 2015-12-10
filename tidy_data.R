# tidy_data.R
# 
# tidy the data from PhORCAS
# 

library(tidyr)
library(dplyr)
library(stringr)

# determine how many references there were for each candidate, remove duplicates
references <- select(ref.data, cas_id:evaluator_id_for_references_3) %>%
    gather(ref_num, writer_id, evaluator_id_for_references_0:evaluator_id_for_references_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id, writer_id) %>%
    distinct

# gather the time mangement ratings into long data format, remove those rows
# which correspond to duplicate evaluations
rating <- select(ref.data, cas_id, reference_time_management_rating_0:reference_time_management_rating_3) %>%
    gather(ref_num, rating, reference_time_management_rating_0:reference_time_management_rating_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:rating)

# gather the time management comments into long format, remove those rows which
# correspond to duplicate evaluations; join with the time management ratings
time.mgmt <- select(ref.data, cas_id, reference_time_management_rating_comments_0:reference_time_management_rating_comments_3) %>%
    gather(ref_num, comment, reference_time_management_rating_comments_0:reference_time_management_rating_comments_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]")),
           quality = "Time Management") %>%
    inner_join(rating, by=c("cas_id", "ref_num"))

# repeat the process for other characteristics
rating <- select(ref.data, cas_id, reference_problem_solving_rating_0:reference_problem_solving_rating_3) %>%
    gather(ref_num, rating, reference_problem_solving_rating_0:reference_problem_solving_rating_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:rating)

prob.solve <- select(ref.data, cas_id, reference_problem_solving_rating_comments_0:reference_problem_solving_rating_comments_3) %>%
    gather(ref_num, comment, reference_problem_solving_rating_comments_0:reference_problem_solving_rating_comments_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]")),
           quality = "Problem Solving") %>%
    inner_join(rating, by=c("cas_id", "ref_num"))

rating <- select(ref.data, cas_id, reference_independence_rating_0:reference_independence_rating_3) %>%
    gather(ref_num, rating, reference_independence_rating_0:reference_independence_rating_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:rating)

independ <- select(ref.data, cas_id, reference_independence_rating_comments_0:reference_independence_rating_comments_3) %>%
    gather(ref_num, comment, reference_independence_rating_comments_0:reference_independence_rating_comments_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]")),
           quality = "Independence") %>%
    inner_join(rating, by=c("cas_id", "ref_num"))

rating <- select(ref.data, cas_id, reference_emotional_maturity_rating_0:reference_emotional_maturity_rating_3) %>%
    gather(ref_num, rating, reference_emotional_maturity_rating_0:reference_emotional_maturity_rating_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:rating)

mature <- select(ref.data, cas_id, reference_emotional_maturity_rating_comments_0:reference_emotional_maturity_rating_comments_3) %>%
    gather(ref_num, comment, reference_emotional_maturity_rating_comments_0:reference_emotional_maturity_rating_comments_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]")),
           quality = "Maturity") %>%
    inner_join(rating, by=c("cas_id", "ref_num"))

# bind all the characteristics together into a single table
lor <- bind_rows(time.mgmt, prob.solve, independ, mature) %>%
    mutate(comment = ifelse(comment == "", NA, comment),
           quality = factor(quality),
           rating = factor(rating, exclude = "")) %>%
    arrange(cas_id, quality, ref_num)
