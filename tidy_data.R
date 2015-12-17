# tidy_data.R
# 
# tidy the data from PhORCAS
# 

library(tidyr)
library(dplyr)
library(stringr)

if (!exists("data")) {
    data <- readRDS("phorcas_data.Rds")
}

# PhORCAS Id for PGY1
program.id <- 1634

# get raw data for letters and references
data.ref <- data[[1]]
data.intent <- data[[2]]

# determine how many references there were for each candidate, remove duplicates
references <- data.ref %>%
    filter(designation_program_lookup_id == 1634) %>%
    select(cas_id:evaluator_id_for_references_3) %>%
    gather(ref_num, writer_id, evaluator_id_for_references_0:evaluator_id_for_references_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id, writer_id) %>%
    distinct

# gather the ratings into long data format, remove those rows which correspond
# to duplicate evaluations
rating <- data.ref %>%
    filter(designation_program_lookup_id == 1634) %>%
    select(cas_id, matches("reference_(.*)_rating_[0-3]$")) %>%
    gather(ref_num, rating, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_rating_([0-3])$") %>%
    mutate(ref_num = as.numeric(ref_num),
           rating = factor(rating, exclude = "")) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:rating)

# gather the comments into long data format, remove those rows which correspond
# to duplicate evaluations
comments <- data.ref %>%
    filter(designation_program_lookup_id == 1634) %>%
    select(cas_id, matches("reference_(.*)_comments_[0-3]$")) %>%
    gather(ref_num, comment, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_rating_comments_([0-3])$") %>%
    mutate(ref_num = as.numeric(ref_num), 
           comment = str_replace_all(comment, "\\n", " ")) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:comment)

# combine rating and comments 
lor <- inner_join(rating, comments, by=c("cas_id", "quality", "ref_num")) %>%
    arrange(cas_id, quality, ref_num)

rm(rating, comments)

saveRDS(lor, "lor.Rds")
