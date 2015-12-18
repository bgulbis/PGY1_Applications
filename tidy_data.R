# tidy_data.R
# 
# tidy the data from PhORCAS
# 

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

if (!exists("data")) {
    data <- readRDS("phorcas_data.Rds")
}

# PhORCAS Id for PGY1
program.id <- 1634

# get raw data for letters and references
data.ref <- data$API_References
data.intent <- data$API_Intent
data.applicant <- data$API_Applicants

interest.areas <- c("Not Specified", "Ambulatory Care", "Cardiology", "Critical Care", 
               "Emergency Medicine", "Infectious Diseases", "Internal Medicine", 
               "Management", "Oncology", "Pediatrics", "Psychiatry", "Transplant", "Other")

# tidy applicant data
applicants <- data.applicant %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(-starts_with("custom_field_interest")) %>%
    mutate(pharmacy_school_gpa_collected = ifelse(pharmacy_school_gpa_collected == "Y", TRUE, FALSE),
           custom_field_mh.tmc_rec = ifelse(custom_field_school_score == "Y", TRUE, FALSE),
           pharmacy_school_graduation_date = ymd(pharmacy_school_graduation_date),
           decision_code = factor(decision_code, exclude = ""),
           citizenship_status = factor(citizenship_status, exclude = ""))

saveRDS(applicants, "applicants.Rds")

# gather applicant areas of interests
interests <- data.applicant %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, starts_with("custom_field_interest")) %>%
    mutate_each(funs(ifelse(. == "", NA, .)), starts_with("custom_field_interest")) %>%
    gather(interest_num, interest, starts_with("custom_field_interest"), na.rm = TRUE) %>%
    mutate(interest_num = as.numeric(str_extract(interest_num, "[0-9]")),
           interest = factor(interest, levels = interest.areas))

saveRDS(interests, "interests.Rds")

# determine how many references there were for each candidate, remove duplicates
references <- data.ref %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id:evaluator_id_for_references_3) %>%
    gather(ref_num, writer_id, evaluator_id_for_references_0:evaluator_id_for_references_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id, writer_id) %>%
    distinct

# gather the ratings into long data format, remove those rows which correspond
# to duplicate evaluations
rating <- data.ref %>%
    filter(designation_program_lookup_id == program.id) %>%
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
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, matches("reference_(.*)_comments_[0-3]$")) %>%
    gather(ref_num, comment, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_comments_([0-3])$") %>%
    mutate(ref_num = as.numeric(ref_num),
           quality = str_replace(quality, "_rating", ""),
           comment = str_trim(str_replace_all(comment, "\\n", " "), side = "both"),
           comment = ifelse(comment == "", NA, comment)) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:comment)

# combine rating and comments 
lor <- full_join(rating, comments, by=c("cas_id", "quality", "ref_num")) %>%
    mutate(quality = factor(quality)) %>%
    arrange(cas_id, quality, ref_num)

# rm(rating, comments)

saveRDS(lor, "lor.Rds")

# gather the letter of intent information into long data format
intent <- data.intent %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, matches("assignments_")) %>%
    gather(question, response, starts_with("assignments_"), na.rm = TRUE) %>%
    mutate(response = str_trim(str_replace_all(response, "\\n", " "), side = "both"),
           question = str_replace(question, "(.*)why_do_you_want(.*)", "motivation"),
           question = str_replace(question, "(.*)what_are_you_expecting(.*)", "expectations"),
           question = str_replace(question, "(.*)what_are_your_goals(.*)", "goals"),
           question = str_replace(question, "(.*)what_can_you_bring(.*)", "contributions")) %>%
    mutate(question = factor(question))

saveRDS(intent, "intent.Rds")
