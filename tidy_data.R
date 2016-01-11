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
data.extract <- data$API_Extraction
data.applicant <- data$API_Applicants
data.vidyo <- data$API_Vidyo
data.scores <- data$API_Application_Scores

interest.areas <- c("Not Specified", "Ambulatory Care", "Cardiology", "Critical Care", 
               "Emergency Medicine", "Infectious Diseases", "Informatics", "Internal Medicine", 
               "Management", "Oncology", "Pediatrics", "Psychiatry", "Transplant", "Other")

# tidy applicant data
applicants <- data.applicant %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(-starts_with("custom_field_interest"), -starts_with("pharmacy_school")) %>%
    mutate(custom_field_mh.tmc_rec = ifelse(custom_field_mh.tmc_rec == "Y", TRUE, FALSE),
           decision_code = factor(decision_code, exclude = ""),
           citizenship_status = factor(citizenship_status, exclude = ""))

names(applicants) <- str_replace_all(names(applicants), "custom_field_", "")

schools <- data.applicant %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, starts_with("pharmacy_school")) %>%
    mutate_each(funs(ifelse(. == "Y", TRUE, FALSE)), contains("gpa_collected")) %>%
    mutate_each(funs(ymd(.)), contains("graduation_date")) %>%
    mutate(two.schools = ifelse(pharmacy_school_graduation_date_0 == pharmacy_school_graduation_date_1 | 
                                    is.na(pharmacy_school_graduation_date_1), FALSE, TRUE)) %>%
    rename(school = pharmacy_school_name_0,
           gpa = pharmacy_school_gpa_0,
           gpa_collected = pharmacy_school_gpa_collected_0,
           graduation_date = pharmacy_school_graduation_date_0) %>%
    select(cas_id, school, gpa, gpa_collected, graduation_date, two.schools)

applicants <- left_join(applicants, schools, by = "cas_id")

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

levels(lor$quality) <- c("Assertive", "Problem Solving", "Criticism", "Dependable", "Maturity", "Independence", 
                         "Leadership", "Oral Comm", "Patient Interact", "Peer Comm", "Professional", 
                         "Time Managemetn", "Written Comm")
saveRDS(lor, "lor.Rds")

# strengths / weaknesses

ref.assess <- data.ref %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, contains("other_observances"), contains("description"), contains("reference_comments")) %>%
    gather(ref_num, comment, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_([0-3])$") %>%
    mutate(ref_num = as.numeric(ref_num),
           quality = factor(quality),
           comment = str_trim(str_replace_all(comment, "\\n", " "), side = "both"),
           comment = ifelse(comment == "", NA, comment)) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:comment)

levels(ref.assess$quality) <- c("Comments", "Weaknesses", "Strengths")

saveRDS(ref.assess, "ref.assess.Rds")

# gather the letter of intent information into long data format
intent <- data.extract %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, contains("comments")) %>%
    mutate_each(funs(as.character(.)), contains("comments")) %>%
    gather(question, response, contains("comments"), na.rm = TRUE) %>%
    mutate(response = str_trim(str_replace_all(response, "\\n", " "), side = "both"),
           question = str_replace(question, "(.*)why_do_you_want(.*)", "motivation"),
           question = str_replace(question, "(.*)what_are_you_expecting(.*)", "expectations"),
           question = str_replace(question, "(.*)what_are_your_goals(.*)", "goals"),
           question = str_replace(question, "(.*)what_can_you_bring(.*)", "contributions"),
           question = str_replace(question, "(.*)other_statements(.*)", "other_letter"),
           question = str_replace(question, "(.*)other_findings_in_cv(.*)", "other_cv"),
           question = str_replace(question, "(.*)extraction_comments(.*)", "reviewer_comments")) %>%
    mutate(question = factor(question)) %>%
    spread(question, response)

saveRDS(intent, "intent.Rds")

# data extracted from CV
cv <- data.extract %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, contains("score")) 

names(cv) <- str_replace_all(names(cv), "assignments_data_extraction_question_number_of_", "")
names(cv) <- str_replace_all(names(cv), "_score", "")
names(cv) <- str_replace_all(names(cv), "rotations_(.*)_centers", "academic_rotations")
names(cv) <- str_replace_all(names(cv), "assignments_(.*)_rotations", "num_rotations")
names(cv) <- str_replace_all(names(cv), "peer(.*)_publications", "publications")
names(cv) <- str_replace_all(names(cv), "state_national_", "")

saveRDS(cv, "cv.Rds")

# application scores
app.scores <- data.scores %>%
    filter(designation_program_lookup_id == program.id) %>%
    mutate_each(funs(str_trim(str_replace_all(., "\\n", " "), side = "both")), contains("comments"))

names(app.scores) <- str_replace_all(names(app.scores), "assignments_application_scoring_question_", "")
names(app.scores) <- str_replace_all(names(app.scores), "_(.?[0-9])_to_[0-9]", "")
names(app.scores) <- str_replace_all(names(app.scores), "assignment(s)?_application_scoring", "application")
names(app.scores) <- str_replace_all(names(app.scores), "contributions_(.*)_patients", "contributions")
names(app.scores) <- str_replace_all(names(app.scores), "expecting_(.*)_residency", "expectations")
names(app.scores) <- str_replace_all(names(app.scores), "motivation_(.*)_residency", "motivation")
names(app.scores) <- str_replace_all(names(app.scores), "followed_(.*)_instructions", "instructions")
names(app.scores) <- str_replace_all(names(app.scores), "any_spelling_(.*)_application", "spelling")
names(app.scores) <- str_replace_all(names(app.scores), "other_(.*)_awarded", "other")
names(app.scores) <- str_replace_all(names(app.scores), "recommender_(.*)_us", "known_recommender")
names(app.scores) <- str_replace_all(names(app.scores), "review_(.*)_recommendations", "recs")
names(app.scores) <- str_replace_all(names(app.scores), "clinical_(.*)_competition", "clin_skills")
names(app.scores) <- str_replace_all(names(app.scores), "lcep(.*)_program", "lcep")
names(app.scores) <- str_replace_all(names(app.scores), "leadership_experience", "leadership")
names(app.scores) <- str_replace_all(names(app.scores), "long(.*)_goals", "goals")

app.scores <- app.scores %>%
    mutate(application_reviewer = factor(application_reviewer),
           application_remark = as.numeric(str_extract(application_remark, "[0-9]")))

saveRDS(app.scores, "app.scores.Rds")

# vidyo interviews
vidyo <- data.vidyo %>%
    filter(designation_program_lookup_id == program.id) %>%
    mutate_each(funs(str_trim(str_replace_all(., "\\n", " "), side = "both")), contains("comments"))

names(vidyo) <- str_replace_all(names(vidyo), "interview(s)?_vidyo_interview_(question_)?", "")
names(vidyo) <- str_replace_all(names(vidyo), "critical_(.*)_task", "crit_think")
names(vidyo) <- str_replace_all(names(vidyo), "time_(.*)_assignments", "time_mgmt")
names(vidyo) <- str_replace_all(names(vidyo), "self(.*)_obstacle", "prob_solve")
names(vidyo) <- str_replace_all(names(vidyo), "integrity_(.*)_harmed", "integrity")

vidyo <- vidyo %>%
    mutate(interviewer = factor(interviewer),
           remarks = as.numeric(str_extract(remarks, "[0-9]")))

saveRDS(vidyo, "vidyo.Rds")
