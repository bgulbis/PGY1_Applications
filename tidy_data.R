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
data.interviews <- data$API_Interviews

interest.areas <- c("Not Specified", "Ambulatory Care", "Cardiology", "Critical Care", 
               "Emergency Medicine", "Infectious Diseases", "Informatics", "Internal Medicine", 
               "Management", "Oncology", "Pediatrics", "Psychiatry", "Transplant", "Other")

# tidy applicant data
applicants <- data.applicant %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(-starts_with("custom_field_interest"), -starts_with("pharmacy_school")) %>%
    # rename_(.dots = setNames(list(quote(custom_field_mh-tmc_rec)), "custom_field_mhtmc_rec")) %>%
    mutate(mh.tmc_rec = ifelse(quote(custom_field_mh-tmc_rec) == "Y", TRUE, FALSE),
           decision_code = factor(decision_code, exclude = ""),
           citizenship_status = factor(citizenship_status, exclude = "")) %>%
    select(-starts_with("custom_field_mh"))

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
    group_by(cas_id, writer_id) 

ref.first.names <- data.ref %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, reference_first_name_0:reference_first_name_3) %>%
    gather(ref_num, ref_first_name, reference_first_name_0:reference_first_name_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id) %>%
    arrange(ref_num)

ref.last.names <- data.ref %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, reference_last_name_0:reference_last_name_3) %>%
    gather(ref_num, ref_last_name, reference_last_name_0:reference_last_name_3, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id) %>%
    arrange(ref_num)

ref.names <- inner_join(ref.first.names, ref.last.names, by = c("cas_id", "ref_num")) %>%
    filter(ref_last_name != "")

references <- inner_join(references, ref.names, by = c("cas_id", "ref_num"))

saveRDS(references, "references.Rds")

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
           comment = str_trim(str_replace_all(comment, "(\\n|\\t)", " "), side = "both"),
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
           comment = str_trim(str_replace_all(comment, "(\\n|\\t)", " "), side = "both"),
           comment = ifelse(comment == "", NA, comment)) %>%
    inner_join(references, by=c("cas_id", "ref_num")) %>%
    select(cas_id:comment)

levels(ref.assess$quality) <- c("Comments", "Weaknesses", "Other", "Strengths")

saveRDS(ref.assess, "ref.assess.Rds")

# gather the letter of intent information into long data format
intent <- data.extract %>%
    filter(designation_program_lookup_id == program.id) %>%
    select(cas_id, contains("comments")) %>%
    mutate_each(funs(as.character(.)), contains("comments")) %>%
    gather(question, response, contains("comments"), na.rm = TRUE) %>%
    mutate(response = str_trim(str_replace_all(response, "(\\n|\\t)", " "), side = "both"),
           response = str_replace_all(response, "â\u0080\u0099", "'"),
           response = str_replace_all(response, "â\u0080\u0093", "-"),
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
    mutate_each(funs(str_trim(str_replace_all(., "(\\n|\\t)", " "), side = "both")), contains("comments"))

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
    mutate_each(funs(str_trim(str_replace_all(., "(\\n|\\t)", " "), side = "both")), contains("comments"))

names(vidyo) <- str_replace_all(names(vidyo), "interview(s)?_vidyo_interview_(question_)?", "")
names(vidyo) <- str_replace_all(names(vidyo), "critical_(.*)_task", "crit_think")
names(vidyo) <- str_replace_all(names(vidyo), "time_(.*)_assignments", "time_mgmt")
names(vidyo) <- str_replace_all(names(vidyo), "self(.*)_obstacle", "prob_solve")
names(vidyo) <- str_replace_all(names(vidyo), "integrity_(.*)_harmed", "integrity")

vidyo <- vidyo %>%
    mutate(interviewer = factor(interviewer),
           remarks = as.numeric(str_extract(remarks, "[0-9]")))

saveRDS(vidyo, "vidyo.Rds")

cand.interests <- interests %>%
    spread(interest_num, interest)

names(cand.interests) <- c("cas_id", "interest1", "interest2", "interest3")

# score summary
result.summary <- select(applicants, cas_id:last_name, citizenship_status:two.schools) %>%
    inner_join(cand.interests, by = "cas_id") %>%
    inner_join(select(app.scores, cas_id, application_reviewer:spelling_score), by = "cas_id") %>%
    inner_join(cv, by = "cas_id") %>%
    inner_join(select(vidyo, cas_id, interviewer:score, crit_think_score:integrity_score), by = "cas_id") %>%
    group_by(cas_id) %>%
    mutate(overall.score = school_score + application_score + score,
           avg.fit = mean(c(application_remark, remarks), na.rm = TRUE),
           low.fit = ifelse(application_remark < 3 | remarks < 3, TRUE, FALSE),
           low.school = ifelse(school_score < 2, TRUE, FALSE)) %>%
    ungroup %>%
    arrange(desc(overall.score), desc(avg.fit)) %>%
    select(cas_id, last_name, first_name, school, overall.score, avg.fit, low.fit, low.school, everything())

write.csv(result.summary, "result.summary.csv", row.names = FALSE)

saveRDS(result.summary, "result.summary.Rds")

# interview scores ----

# get fit scores
interview_remarks <- data.interviews %>% 
    select(cas_id, contains("remarks")) %>%
    mutate_each(funs(str_extract(., ": ([0-9])")), -cas_id) %>%
    mutate_each(funs(str_extract(., "([0-9])")), -cas_id) %>%
    mutate_each(funs(as.numeric), -cas_id)

names(interview_remarks) <- str_replace_all(names(interview_remarks), "(interview|preceptor)_", "")
names(interview_remarks) <- str_replace_all(names(interview_remarks), "_1-2", "JC")
names(interview_remarks) <- str_replace_all(names(interview_remarks), "_3-4", "AS")

# summarize the fit scores
interview_remarks_avg <- interview_remarks %>%
    group_by(cas_id) %>%
    gather(interview_group, remark, -cas_id) %>%
    summarize(interview.remarks.median = median(remark, na.rm = TRUE),
              interview.remarks.mean = mean(remark, na.rm = TRUE),
              interview.remarks.min = min(remark, na.rm = TRUE),
              interview.remarks.max = max(remark, na.rm = TRUE),
              interivew.remarks.var = var(remark, na.rm = TRUE),
              # interview.remarks.sd = sd(remark, na.rm = TRUE),
              interview.remarks.low = sum(remark <= 2, na.rm = TRUE))

evaluator.compare <- interview_remarks %>%
    group_by(cas_id) %>%
    gather(interview_group, remark, -cas_id) %>%
    mutate(interview_group = str_replace_all(interview_group, "_remarks", ""),
           interview_group = str_replace_all(interview_group, "coachability", "coachability_0"),
           interview_group = str_replace_all(interview_group, "mmi12", "mmiJC"),
           interview_group = str_replace_all(interview_group, "mmi34", "mmiAS")) %>%
    separate(interview_group, c("interview_group", "reviewer"), "_") %>%
    ungroup %>%
    group_by(cas_id, interview_group) %>%
    summarize(med.remark = median(remark, na.rm = TRUE)) %>%
    spread(interview_group, med.remark)
    
evaluator.compare.sum <- evaluator.compare %>%
    ungroup %>%
    select(-cas_id) %>%
    summarize_each(funs(mean(., na.rm = TRUE), median(., na.rm = TRUE)))

# get scores from interview questions
interview_questions <- data.interviews %>%
    select(cas_id, contains("score")) %>%
    select(cas_id, contains("question"))

names(interview_questions) <- str_replace_all(names(interview_questions), "(interview|preceptor|question|score)_", "")
names(interview_questions) <- str_replace_all(names(interview_questions), "_1-2", "JC")
names(interview_questions) <- str_replace_all(names(interview_questions), "_3-4", "AS")
names(interview_questions) <- str_replace_all(names(interview_questions), "-_", "")
names(interview_questions) <- str_replace_all(names(interview_questions), "_mmi_[0-9]", "")
names(interview_questions) <- str_replace_all(names(interview_questions), "time_management", "timeMgmt")
names(interview_questions) <- str_replace_all(names(interview_questions), "critical_thinking", "critThink")
names(interview_questions) <- str_replace_all(names(interview_questions), "difficult_person", "dfcltPerson")
names(interview_questions) <- str_replace_all(names(interview_questions), "_(difficult_decision|large_workload)", "A")
names(interview_questions) <- str_replace_all(names(interview_questions), "_(short_deadline|unclear_expectations)", "B")
names(interview_questions) <- str_replace_all(names(interview_questions), "(critical|new_drug_disease_state|stressful_situation)_", "")

interview_questions <- interview_questions %>%
    group_by(cas_id) %>%
    gather(question, score, -cas_id) %>%
    separate(question, c("group", "question", "reviewer"), "_") 

interview_score <- interview_questions %>%
    summarize(score.sum = sum(score, na.rm = TRUE),
              score.med = median(score, na.rm = TRUE),
              score.min = min(score, na.rm = TRUE),
              score.max = max(score, na.rm = TRUE),
              score.var = var(score, na.rm = TRUE))

interview_groups <- interview_questions %>%
    ungroup %>%
    group_by(cas_id, group)

group_score <- interview_groups %>%
    summarize(score.group = sum(score, na.rm = TRUE)) %>%
    spread(group, score.group)

group_med <- interview_groups %>%
    summarize(med.group = median(score, na.rm = TRUE)) %>%
    spread(group, med.group)

interview_score <- inner_join(interview_score, group_score, by = "cas_id") %>%
    inner_join(group_med, by = "cas_id")

names(interview_score) <- str_replace_all(names(interview_score), "\\.x", "\\.sum")
names(interview_score) <- str_replace_all(names(interview_score), "\\.y", "\\.med")

compare_questions <- interview_questions %>%
    ungroup %>%
    group_by(cas_id, question) %>%
    summarize(med.score = median(score, na.rm = TRUE)) %>%
    spread(question, med.score) %>%
    select(-characteristics, -mentor) %>%
    mutate(critThinkMed = median(c(critThink, critThinkA, critThinkB, knowledge), na.rm = TRUE),
           critThinkVar = var(c(critThink, critThinkA, critThinkB, knowledge), na.rm = TRUE),
           timeMgmtMed = median(c(timeMgmtA, timeMgmtB), na.rm = TRUE),
           timeMgmtVar = var(c(timeMgmtA, timeMgmtB), na.rm = TRUE)) %>%
    select(cas_id, critThinkMed:timeMgmtVar, contains("critThink"), knowledge, everything())


# join fit and scores
interview_results <- select(result.summary, cas_id:school, interest1:interest3) %>%
    inner_join(interview_remarks_avg, by = "cas_id") %>%
    inner_join(compare_questions, by = "cas_id") %>%
    inner_join(interview_score, by = "cas_id") %>%
    inner_join(interview_remarks, by = "cas_id") %>%
    # inner_join(interview_questions, by = "cas_id") %>%
    arrange(desc(interview.remarks.median, critThinkMed))

<<<<<<< HEAD
write.csv(interview_results, "interview_results.csv", row.names = FALSE)
=======
write.csv(interview_results, "interview_results.csv", row.names = FALSE)
>>>>>>> origin/master
