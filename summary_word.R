# 
# summary_word.R
# 
# create summary sheet in Microsoft Word format for each applicant
# 

library(ReporteRs)
library(dplyr)

if (!exists("applicants")) {
    applicants <- readRDS("applicants.Rds")
}

# for (i in 1:nrow(applicants)) {

for (i in 1:3) {
    mydoc <- docx()
    
    app.id <- applicants$cas_id[i]
    
    candidate <- applicants[applicants$cas_id == app.id, ]
    app.name <- paste(candidate$last_name, candidate$first_name, sep = ", ")
    
    if (candidate$gpa_collected == TRUE) {
        gpa <- candidate$gpa
    } else {
        gpa <- "No GPA"
    }
    
    grad.date <- as.character(candidate$graduation_date)
    
    if (!exists("interests")) {
        interests <- readRDS("interests.Rds")
    }
    
    app.interest <- as.character(interests[interests$cas_id == app.id, "interest"])
    
    if (!exists("intent")) {
        intent <- readRDS("intent.Rds")
    }
    loi <- intent[intent$cas_id == app.id, ]
    
    if (!exists("cv")) {
        cv <- readRDS("cv.Rds")
    }
    cv.data <- cv[cv$cas_id == app.id, ]
    
    if (!exists("app.scores")) {
        app.scores <- readRDS("app.scores.Rds")
    }
    score <- app.scores[app.scores$cas_id == app.id, ]
    
    letter.score <- score$goals_score + score$contributions_score + score$expectations_score + score$motivation_score
    letter.hdr <- paste(letter.score, " (", round((letter.score / 6) * 100, 0), "%)", sep = "")
    
    resume.score <- score$clin_skills_score + score$lcep_score + score$rotations_score + score$work_experience_score + score$publications_score + score$presentations_score + score$leadership_score
    resume.hdr <- paste(resume.score, " (", round((resume.score / 19) * 100, 0), "%)", sep = "")
    
    reference.score <- score$recs_score + score$known_recommender_score
    reference.hdr <- paste(reference.score, " (", round((reference.score / 11) * 100, 0), "%)", sep = "")
    
    other.hdr <- paste(score$other_score, " (", round((score$other_score / 6) * 100, 0), "%)", sep = "")
    
    if (!exists("lor")) {
        lor <- readRDS("lor.Rds")
    }
    
    recs <- lor[lor$cas_id == app.id, ]
    
    if (!exists("ref.assess")) {
        ref.assess <- readRDS("ref.assess.Rds")
    }
    
    str.wk <- ref.assess[ref.assess$cas_id == app.id, ]
    
    if (!exists("references")) {
        references <- readRDS("references.Rds")
    }
    
    writers <- references[references$cas_id == app.id, ]
    
    writers <- mutate(writers, ref_name = paste(ref_last_name, ref_first_name, sep = ", "))
    writers.hdr <- writers$ref_name
    if (length(writers.hdr) == 4) {
        writers.hdr <- c(writers.hdr, "")
    } else {
        writers.hdr <- c(writers.hdr, "", "")
    }
    
    if (!exists("vidyo")) {
        vidyo <- readRDS("vidyo.Rds")
    }
    video <- vidyo[vidyo$cas_id == app.id, ]
    
    video.hdr <- paste(video$score, " (", round((video$score / 28) * 100, 0), "%)", sep = "")
    
    styles(mydoc)
    mydoc <- declareTitlesStyles(mydoc, stylenames = c("Titre1", "Titre2"))
    mydoc <- addTitle(mydoc, paste(app.name, " - ", candidate$school, sep = ""), level = 1)
    mydoc <- addTitle(mydoc, "Interests: ", level = 2)
    
    application <- c("Letter of Intent", "Curriculum Vitae", "Letters of Reference", "Reviewer Fit", "Reviewer")
    interview <- c("Application Score", "Vidyo Score", "Total Score", "Interviewer Fit", "Interviewer")
    school <- c("GPA", "Grad Date", "School Score", "Known Rec", "")
    
    app.score.hdr <- paste(score$application_score, " (", round((score$application_score / 42) * 100, 0), "%)", sep = "")
    # out of 70
    total.score <- score$application_score + video$score + candidate$school_score
    
    total.hdr <- paste(total.score, " (", round((total.score / 70) * 100, 0), "%)", sep = "")
    
    application.col <- c(letter.hdr, resume.hdr, reference.hdr, score$application_remark, as.character(score$application_reviewer))
    interivew.col <- c(app.score.hdr, video.hdr, total.hdr, video$remarks, as.character(video$interviewer))
    school.col <- c(gpa, grad.date, candidate$school_score, candidate$mh.tmc_rec, "")
    
    score.summary <- data.frame(Application = application, Scores = application.col, Interview = interview, Assessment = interivew.col, School = school, Values = school.col, References = writers.hdr) 
    
    mydoc <- addFlexTable(mydoc, vanilla.table(score.summary))
    
    file.name <- paste("summaries_word/", applicants$last_name[i], "_", applicants$first_name[i], ".docx", sep = "")
    writeDoc(mydoc, file = file.name)
}


