# 
# summary_word.R
# 
# create summary sheet in Microsoft Word format for each applicant
# 

library(ReporteRs)
library(dplyr)
library(stringr)
library(tidyr)

if (!exists("applicants")) {
    applicants <- readRDS("applicants.Rds")
}

for (i in 1:nrow(applicants)) {
    mydoc <- docx(template = "style_template.docx")
    styles(mydoc)
    
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
    
    app.interest <- str_c(interests[interests$cas_id == app.id, "interest"], collapse = ", ")

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
    
    # styles(mydoc)
    mydoc <- declareTitlesStyles(mydoc, stylenames = c("Heading1", "Heading2", "Heading3"))
    mydoc <- addParagraph(mydoc, paste(app.name, " - ", candidate$school, sep = ""), stylename = "Heading1", bookmark = "Start")
    mydoc <- addParagraph(mydoc, paste("Interests: ", app.interest, sep = ""), stylename = "Heading2")
    
    # Summary of Scores
    application <- c("Letter of Intent", "Curriculum Vitae", "Letters of Reference", "Reviewer Fit", "Reviewer")
    interview <- c("Application Score", "Vidyo Score", "Total Score", "Interviewer Fit", "Interviewer")
    school <- c("GPA", "Grad Date", "School Score", "Known Rec", "")
    app.score.hdr <- paste(score$application_score, " (", round((score$application_score / 42) * 100, 0), "%)", sep = "")
    total.score <- score$application_score + video$score + candidate$school_score
    total.hdr <- paste(total.score, " (", round((total.score / 70) * 100, 0), "%)", sep = "")
    application.col <- c(letter.hdr, resume.hdr, reference.hdr, score$application_remark, as.character(score$application_reviewer))
    interivew.col <- c(app.score.hdr, video.hdr, total.hdr, video$remarks, as.character(video$interviewer))
    school.col <- c(gpa, grad.date, candidate$school_score, candidate$mh.tmc_rec, "")
    score.summary <- data.frame(Application = application, Scores = application.col, Interview = interview, Assessment = interivew.col, School = school, Values = school.col, References = writers.hdr) 

    mydoc <- addTitle(mydoc, "Summary of Scores", level = 3)
    mytable <- vanilla.table(score.summary)
    mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
    mytable[3, 3:4] <- textProperties(font.size = 8, font.family = "Calibri", font.weight = "bold")
    mytable[, c(2,4,6)] <- parCenter()
    mydoc <- addFlexTable(mydoc, mytable)
    
    # Letter of Intent
    loi.attrib <- c("Motivation for residency", "Expectating from residency", "Contributions to hospital", "Career goals", "Other statements")
    loi.score <- c(score$motivation_score, score$expectations_score, score$contributions_score, score$goals_score, NA)
    loi.comment <- c(loi$motivation, loi$expectations, loi$contributions, loi$goals, loi$other_letter)
    loi.table <- data.frame(Attribute = loi.attrib, Score = loi.score, Statement = loi.comment)
    
    mydoc <- addTitle(mydoc, "Letter of Intent", level = 3)
    mytable <- vanilla.table(loi.table)
    mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    mytable[, to = "header"] <- parCenter()
    mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
    # mytable <- setFlexTableWidths(mytable, widths = c(1.5, 0.75, 5.25))
    mytable[, 2] <- parCenter()
    mytable[, 3] <- parLeft()
    mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
    mydoc <- addFlexTable(mydoc, mytable)

    # Curriculum Vitae
    cv.attrib <- c("Leadership", "Posters / Platform Presentations", "Research / Publications", "Work Experience", "Rotations / Acute Care / Academic", "Longitudinal APPE Program", "Clinical Skills Winner")
    cv.num <- c(cv.data$leadership_positions, paste(cv.data$poster_presentations, cv.data$platform_presentations, sep = " / "), paste(cv.data$research_projects, cv.data$publications, sep = " / "), "", paste(cv.data$num_rotations, cv.data$acute_care_rotations, cv.data$academic_rotations, sep = " / "), "", "")
    cv.score <- c(score$leadership_score, score$presentations_score, score$publications_score, score$work_experience_score, score$rotations_score, score$lcep_score, score$clin_skills_score)
    cv.comment <- c(score$leadership_comments, score$presentations_comments, score$publications_comments, score$work_experience_comments, score$rotations_comments, score$lcep_comments, score$clin_skills_comments)
    cv.table <- data.frame(Attribute = cv.attrib, Numbers = cv.num, Score = cv.score, Comments = cv.comment)
    
    mydoc <- addTitle(mydoc, "Curriculum Vitae", level = 3)
    mytable <- vanilla.table(cv.table)
    mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    mytable[, to = "header"] <- parCenter()
    mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
    mytable[, 2:3] <- parCenter()
    mytable[, 4] <- parLeft()
    mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
    mydoc <- addFlexTable(mydoc, mytable)
    
    # Letters of Recommendation - Ratings
    recs.table <- recs %>%
        select(-cas_id, -ref_num) %>%
        filter((quality %in% c("Problem Solving", "Time Management", "Maturity", "Independence") | rating == "Fails to Meet" | str_detect(comment, regex("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improv(e|ement|ing)?", ignore_case = TRUE)) == TRUE | str_detect(comment, regex("(struggle|difficult|deficien)", ignore_case = TRUE)) == TRUE) & comment != "")
    
    names(recs.table) <- c("Quality", "Rating", "Comment")
    
    mydoc <- addTitle(mydoc, "Letters of Recommendation", level = 3)
    if (nrow(recs.table) == 0) {
        mydoc <- addParagraph(mydoc, "No recommendation data meeting criteria", stylename = "Normal")
    } else {
        need.improve <- str_detect(recs.table$Comment, regex("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improv(e|ement|ing)?", ignore_case = TRUE))
        struggle <- str_detect(recs.table$Comment, regex("(struggle|difficult|deficien)", ignore_case = TRUE))
        bold.rows <- c(which(recs.table$Rating == "Fails to Meet"), which(need.improve == TRUE), which(struggle == TRUE))
        
        mytable <- vanilla.table(recs.table)
        mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
        mytable[, to = "header"] <- parCenter()
        mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
        mytable[, 1:2] <- parCenter()
        mytable[, 3] <- parLeft()
        # mytable <- setRowsColors(mytable, i = bold.rows, colors = "#666633")
        mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
        mydoc <- addFlexTable(mydoc, mytable)
    }
    
    # Strengths and Weaknesses
    mydoc <- addTitle(mydoc, "Strengths and Weaknesses", level = 3)
    
    strwk.table <- str.wk %>%
        spread(quality, comment) %>%
        select(Strengths, Weaknesses)

    mytable <- vanilla.table(strwk.table)
    mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    mytable[, to = "header"] <- parCenter()
    mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
    mytable[] <- parLeft()
    mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
    mydoc <- addFlexTable(mydoc, mytable)
    
    # Vidyo Interviews
    mydoc <- addTitle(mydoc, "Vidyo Interviews", level = 3)

        vidyo.attrib <- c("Critical Thinking", "Time Management", "Problem Solving", "Integrity")
    vidyo.score <- c(video$crit_think_score, video$time_mgmt_score, video$prob_solve_score, video$integrity_score)
    vidyo.comment <- c(video$crit_think_comments, video$time_mgmt_comments, video$prob_solve_comments, video$integrity_comments)
    vidyo.table <- data.frame(Attribute = vidyo.attrib, Score = vidyo.score, Comment = vidyo.comment)
    
    mytable <- vanilla.table(vidyo.table)
    mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    mytable[, to = "header"] <- parCenter()
    mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
    mytable[, 1:2] <- parCenter()
    mytable[, 3] <- parLeft()
    mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
    mydoc <- addFlexTable(mydoc, mytable)
    
    mydoc <- addTitle(mydoc, "Overall Comments", level = 3)
    mydoc <- addParagraph(mydoc, paste("Application Comments: ", score$application_comments, sep = ""), stylename = "Normal")
    mydoc <- addParagraph(mydoc, paste("Vidyo Comments: ", video$comments, sep = ""), stylename = "Normal")
    
    file.name <- paste("summaries_word/", applicants$last_name[i], "_", applicants$first_name[i], ".docx", sep = "")
    writeDoc(mydoc, file = file.name)
}


