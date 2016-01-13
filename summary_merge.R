# 
# summary_merge.R
# 
# creates PDF file for each applicant

library(knitr)
library(rmarkdown)
# opts_chunk$set(tidy.opts=list(width.cutoff=60))

if (!exists("applicants")) {
    applicants <- readRDS("applicants.Rds")
}

for (i in 1:nrow(applicants)) {
    rmarkdown::render(input = "summary.Rmd", output_format = "pdf_document", 
                      output_file = paste(applicants$last_name[i], "_", applicants$first_name[i], ".pdf", sep = ""),
                      output_dir = "summaries")
}

# test
# i <- 24
# rmarkdown::render(input = "summary.Rmd", output_format = "pdf_document", 
#                   output_file = paste(applicants$last_name[i], "_", applicants$first_name[i], ".pdf", sep = ""),
#                   output_dir = "summaries")

# i <- 24
# rmarkdown::render(input = "summary_test.Rmd", output_format = "word_document", 
#                   output_file = paste(applicants$last_name[i], "_", applicants$first_name[i], ".docx", sep = ""),
#                   output_dir = "summaries")
