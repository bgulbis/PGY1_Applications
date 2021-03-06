# get_data.R
# 
# read data from PhORCAS / WebAdMIT API
# 
# reference: http://help.webadmit.org/webadmit2016/documents/WebAdMIT_Export_Manager_API.pdf

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

mykey <- "36e185e0cd26c255b1c36a30b2b4468b"
base.url <- "https://api.webadmit.org"

# get the user_identity_id
userid.url <- paste(base.url, "/api/v1/user_identities", sep = "")
userid <- GET(userid.url, add_headers("x-api-key" = mykey))
userid.json <- content(userid)
userid.json <- fromJSON(toJSON(userid.json))

# select the user_identity_id which corresponds to the desired program
myuserid.2014 <- userid.json$user_identities %>%
    mutate_each(funs(unlist(.))) %>%
    filter(organization == "MEMORIAL HERMANN/TEXAS MED CTR", 
           cycle == "2014 - 2015")

myuserid.2015 <- userid.json$user_identities %>%
    mutate_each(funs(unlist(.))) %>%
    filter(organization == "Memorial Hermann/Texas Medical Center", 
           cycle == "2015 - 2016")

# myuserid <- myuserid.2014$id
myuserid <- myuserid.2015$id

# get all export_id values for the selected user
exportid.url <- paste(base.url, "/api/v1/user_identities/", myuserid, "/exports", sep = "")
exportid <- GET(exportid.url, add_headers("x-api-key" = mykey))
exportid.json <- content(exportid)
exportid.json <- fromJSON(toJSON(exportid.json))

# export.names <- c("API_Applicants", "API_Extraction", "API_References", "API_Application_Scores", "API_Vidyo")

exportids <- exportid.json$exports %>%
    # filter(name %in% export.names)
    filter(str_detect(name, "^API_"))
    
get_data <- function(export.id) {
    # select the export_id for the desired export (saved in Export Manager)
    export.run.url <- paste(base.url, "/api/v1/user_identities/", myuserid, "/exports/", export.id, "/export_files", sep = "")
    
    # trigger the export process to run
    export.runid <- POST(export.run.url, add_headers("x-api-key" = mykey))
    export.runid.json <- content(export.runid)
    
    # find the export_file_id, which is the id for the running instance
    runid <- export.runid.json$export_files$id
    status.url <- paste(base.url, "/api/v1/exports/", export.id, "/export_files/", runid, sep = "")

    # check the status of the running instance every 2 minutes until it's available;
    # when completed will contain the url to download the data
    repeat {
        # get the status of the running instance, 
        statusid <- GET(status.url, add_headers("x-api-key" = mykey))
        statusid.json <- content(statusid)

        if (statusid.json$export_files$status == "Available") {
            # get the url to use for downloading the data; expires after ~30 seconds
            download.url <- statusid.json$export_files$download_url
            
            # download the data
            mydata <- GET(download.url)
            download.data <- content(mydata)
            
            print("Export complete")
            
            # end repeat loop
            break
        } else {
            # time to wait in seconds; adjust based on how much data being pulled
            delay <- 60
            msg <- paste("Export running... Next check at: ", Sys.time() + delay, sep = "")
            print(msg)
            Sys.sleep(delay)
        }
    }
    
    # return the downloaded data
    download.data
    # mydata
}

data <- lapply(exportids$id, get_data)
names(data) <- exportids$name

saveRDS(data, "phorcas_data.Rds")

# other options
# 
# list of all exports
# export.url <- paste(base.url, "/api/v1/exports", sep="")

# check status of all running instances, does not include download url
# batch.url <- paste(base.url, "/api/v1/user_identities/", myuserid.2014$id, "/export_files", sep = "")
# export.batch <- POST(batch.url, add_headers("x-api-key" = mykey))
# batch.json <- content(export.batch)
# batch.json <- fromJSON(toJSON(batch.json))

