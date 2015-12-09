# get_data.R
# 
# read data from PhORCAS / WebAdMIT API
# 

library(httr)

mykey <- "36e185e0cd26c255b1c36a30b2b4468b"
# myapp <- oauth_app("phorcas", key = mykey)

baseurl <- "https://api.webadmit.org"
# base <- handle(baseurl)
url <- "/api/v1/user_identities"

gourl <- paste(baseurl, url, sep="")

userid <- GET(gourl, add_headers("x-api-key" = mykey))
json1 <- content(userid)
json2 <- fromJSON(toJSON(json1))
json2

export.url <- "/api/v1/exports"
gourl <- paste(baseurl, export.url, sep="")

exportid <- GET(gourl, add_headers("x-api-key" = mykey))
json3 <- content(exportid)
json4 <- fromJSON(toJSON(json3))

myuserid <- json2$user_identities[[7, 1]]
export.userid <- paste(baseurl, "/api/v1/user_identities/", myuserid, "/exports", sep = "")

exportid2 <- GET(export.userid, add_headers("x-api-key" = mykey))
json5 <- content(exportid2)
json6 <- fromJSON(toJSON(json5))
json6

myexportid <- json6$exports[[12, 1]]
run_export <- paste(baseurl, "/api/v1/user_identities/", myuserid, "/exports/", myexportid, "/export_files", sep = "")

export_run <- POST(run_export, add_headers("x-api-key" = mykey))
json7 <- content(export_run)

runid <- json7$export_files$id
status_url <- paste(baseurl, "/api/v1/exports/", myexportid, "/export_files/", runid, sep = "")

export_status <- GET(status_url, add_headers("x-api-key" = mykey))
json8 <- content(export_status)
json8

run_batch <- paste(baseurl, "/api/v1/user_identities/", myuserid, "/export_files", sep = "")
export_status <- GET(run_batch, add_headers("x-api-key" = mykey))
json8a <- content(export_status)
json8a

download_url <- json8$export_files$download_url

mydata <- GET(download_url)
json9 <- content(mydata)
