#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
email_user <- args[1]
session <- args[2]
# ------ Send start email to user ----------------------------------------------
#api_key <- Sys.getenv("MJ_APIKEY_PUBLIC")
#api_secret <- Sys.getenv("MJ_APIKEY_SECRET")
#-------Get credentials----------------------#
credentials<-read.table(file = "csv/credentials.csv",header = TRUE,sep = ";")
api_key<-credentials$api_key
api_secret<-credentials$api_secret
sender_email <- "nirsdb@post.com"

###### Get all contribution files ######
# Write zip file

files <- list.files(file.path(paste("contribution/",session,sep = "")), full.names = TRUE)
tryCatch(zip(
  zipfile = file.path(paste("contribution/",session,sep = "")), 
  flags = "-r9X", 
  files = files
))

report_base64 <- base64enc::base64encode(
  file.path(paste("contribution/",session,".zip",sep = ""))
)

send_email <- httr::POST(
  url = "https://api.mailjet.com/v3.1/send",
  httr::authenticate(
    api_key,
    api_secret
  ),
  httr::add_headers(
    `content-type` = "application/json"
  ),
  body = jsonlite::toJSON(
    list(
      Messages = list(
        list(
          From = list(
            Email = sender_email,
            Name = "NirsDB Application"
          ),
          To = list(
            list(
              Email = 'axel.vaillant@cefe.cnrs.fr',
              Name = ""
            )
          ),
          Subject = paste0("[NirsDB] Une contribution à été soumise."),
          TextPart = paste0(
            'Hello,\n\n',
            'A contribution has been submitted to the NirsDB tool.',
            'The contribution come from the user ',email_user,'.\n\n',
            'This email is automatic. Please don t answer it.'
          ),
          HTMLPart = paste0(
            'Hello,<br><br>',
            'A contribution has been submitted to the NirsDB tool.<br><br>',
            'The contribution come from the user ',email_user,'.\n\n',
            '<i>This email is automatic. Please don t answer it.</i>'
          ),
          Attachments = list(
            list(
              ContentType = "application/zip",
              Filename = "contribution.zip",
              Base64Content = report_base64
            )
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
)
system(paste("rm -Rf contribution/",session,sep = ""))
system(paste("rm -Rf contribution/",session,".zip",sep = ""))