#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
email_user <- args[1]
# ------ Send start email to user ----------------------------------------------
#api_key <- Sys.getenv("MJ_APIKEY_PUBLIC")
#api_secret <- Sys.getenv("MJ_APIKEY_SECRET")
api_key <- "19bfd6c40901eeb0229f4657bab053be"
api_secret <- "79f50ec2e26b3b74374e83ab4fe0ffe5"
sender_email <- "nirsdb@post.com"
print(api_key)
print(api_secret)
print(sender_email)


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
              Email = email_user,
              Name = ""
            )
          ),
          Subject = paste0("[NirsDB] The computation of your NIRS predictions has started."),
          TextPart = paste0(
            'Hello,\n\n',
            'The NirsDB tool has started the computation of your NIRS predictions. ',
            'You will receive an email with the results as soon as the job is over.\n\n',
            'This email is automatic. Please don t answer it.'
          ),
          HTMLPart = paste0(
            'Bonjour,<br><br>',
            'The NirsDB tool has started the computation of your NIRS predictions. ',
            'You will receive an email with the results as soon as the job is over.<br><br>',
            '<i>This email is automatic. Please don t answer it.</i>'
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
)
