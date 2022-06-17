#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
email_user <- args[1]
# ------ Send start email to user ----------------------------------------------
#api_key <- Sys.getenv("MJ_APIKEY_PUBLIC")
#api_secret <- Sys.getenv("MJ_APIKEY_SECRET")
#sender_email <- Sys.getenv("SENDER_EMAIL")
api_key <- "19bfd6c40901eeb0229f4657bab053be"
api_secret <- "79f50ec2e26b3b74374e83ab4fe0ffe5"
sender_email <- "axel.vaillant@cefe.cnrs.fr"
print(api_key)
print(api_secret)
print(sender_email)



###### Get all ouput files ######
# Write zip file

files <- list.files(file.path("Results"), full.names = TRUE)
tryCatch(zip(
  zipfile = file.path("runResults"), 
  flags = "-r9X", 
  files = files
))

report_base64 <- base64enc::base64encode(
  file.path("runResults.zip")
)

##########
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
            Name = "Application NirsDB"
          ),
          To = list(
            list(
              Email = email_user,
              Name = ""
            )
          ),
          Subject = paste0("[NirsDB] Le calcul de vos prédictions est terminé"),
          TextPart = paste0(
            'Bonjour,\n\n',
            'L’outil NirsDB a terminé le calcul de prédictions lié à votre spectre. ',
            'Veuillez trouver ci-joint les fichiers contenant vos prédictions et leurs représentations graphiques.\n\n',
            'Cet email est automatisé. Merci de ne pas y répondre.'
          ),
          HTMLPart = paste0(
            'Bonjour,<br><br>',
            'L’outil NirsDB a terminé le calcul de prédictions lié à votre spectre.  ',
            'Veuillez trouver ci-joint les fichiers contenant vos prédictions et leurs représentations graphiques.<br><br>',
            '<i>Cet email est automatisé. Merci de ne pas y répondre.</i>'
          ),
          Attachments = list(
            list(
              ContentType = "application/zip",
              Filename = "runResults.zip",
              Base64Content = report_base64
            )
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
)
system(paste("rm runResults.zip"),wait = FALSE)
