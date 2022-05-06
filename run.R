args = commandArgs(trailingOnly=TRUE)
email_user <- args[1]
# ------ Send start email to user ----------------------------------------------
api_key <- Sys.getenv("MJ_APIKEY_PUBLIC")
api_secret <- Sys.getenv("MJ_APIKEY_SECRET")
sender_email <- Sys.getenv("SENDER_EMAIL")

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
          Subject = paste0("[ERC Lynx] Le calcul de votre scénario a démarré."),
          TextPart = paste0(
            'Bonjour,\n\n',
            'L’outil ERC Lynx a démarré le calcul de votre scénario. ',
            'Vous recevrez un email sous 24-48 heures avec les résultats.\n\n',
            'Cet email est automatisé. Merci de ne pas y répondre.'
          ),
          HTMLPart = paste0(
            'Bonjour,<br><br>',
            'L’outil ERC Lynx a démarré le calcul de votre scénario. ',
            'Vous recevrez un email sous 24-48 heures avec les résultats.<br><br>',
            '<i>Cet email est automatisé. Merci de ne pas y répondre.</i>'
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
)