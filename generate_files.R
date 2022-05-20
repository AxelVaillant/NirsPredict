#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

source("global.R")




# ------ Send the report by email ----------------------------------------------

report_base64 <- base64enc::base64encode(
  file.path("sim_files", random_string, "resultatsSimulationsScenarios.pdf")
)

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
          Subject = paste0("[NirsDB] Les résultats de vos prédictions sont prêts."),
          TextPart = paste0(
            'Bonjour,\n\n',
            'Le modèle Lynx-Collision-Habitat a terminé de calculer ',
            'le scénario que vous avez soumis à l’outil ERC-Lynx.\n\n',
            'Vous trouverez le rapport de résultats en pièce jointe de cet ',
            'email.\n\n',
            'Vous pouvez télécharger les fichiers de travail à l’adresse ',
            'suivante : https://www.erc-lynx.fr/sim_files/',
            random_string, '/. Les données seront conservées pendant ',
            '30 jours.\n\n',
            'Cet email est automatisé. Merci de ne pas y répondre.'
          ),
          HTMLPart = paste0(
            'Bonjour,<br><br>',
            'Le modèle Lynx-Collision-Habitat a terminé de calculer ',
            'le scénario que vous avez soumis à l’outil ERC-Lynx.<br><br>',
            'Vous trouverez le rapport de résultats en pièce jointe de cet ',
            'email.<br><br>',
            'Vous pouvez télécharger les fichiers de travail à l’adresse ',
            'suivante : <a href="https://www.erc-lynx.fr/sim_files/',
            random_string, '/">https://www.erc-lynx.fr/sim_files/',
            random_string, '/</a>. Les données seront conservées pendant ',
            '30 jours.<br><br>',
            '<em>Cet email est automatisé. Merci de ne pas y répondre.</em>'
          ),
          Attachments = list(
            list(
              ContentType = "application/pdf",
              Filename = "resultatsSimulationsScenarios.pdf",
              Base64Content = report_base64
            )
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
)