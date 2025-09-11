#### Send Emails ####

#' @name send_email
#'
#' @title Send an Email
#'
#' @description It's common in the Crosbie lab to need to send an email as a part of an automated pipeline. R does not provide great functionality for doing so.
#' This function uses the reticulate package to utilize Python and the smtplib Python package to send emails. \cr \cr
#' YOU MUST BE LOGGED INTO THE VPN TO USE
#'
#' @param host The host of the server
#' @param port The port for email sending
#' @param from Email address of person sending email
#' @param to The email address or email addresses of people receiving email
#' @param subject Subject header
#' @param body The body of the email, which will be in HTML
#' @param attachments paths to one or multiple files to be included
#'
#' @import reticulate
#'
#' @return Success of whether email is sent
#'
#' @export

send_email <- function(host = 'smtp.sickkids.ca', port = 25,
                       from = NULL, to = NULL, subject = NULL, body = NULL,
                       attachments = NULL) {

  smtplib_lib <- reticulate::import("smtplib")
  email_lib <- reticulate::import("email")
  io_lib <- reticulate::import("io")
  `%as%` <- reticulate::`%as%`

  # Build email
  email = email_lib$mime$multipart$MIMEMultipart()

  to_list <- reticulate::r_to_py(as.list(to))

  # Set email parts
  if (length(to) >= 2) {
    email["To"] <- paste(to, collapse = ", ")
  } else {
    email["To"] = to
  }
  email["From"] = from
  email["Subject"] = subject

  # Body
  body_format = email_lib$mime$text$MIMEText(body, "html")
  email_lib$message$Message$attach(email, body_format)

  # Attachments
  if (!is.null(attachments)) {

    for (file in attachments) {
      with(io_lib$open(file, "rb") %as% attachment, {
        part = email_lib$mime$multipart$MIMEBase("application", "octet-stream")
        email_lib$message$Message$set_payload(
          part,
          io_lib$BufferedReader$read(attachment)
        )
      })

      email_lib$encoders$encode_base64(part)
      email_lib$message$Message$add_header(
        part,
        "Content-disposition",
        sprintf("attachment; filename= %s", basename(file))
      )
      email_lib$message$Message$attach(email, part)
    }
  }

  # Try to connect to server. Throw error if not connected to VPN
  tryCatch({
    server = smtplib_lib$SMTP(host, as.integer(port))
  }, error = function(e){
    response = 'failed'
  }

  )
  email = email_lib$message$Message$as_string(email)

  # Try to send the email. Catch the result.
  result = tryCatch({
    smtplib_lib$SMTP$sendmail(
      server,
      from,
      to_list,
      email
    )

    response <- 'Successfully sent email'
  }, error = function(e){
    # error handler picks up where error was generated
    response <- paste("Error sending email:  ",e,"\n\nBe sure that you are logged into the VPN. ")
    print(cli::cli_alert_danger(response))
  }, finally = function(f){
    return(response)
  }

  )

}

#### Email Body Builder ####

#' @name body_builder
#'
#' @title Build the body of an HTML email
#'
#' @description It's common in the Crosbie lab to need to send an email as a part of an automated pipeline. This function reduces the time to create and HTML body.
#'
#' @details
#' The builder automatically adds the formatting HTML like <tr>, etc. Use the sections param to create the sections in the email. This assumes everything is left aligned.
#' For more complicated bodies, you'll need to build it yourself or update this function
#'
#' @param title metadata title of the email
#' @param sections Pass the sections of the email into a vector. If you include 5 elements, then 5 sections will be created with a line between each. If you'd like HTML
#' formatting in the sections, i.e. bolding text, the character text needs to be HTML formatted
#'
#' @export


body_builder <- function(title = NULL, sections = NULL) {

  header <- paste0('<!DOCTYPE html>
                  <html lang="en">
                  <head>
                  <meta charset="utf-8">
                  <title><!--',title,'--></title>
                  </head>')

  # Build body of email
  body <- character()

  for(i in 1:length(sections)){

    section <- paste0('<tr><td align="left"><tr><p>',sections[i],'</p></tr></td><hr />')

    body <- paste0(body,section)
  }

  # Full email
  full <- paste0(header, body)

  return(full)

}
