# Run in RStudio console: source("diagnose_gmail.R")
library(gmailr)

APP_DIR <- "/Users/irisjeanjacobtizon/Documents/Rstudio Files/DRPAWLUK_FINANCES"
gm_auth_configure(path = file.path(APP_DIR, ".gmail_credentials.json"))
gm_auth(email = "juan@magiteklabs.co", cache = file.path(APP_DIR, ".gmail_token"))

check <- function(query, label) {
  msgs <- tryCatch(gm_messages(search = query, num_results = 3), error = function(e) NULL)
  n <- if (is.null(msgs)) 0 else length(gm_id(msgs))
  message(sprintf("%-40s → %d emails found", label, n))
  if (n > 0) {
    for (id in gm_id(msgs)) {
      m <- gm_message(id, format = "metadata")
      subj <- tryCatch(gm_subject(m), error = function(e) "(no subject)")
      from <- tryCatch(gm_from(m),    error = function(e) "(unknown)")
      message("    FROM: ", from)
      message("    SUBJ: ", subj)
      message("")
    }
  }
}

message("=== GMAIL DIAGNOSTIC ===\n")
check("from:intuit.com",                     "Any email from intuit.com")
check("from:quickbooks",                      "Any email from quickbooks")
check("subject:quickbooks has:attachment",    "QuickBooks + attachment")
check("subject:profit has:attachment",        "Profit + attachment")
check("subject:transaction has:attachment",   "Transaction + attachment")
check("from:stripe.com has:attachment",       "Stripe + attachment")
check("has:attachment filename:xlsx",         "Any xlsx attachment (recent)")
check("has:attachment filename:csv",          "Any csv attachment (recent)")
