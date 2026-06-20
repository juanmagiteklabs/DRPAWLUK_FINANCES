# ============================================================
# STEP 1: Run this ONCE in RStudio to connect Gmail
# It will open a browser — sign in as juan@magiteklabs.co
# ============================================================

library(gmailr)

APP_DIR <- "/Users/irisjeanjacobtizon/Documents/Rstudio Files/DRPAWLUK_FINANCES"

gm_auth_configure(path = file.path(APP_DIR, ".gmail_credentials.json"))

gm_auth(
  email  = "juan@magiteklabs.co",
  scopes = "https://www.googleapis.com/auth/gmail.readonly",
  cache  = file.path(APP_DIR, ".gmail_token")
)

message("Gmail connected! Token saved. Now run fetch_gmail_reports.R")
