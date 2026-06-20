# ============================================================
# fetch_gmail_reports.R — run in RStudio console
# ============================================================
options(rlang_interactive = TRUE)
library(gmailr)

APP_DIR  <- "/Users/irisjeanjacobtizon/Documents/Rstudio Files/DRPAWLUK_FINANCES"
DATA_DIR <- file.path(APP_DIR, "data")
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

# Use cached token from setup_gmail_auth.R
gm_auth_configure(path = file.path(APP_DIR, ".gmail_credentials.json"))
gm_auth(
  email  = "juan@magiteklabs.co",
  scopes = "https://www.googleapis.com/auth/gmail.readonly",
  cache  = file.path(APP_DIR, ".gmail_token")
)

# ---- fetch helper ----
fetch_one <- function(query, saveas, label) {
  message("Searching: ", label)
  msgs <- tryCatch(gm_messages(search = query, num_results = 5), error = function(e) NULL)
  if (is.null(msgs) || length(gm_id(msgs)) == 0) {
    message("  NOT FOUND: ", label); return(FALSE)
  }
  dest <- file.path(DATA_DIR, saveas)
  for (id in gm_id(msgs)) {
    msg   <- gm_message(id, format = "full")
    parts <- gm_attachments(msg)
    if (length(parts) == 0) next
    tryCatch({
      # save to DATA_DIR then rename to expected filename
      gm_save_attachments(msg, path = DATA_DIR)
      all_files <- list.files(DATA_DIR, full.names = TRUE)
      ext <- tolower(tools::file_ext(saveas))
      for (f in all_files) {
        fn <- basename(f)
        if (tolower(tools::file_ext(f)) == ext && fn != saveas)
          file.rename(f, dest)
      }
      if (file.exists(dest)) { message("  SAVED: ", saveas); return(TRUE) }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  NO ATTACHMENT FOUND: ", label); FALSE
}

# ---- fetch all reports ----
message("\n====== Fetching from juan@magiteklabs.co ======\n")

# QuickBooks: fetch all intuit.com emails and route by subject keyword
# (Gmail phrase-search breaks on underscore-separated words)
qb_map <- list(
  list(kw = "Profit",          saveas = "Health Energy Partners LLC_Profit and Loss.xlsx"),
  list(kw = "Vendor Detail",   saveas = "Health Energy Partners LLC_Purchases by Vendor Detail.xlsx"),
  list(kw = "Transaction List",saveas = "Health Energy Partners LLC_Transaction List by Date.xlsx")
)

message("Fetching all QuickBooks attachments from intuit.com...")
qb_msgs <- tryCatch(
  gm_messages(search = "from:intuit.com has:attachment", num_results = 20),
  error = function(e) { message("  Gmail error: ", e$message); NULL }
)
if (!is.null(qb_msgs)) {
  for (id in gm_id(qb_msgs)) {
    msg  <- gm_message(id, format = "full")
    subj <- tryCatch(gm_subject(msg), error = function(e) "")
    parts <- gm_attachments(msg)
    if (length(parts) == 0) next
    for (entry in qb_map) {
      if (!grepl(entry$kw, subj, ignore.case = TRUE)) next
      dest <- file.path(DATA_DIR, entry$saveas)
      if (file.exists(dest)) { message("  Already exists: ", entry$saveas); next }
      tryCatch({
        before <- list.files(DATA_DIR, full.names = TRUE)
        gm_save_attachments(msg, path = DATA_DIR)
        after  <- list.files(DATA_DIR, full.names = TRUE)
        new_files <- setdiff(after, before)
        if (length(new_files) > 0) {
          file.rename(new_files[[1]], dest)
          message("  SAVED: ", entry$saveas)
        } else {
          message("  WARNING: no new file appeared for ", entry$saveas)
        }
      }, error = function(e) message("  ERROR saving ", entry$saveas, ": ", e$message))
    }
  }
}

fetch_one(
  query  = 'from:stripe.com has:attachment filename:csv',
  saveas = "Stripe Payment Report All Transaction.csv",
  label  = "Stripe CSV"
)

# ---- summary ----
message("\n=== Files now in data/ ===")
files <- list.files(DATA_DIR)
if (length(files) == 0) message("  (empty)") else for (f in files) message("  ", f)
