suppressPackageStartupMessages({
  library(bs4Dash)
  library(shiny)
  library(shinyjs)
  library(shinycssloaders)
  library(data.table)
  library(vroom)
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(scales)
  library(htmltools)
})

# ============================================================
# FILE PATHS  (relative — works both locally and on shinyapps.io)
# ============================================================
DATA_DIR <- "data"

files <- list(
  braintree = file.path(DATA_DIR, "Braintree transaction_search.csv"),
  hep_2026  = file.path(DATA_DIR, "HEP PAYPAL Jan 1, 2026 - Jun 1, 2026.CSV"),
  htw_2026  = file.path(DATA_DIR, "High Tech Wellness Paypal Jan 1, 2026 - Jun 1, 2026.CSV"),
  stripe    = file.path(DATA_DIR, "Stripe Payment Report All Transaction.csv"),
  pl        = file.path(DATA_DIR, "Health Energy Partners LLC_Profit and Loss.xlsx"),
  vendor    = file.path(DATA_DIR, "Health Energy Partners LLC_Purchases by Vendor Detail.xlsx"),
  txlist    = file.path(DATA_DIR, "Health Energy Partners LLC_Transaction List by Date.xlsx")
)

missing_files <- character(0)

# ============================================================
# COLORS & HELPERS
# ============================================================
COLORS <- list(
  green  = "#00A878",
  red    = "#E03131",
  yellow = "#E67700",
  blue   = "#1971C2",
  bg     = "#FFFFFF",
  sidebar = "#1C2331",
  card   = "#FFFFFF",
  grid   = "#E9ECEF",
  text   = "#1A1A2E",
  text2  = "#6C757D"
)

PLOTLY_LAYOUT <- list(
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  font          = list(color = "#1A1A2E", family = "'IBM Plex Mono', monospace"),
  xaxis = list(
    gridcolor     = "#E9ECEF",
    zerolinecolor = "#CED4DA",
    tickfont      = list(color = "#6C757D")
  ),
  yaxis = list(
    gridcolor     = "#E9ECEF",
    zerolinecolor = "#CED4DA",
    tickfont      = list(color = "#6C757D")
  ),
  legend = list(font = list(color = "#1A1A2E"), bgcolor = "rgba(0,0,0,0)"),
  hoverlabel = list(
    bgcolor     = "#FFFFFF",
    bordercolor = "#CED4DA",
    font        = list(color = "#1A1A2E")
  ),
  margin = list(t = 40, r = 20, b = 40, l = 60)
)

clean_amount <- function(x) {
  as.numeric(gsub("[,$\\s]", "", as.character(x)))
}

fmt_currency <- function(x) {
  ifelse(is.na(x), "N/A", scales::dollar(as.numeric(x), accuracy = 1))
}

fmt_number <- function(x) {
  ifelse(is.na(x), "N/A", format(round(as.numeric(x)), big.mark = ","))
}

SUCCESSFUL_STATUSES <- c("settled", "completed", "paid")

# ============================================================
# LOAD BRAINTREE
# Strategy: always prefer the RDS cache (bundled with app).
# CSV fallback only runs locally when cache is missing/stale.
# On shinyapps.io only the RDS is present — CSV is too large to deploy.
# ============================================================
bt_data          <- NULL
bt_failure_stats <- data.table(month = as.Date(character()), status = character(), N = integer())
BT_RDS <- file.path(DATA_DIR, "braintree_cache.rds")

if (file.exists(BT_RDS)) {
  message("Loading Braintree from RDS cache...")
  cached <- tryCatch(readRDS(BT_RDS),
                     error = function(e) { message("RDS read error: ", e$message); NULL })
  if (!is.null(cached)) {
    bt_data          <- cached$bt_data
    bt_failure_stats <- cached$bt_failure_stats
    message("Braintree (cache): ", nrow(bt_data), " rows + failure stats")
  }
} else if (file.exists(files$braintree)) {
  message("No RDS cache found — reading Braintree CSV (large file)...")
  bt_cols <- c("Transaction ID","Transaction Type","Transaction Status",
               "Created Datetime","Amount Submitted For Settlement",
               "Customer Email","Customer First Name","Customer Last Name",
               "Order ID","Currency ISO Code")
  bt_raw <- tryCatch(
    fread(files$braintree, select = bt_cols, showProgress = FALSE, encoding = "UTF-8"),
    error = function(e) { message("Braintree CSV error: ", e$message); NULL }
  )
  if (!is.null(bt_raw)) {
    bt_raw[, parsed_date  := as.Date(substr(`Created Datetime`,1,10), format="%m/%d/%Y")]
    bt_raw[, parsed_month := floor_date(parsed_date, "month")]
    bt_failure_stats <- bt_raw[
      tolower(`Transaction Status`) %in%
        c("gateway_rejected","processor_declined","failed","voided","settlement_declined"),
      .N, by=.(month=parsed_month, status=tolower(`Transaction Status`))][order(month)]
    bt_settled <- bt_raw[
      !is.na(`Amount Submitted For Settlement`) &
      `Amount Submitted For Settlement` != "" &
      tolower(`Transaction Status`) == "settled"]
    bt_data <- bt_settled[, .(
      transaction_id = `Transaction ID`,
      date           = parsed_date,
      amount         = as.numeric(`Amount Submitted For Settlement`),
      fee            = NA_real_,
      net_amount     = as.numeric(`Amount Submitted For Settlement`),
      status         = tolower(`Transaction Status`),
      type           = `Transaction Type`,
      source         = "Braintree",
      description    = as.character(`Order ID`),
      currency       = `Currency ISO Code`,
      customer_name  = trimws(paste(`Customer First Name`, `Customer Last Name`)),
      customer_email = `Customer Email`,
      is_refund      = (`Transaction Type` == "credit"),
      balance_impact = fifelse(`Transaction Type` == "credit", "debit", "credit")
    )]
    tryCatch(saveRDS(list(bt_data=bt_data, bt_failure_stats=bt_failure_stats), BT_RDS),
             error = function(e) message("RDS save warning: ", e$message))
    message("Braintree CSV → RDS: ", nrow(bt_data), " settled rows")
  }
} else {
  missing_files <- c(missing_files, "Braintree")
  message("WARNING: Braintree RDS cache and CSV both missing")
}

# ============================================================
# LOAD PAYPAL
# ============================================================
load_paypal <- function(path, source_name) {
  if (!file.exists(path)) {
    missing_files <<- c(missing_files, paste(source_name, basename(path)))
    message("WARNING: Missing ", basename(path))
    return(NULL)
  }
  message("Loading ", source_name, ": ", basename(path))
  raw <- tryCatch({
    df <- vroom::vroom(path, show_col_types = FALSE, progress = FALSE,
                       locale = vroom::locale(encoding = "UTF-8"))
    as.data.table(df)
  }, error = function(e) {
    tryCatch({
      df <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
      as.data.table(df)
    }, error = function(e2) {
      message("PayPal load error for ", basename(path), ": ", e2$message)
      NULL
    })
  })
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Strip BOM and quotes from column names, then trim whitespace
  setnames(raw, names(raw),
           trimws(gsub('^\xef\xbb\xbf|^"+|"+$', "", names(raw))))

  get_col <- function(dt, col, default = NA_character_) {
    if (col %in% names(dt)) dt[[col]] else rep(default, nrow(dt))
  }

  result <- data.table(
    transaction_id = get_col(raw, "Transaction ID"),
    date           = as.Date(get_col(raw, "Date"), format = "%m/%d/%Y"),
    amount         = clean_amount(get_col(raw, "Gross", "0")),
    fee            = clean_amount(get_col(raw, "Fee", "0")),
    net_amount     = clean_amount(get_col(raw, "Net", "0")),
    status         = tolower(get_col(raw, "Status", "unknown")),
    type           = get_col(raw, "Type"),
    source         = source_name,
    description    = get_col(raw, "Item Title"),
    currency       = get_col(raw, "Currency", "USD"),
    customer_name  = get_col(raw, "Name"),
    customer_email = get_col(raw, "From Email Address"),
    is_refund      = grepl("refund", get_col(raw, "Type", ""), ignore.case = TRUE),
    balance_impact = tolower(get_col(raw, "Balance Impact", "credit"))
  )
  message(source_name, " (", basename(path), "): ", nrow(result), " rows")
  result
}

pp_hep_2026 <- load_paypal(files$hep_2026, "PayPal HEP")
pp_htw_2026 <- load_paypal(files$htw_2026, "PayPal HTW")

# ============================================================
# LOAD STRIPE
# ============================================================
stripe_data <- NULL
if (file.exists(files$stripe)) {
  message("Loading Stripe...")
  stripe_raw <- tryCatch(
    fread(files$stripe, showProgress = FALSE, encoding = "UTF-8"),
    error = function(e) { message("Stripe error: ", e$message); NULL }
  )
  if (!is.null(stripe_raw)) {
    refunded <- suppressWarnings(as.numeric(stripe_raw[["Amount Refunded"]]))
    stripe_data <- data.table(
      transaction_id = stripe_raw[["id"]],
      date           = as.Date(substr(stripe_raw[["Created date (UTC)"]], 1, 10)),
      amount         = as.numeric(stripe_raw[["Amount"]]),
      fee            = suppressWarnings(as.numeric(stripe_raw[["Fee"]])),
      net_amount     = as.numeric(stripe_raw[["Amount"]]) - ifelse(is.na(refunded), 0, refunded) -
                         suppressWarnings(as.numeric(stripe_raw[["Fee"]])),
      status         = tolower(stripe_raw[["Status"]]),
      type           = fifelse(!is.na(refunded) & refunded > 0, "refund", "payment"),
      source         = "Stripe",
      description    = stripe_raw[["Description"]],
      currency       = toupper(stripe_raw[["Currency"]]),
      customer_name  = stripe_raw[["Customer Description"]],
      customer_email = stripe_raw[["Customer Email"]],
      is_refund      = !is.na(refunded) & refunded > 0,
      balance_impact = "credit"
    )
    message("Stripe: ", nrow(stripe_data), " rows")
  }
} else {
  missing_files <- c(missing_files, "Stripe")
  message("WARNING: Stripe file missing")
}

# ============================================================
# COMBINE ALL TRANSACTIONS
# ============================================================
all_txn_list <- Filter(Negate(is.null),
  list(bt_data, pp_hep_2026, pp_htw_2026, stripe_data))

if (length(all_txn_list) > 0) {
  all_transactions <- rbindlist(all_txn_list, use.names = TRUE, fill = TRUE)
  all_transactions <- all_transactions[
    !is.na(date) & date >= as.Date("2026-01-01") & date <= as.Date("2026-12-31")
  ]
  all_transactions[, `:=`(
    month   = floor_date(date, "month"),
    yearmon = format(date, "%Y-%m"),
    yr      = year(date)
  )]
  setkey(all_transactions, date)
  message("Combined: ", nrow(all_transactions), " transactions")
} else {
  all_transactions <- data.table(
    transaction_id = character(), date = as.Date(character()),
    amount = numeric(), fee = numeric(), net_amount = numeric(),
    status = character(), type = character(), source = character(),
    description = character(), currency = character(),
    customer_name = character(), customer_email = character(),
    is_refund = logical(), balance_impact = character(),
    month = as.Date(character()), yearmon = character(), yr = integer()
  )
}

SOURCES_ALL <- c("Braintree", "PayPal HEP", "PayPal HTW", "Stripe")
SOURCES_AVAILABLE <- intersect(SOURCES_ALL, unique(all_transactions$source))

# ============================================================
# REVENUE SUBSET  (USD, completed/settled/paid, customer-facing)
# ============================================================
revenue_transactions <- all_transactions[
  toupper(currency) %in% c("USD", "") &
  status %in% SUCCESSFUL_STATUSES &
  !grepl("conversion|currency conversion", type, ignore.case = TRUE) &
  (balance_impact %in% c("credit", NA_character_) | is.na(balance_impact))
]

# ============================================================
# QUICKBOOKS — PROFIT & LOSS
# ============================================================
pl_table <- NULL
pl_metrics <- list(
  gross_revenue  = NA_real_, cogs = NA_real_,
  gross_profit   = NA_real_, total_expenses = NA_real_, net_income = NA_real_
)

if (file.exists(files$pl)) {
  pl_raw <- tryCatch(
    read_excel(files$pl, col_names = FALSE),
    error = function(e) { message("P&L error: ", e$message); NULL }
  )
  if (!is.null(pl_raw)) {
    pl_table <- data.frame(
      label = as.character(pl_raw[[1]]),
      value = suppressWarnings(as.numeric(pl_raw[[2]])),
      stringsAsFactors = FALSE
    ) |> dplyr::filter(!is.na(label) & nchar(trimws(label)) > 0)

    find_val <- function(pat) {
      idx <- grep(pat, pl_table$label, ignore.case = TRUE, perl = TRUE)
      if (!length(idx)) return(NA_real_)
      pl_table$value[idx[1]]
    }
    pl_metrics <- list(
      gross_revenue  = find_val("^Total for Income"),
      cogs           = find_val("^Total for Cost of Goods Sold"),
      gross_profit   = find_val("^Gross Profit$"),
      total_expenses = find_val("^Total for Expenses"),
      net_income     = find_val("^Net Income$|^Net Income\\b")
    )
  }
} else {
  missing_files <- c(missing_files, "P&L xlsx")
}

# ============================================================
# QUICKBOOKS — PURCHASES BY VENDOR
# ============================================================
vendor_data <- NULL
if (file.exists(files$vendor)) {
  vendor_raw <- tryCatch(
    read_excel(files$vendor, col_names = FALSE),
    error = function(e) { message("Vendor error: ", e$message); NULL }
  )
  if (!is.null(vendor_raw)) {
    is_total <- !is.na(vendor_raw[[1]]) & grepl("^Total for ", vendor_raw[[1]])
    vendor_data <- data.frame(
      vendor = gsub("^Total for ", "", vendor_raw[[1]][is_total]),
      amount = suppressWarnings(as.numeric(vendor_raw[[10]][is_total])),
      stringsAsFactors = FALSE
    ) |>
      dplyr::filter(!is.na(amount) & amount > 0) |>
      dplyr::arrange(desc(amount))
  }
} else {
  missing_files <- c(missing_files, "Purchases by Vendor xlsx")
}

# ============================================================
# QUICKBOOKS — TRANSACTION LIST (monthly expense breakdown)
# ============================================================
txlist_monthly <- NULL
if (file.exists(files$txlist)) {
  txlist_raw <- tryCatch(
    read_excel(files$txlist, skip = 3, col_names = TRUE),
    error = function(e) { message("TxList error: ", e$message); NULL }
  )
  if (!is.null(txlist_raw) && ncol(txlist_raw) >= 9) {
    cn <- c("date","trans_type","num","posting","name","memo","account","split","amount")
    names(txlist_raw)[seq_along(cn)] <- cn

    txlist_raw <- txlist_raw |>
      dplyr::filter(!is.na(amount)) |>
      dplyr::mutate(
        date       = as.Date(as.character(date), format = "%m/%d/%Y"),
        amount     = suppressWarnings(as.numeric(amount)),
        trans_type = as.character(trans_type)
      ) |>
      dplyr::filter(!is.na(date) & !is.na(amount) &
                    date >= as.Date("2026-01-01") & date <= as.Date("2026-12-31"))

    txlist_raw$month <- floor_date(txlist_raw$date, "month")

    expense_types <- c("Expense", "Bill", "Bill Payment", "Bill Payment (Check)",
                       "Check", "Credit Card Charge", "Transfer")
    revenue_types <- c("Sales Receipt", "Invoice", "Payment", "Deposit")

    txlist_monthly <- txlist_raw |>
      dplyr::group_by(month) |>
      dplyr::summarise(
        qb_revenue  = sum(amount[trans_type %in% revenue_types & amount > 0], na.rm = TRUE),
        qb_expenses = abs(sum(amount[trans_type %in% expense_types & amount < 0], na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(qb_net = qb_revenue - qb_expenses) |>
      dplyr::arrange(month)
  }
} else {
  missing_files <- c(missing_files, "Transaction List xlsx")
}

# ============================================================
# MONTHLY REVENUE SUMMARY
# ============================================================
if (nrow(revenue_transactions) > 0) {
  monthly_revenue <- revenue_transactions[, .(
    gross = sum(fifelse(!is_refund & amount > 0, amount, 0), na.rm = TRUE),
    refunds  = sum(fifelse(is_refund, abs(amount), 0), na.rm = TRUE),
    net      = sum(fifelse(!is_refund, pmax(amount, 0), -abs(amount)), na.rm = TRUE),
    tx_count = .N
  ), by = .(month, source)][order(month)]
} else {
  monthly_revenue <- data.table(
    month = as.Date(character()), source = character(),
    gross = numeric(), refunds = numeric(), net = numeric(), tx_count = integer()
  )
}

# ============================================================
# MONTHLY P&L TABLE (from QB transaction list + payment processors)
# ============================================================
# Build from payment processor revenue + QB expenses
pp_monthly_rev <- if (nrow(revenue_transactions) > 0) {
  revenue_transactions[is_refund == FALSE & amount > 0, .(
    pp_gross = sum(amount, na.rm = TRUE),
    pp_fees  = sum(ifelse(is.na(fee), 0, fee), na.rm = TRUE)
  ), by = month]
} else {
  data.table(month = as.Date(character()), pp_gross = numeric(), pp_fees = numeric())
}

pp_monthly_ref <- if (nrow(revenue_transactions) > 0) {
  revenue_transactions[is_refund == TRUE, .(pp_refunds = sum(abs(amount), na.rm = TRUE)), by = month]
} else {
  data.table(month = as.Date(character()), pp_refunds = numeric())
}

if (!is.null(txlist_monthly)) {
  monthly_pl <- merge(
    as.data.frame(pp_monthly_rev), as.data.frame(txlist_monthly),
    by = "month", all = TRUE
  ) |>
  merge(as.data.frame(pp_monthly_ref), by = "month", all = TRUE) |>
  dplyr::mutate(
    pp_gross    = ifelse(is.na(pp_gross), 0, pp_gross),
    pp_refunds  = ifelse(is.na(pp_refunds), 0, pp_refunds),
    pp_fees     = ifelse(is.na(pp_fees), 0, pp_fees),
    qb_expenses = ifelse(is.na(qb_expenses), 0, qb_expenses),
    gross_revenue = pp_gross - pp_refunds,
    net_revenue   = gross_revenue - pp_fees,
    total_expenses = qb_expenses,
    net_income     = gross_revenue - qb_expenses
  ) |>
  dplyr::arrange(month) |>
  dplyr::select(month, gross_revenue, pp_refunds, net_revenue, total_expenses, net_income)
} else {
  monthly_pl <- as.data.frame(pp_monthly_rev) |>
    dplyr::left_join(as.data.frame(pp_monthly_ref), by = "month") |>
    dplyr::mutate(
      pp_refunds    = ifelse(is.na(pp_refunds), 0, pp_refunds),
      gross_revenue = pp_gross - pp_refunds,
      net_revenue   = gross_revenue - pp_fees,
      total_expenses = NA_real_,
      net_income     = NA_real_
    ) |>
    dplyr::arrange(month) |>
    dplyr::select(month, gross_revenue, pp_refunds, net_revenue, total_expenses, net_income)
}

# ============================================================
# COGS & LOGISTICS BREAKDOWNS
# ============================================================
cogs_breakdown    <- NULL
logistics_vendors <- NULL
processing_fees   <- NULL
cogs_by_supplier  <- NULL
cogs_monthly      <- NULL
cogs_txn_detail   <- NULL

# ---- P&L-based breakdowns ----
if (!is.null(pl_table)) {
  # All COGS line items from P&L (50xxx + fee accounts)
  cogs_rows <- pl_table[
    grepl("^5[0-9]{4}|^Cost of Goods|^Packaging|^Shipping|^Freight|^Commissions|^Warranty|^Bank Charge|^PayPal Fee|^Credit Card Fee",
          pl_table$label, ignore.case = TRUE) &
    !is.na(pl_table$value) &
    !grepl("^Total for", pl_table$label), ]

  if (nrow(cogs_rows) > 0) {
    cogs_breakdown             <- cogs_rows[order(-abs(cogs_rows$value)), ]
    cogs_breakdown$label_clean <- gsub("^[0-9]+ ", "", cogs_breakdown$label)
  }

  # Processing fees
  fee_labels <- c("51000 Bank Charges", "51001 PayPal Fees", "51100 Credit Card Fees")
  processing_fees <- pl_table[pl_table$label %in% fee_labels & !is.na(pl_table$value), ]
  if (nrow(processing_fees) > 0)
    processing_fees$label_clean <- gsub("^[0-9]+ ", "", processing_fees$label)
}

# ---- Transaction List: Purchase Orders coded to COGS (50xxx split) ----
if (file.exists(files$txlist)) {
  tx_full <- tryCatch(
    read_excel(files$txlist, skip = 3, col_names = TRUE),
    error = function(e) NULL
  )
  if (!is.null(tx_full) && ncol(tx_full) >= 9) {
    names(tx_full)[1:9] <- c("date","trans_type","num","posting","name","memo",
                              "account","split","amount")
    tx_full$date   <- as.Date(as.character(tx_full$date), format = "%m/%d/%Y")
    tx_full$amount <- suppressWarnings(as.numeric(tx_full$amount))
    tx_full <- tx_full[!is.na(tx_full$amount) & !is.na(tx_full$date), ]

    cogs_tx <- tx_full[
      !is.na(tx_full$split) &
      grepl("^50", tx_full$split) &
      tx_full$date >= as.Date("2026-01-01") &
      tx_full$date <= as.Date("2026-12-31"), ]

    if (nrow(cogs_tx) > 0) {
      cogs_tx$account_clean <- gsub("^[0-9]+ ", "", cogs_tx$split)
      cogs_tx$month         <- floor_date(cogs_tx$date, "month")

      # By supplier
      cogs_by_supplier <- cogs_tx |>
        dplyr::group_by(name, split) |>
        dplyr::summarise(total = sum(amount, na.rm = TRUE),
                         n_orders = dplyr::n(), .groups = "drop") |>
        dplyr::filter(!is.na(name) & name != "") |>
        dplyr::arrange(desc(abs(total)))

      # Monthly trend by account
      cogs_monthly <- cogs_tx |>
        dplyr::mutate(account_clean = gsub("^[0-9]+ ", "", split)) |>
        dplyr::group_by(month, account_clean) |>
        dplyr::summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(month)

      # Full detail table
      cogs_txn_detail <- cogs_tx[, c("date","trans_type","num","name","memo",
                                      "account_clean","amount")]
      names(cogs_txn_detail) <- c("Date","Type","PO #","Vendor","Memo",
                                   "COGS Account","Amount")
    }
  }
}

# ---- Logistics vendors ----
if (!is.null(vendor_data)) {
  logistics_vendors <- vendor_data[
    grepl("ship|freight|ups|usps|fedex|dhl|auctane|dimerco|express|postal|carrier",
          vendor_data$vendor, ignore.case = TRUE) &
    vendor_data$amount > 0, ]
  logistics_vendors <- logistics_vendors[order(-logistics_vendors$amount), ]
}

# ---- Key metrics ----
get_pl_val <- function(lbl) {
  if (is.null(pl_table)) return(NA_real_)
  v <- pl_table$value[pl_table$label %in% lbl & !is.na(pl_table$value)]
  if (length(v) == 0) NA_real_ else sum(v)
}

cogs_metrics <- list(
  product_cogs          = get_pl_val("50000 Product COGS"),
  shipping_cost         = get_pl_val(c("50700 Shipping & Delivery","50701 Freight IN")),
  processing_fees_total = get_pl_val(c("51000 Bank Charges","51001 PayPal Fees",
                                        "51100 Credit Card Fees")),
  total_cogs            = pl_metrics$cogs,
  packaging             = get_pl_val("50500 Packaging"),
  commissions           = get_pl_val("50900 Commissions"),
  warranty              = get_pl_val("Warranty Expense")
)

# ============================================================
# INSIGHTS & ALERTS — PRE-COMPUTED DATA
# ============================================================

# Monthly revenue by month (2026 only)
yoy_data <- if (nrow(revenue_transactions) > 0) {
  revenue_transactions[
    is_refund == FALSE & amount > 0,
    .(revenue = sum(amount, na.rm = TRUE)),
    by = .(year = yr, month_num = month(month))
  ][order(year, month_num)]
} else {
  data.table(year = integer(), month_num = integer(), revenue = numeric())
}

# Q1 2026 summary (no YoY comparison — 2025 data not loaded)
q1_2026_rev <- sum(yoy_data[year == 2026L & month_num <= 3]$revenue, na.rm = TRUE)
yoy_q1_chg  <- NA_real_

# Large refunds (all sources, full period, >= $1000)
large_refunds_all <- if (nrow(all_transactions) > 0) {
  r <- all_transactions[
    is_refund == TRUE & abs(amount) >= 1000 &
    toupper(currency) %in% c("USD", ""),
    .(date, source, amount, customer_name, customer_email,
      description, status, transaction_id)
  ][order(-abs(amount))]
  r
} else {
  data.table(date = as.Date(character()), source = character(),
             amount = numeric(), customer_name = character(),
             customer_email = character(), description = character(),
             status = character(), transaction_id = character())
}

# Expense waterfall data (from QB P&L)
waterfall_data <- NULL
if (!is.null(pl_table)) {
  fp <- function(pat) {
    r <- pl_table[grepl(pat, pl_table$label, perl = TRUE) & !is.na(pl_table$value), ]
    if (nrow(r) == 0) 0 else r$value[1]
  }
  wf_rev    <- ifelse(is.na(pl_metrics$gross_revenue), 0, pl_metrics$gross_revenue)
  wf_pcogs  <- fp("^50000 Product COGS")
  wf_ocogs  <- ifelse(is.na(pl_metrics$cogs), 0, pl_metrics$cogs) - wf_pcogs
  wf_legal  <- fp("^Total for 60700")
  wf_mkt    <- fp("^Total for 60900 Marketing")
  wf_staff  <- fp("^Total for 62100 Staffing")
  wf_office <- fp("^Total for 61800 Office")
  wf_web    <- fp("^63800 Website")
  wf_amort  <- fp("^65000 Amortization")
  wf_travel <- fp("^Total for 63200 Travel")
  wf_tax    <- fp("^63001 MD PTE") + fp("^63000 Taxes")
  wf_named  <- wf_legal + wf_mkt + wf_staff + wf_office + wf_web + wf_amort + wf_travel + wf_tax
  wf_other  <- ifelse(is.na(pl_metrics$total_expenses), 0, pl_metrics$total_expenses) - wf_named
  wf_net    <- ifelse(is.na(pl_metrics$net_income), 0, pl_metrics$net_income)

  waterfall_data <- data.frame(
    label   = c("Gross Revenue", "Product COGS", "Other COGS",
                "Gross Profit", "Legal & Prof.", "Marketing", "Staffing",
                "Office & Rent", "Website Costs", "Amortization",
                "Travel & Taxes", "Finance & Other", "Net Income"),
    value   = c(wf_rev, -wf_pcogs, -wf_ocogs,
                wf_rev - wf_pcogs - wf_ocogs,
                -wf_legal, -wf_mkt, -wf_staff,
                -wf_office, -wf_web, -wf_amort,
                -(wf_travel + wf_tax), -wf_other, wf_net),
    measure = c("absolute", "relative", "relative",
                "total", "relative", "relative", "relative",
                "relative", "relative", "relative",
                "relative", "relative", "total"),
    stringsAsFactors = FALSE
  )
}

message("=== LOAD COMPLETE ===")
message("Missing files: ", if (length(missing_files)) paste(missing_files, collapse=", ") else "none")
message("Revenue transactions: ", nrow(revenue_transactions))
