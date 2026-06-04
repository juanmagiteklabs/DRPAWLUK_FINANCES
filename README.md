# DrPawluk Finance Dashboard

R Shiny financial dashboard powered by bs4Dash.
Covers January 2025 – June 2026 across Braintree, PayPal HEP, PayPal HTW, and Stripe.

---

## Required R Packages

Run once to install all dependencies:

```r
install.packages(c(
  "bs4Dash",
  "shiny",
  "shinycssloaders",
  "data.table",
  "vroom",
  "readxl",
  "dplyr",
  "lubridate",
  "plotly",
  "DT",
  "scales",
  "htmltools"
))
```

---

## Data Files

All data files must remain in:

```
/Users/juancastillo/Downloads/June 04, 2026 DrPawluk Finances/
```

Expected files:

| File | Source |
|------|--------|
| `Braintree transaction_search.csv` | Braintree (~200 MB) |
| `HEP PAYPAL Jan 1, 2025 - Dec 31, 2025.CSV` | PayPal HEP |
| `HEP PAYPAL Jan 1, 2026 - Jun 1, 2026.CSV` | PayPal HEP |
| `High Tech Wellness Paypal Jan 1, 2025 - Dec 31, 2025.CSV` | PayPal HTW |
| `High Tech Wellness Paypal Jan 1, 2026 - Jun 1, 2026.CSV` | PayPal HTW |
| `Stripe Payment Report All Transaction.csv` | Stripe |
| `Health Energy Partners LLC_Profit and Loss.xlsx` | QuickBooks |
| `Health Energy Partners LLC_Purchases by Vendor Detail.xlsx` | QuickBooks |
| `Health Energy Partners LLC_Transaction List by Date.xlsx` | QuickBooks |

If any file is missing, the dashboard skips it and shows a yellow warning banner — it will not crash.

To use a different data directory, edit line 11 of `global.R`:
```r
DATA_DIR <- "/your/path/to/data/folder"
```

---

## How to Run

```r
shiny::runApp("/Users/juancastillo/Downloads/drpawluk_finance_dashboard")
```

Or open the folder in RStudio and click **Run App**.

**First launch takes 30–60 seconds** while the Braintree file (~200 MB, 415k rows) loads.
Subsequent launches within the same R session are instant.

---

## Dashboard Tabs

| Tab | Contents |
|-----|----------|
| **Overview** | 4 KPI boxes, monthly revenue trend, revenue by source |
| **Transaction Volume** | Monthly counts by source, avg value, Braintree failure rate |
| **Profit & Loss** | QuickBooks P&L summary, monthly trend, top vendor spend |
| **Raw Data Explorer** | Filterable full transaction table with CSV export |

### Global Sidebar Filters
- **Date Range** — applies to all 4 tabs
- **Payment Source** — toggle Braintree / PayPal HEP / PayPal HTW / Stripe

---

## Notes

- **Revenue figures** (Tabs 1–2) come from settled/completed/paid USD transactions only.
  PayPal outgoing payments (wages, subscriptions) are excluded via Balance Impact filter.
- **P&L figures** come from QuickBooks exports (aggregate Jan 2025 – Jun 2026).
- **Monthly P&L chart** combines payment-processor revenue with QuickBooks expense data.
- **Braintree failure rate** is very high (~95%) because most rows are fraud/bot attempts
  that were gateway-rejected before reaching settlement.
