# DRPAWLUK_FINANCES — Claude Code Context

## What This App Does
R Shiny financial dashboard for DrPawluk / Health Energy Partners LLC.
Default date range: January 2026 – June 2026. Full historical range: Jan 2025 – Jun 2026.

**Live URL:** https://magiteklabs.shinyapps.io/DRPAWLUK_FINANCES/
**shinyapps.io account:** magiteklabs (juan@magiteklabs.co)

## Authentication
Same login-panel pattern as ShipStation_Dashboard.
Credentials are set as **environment variables** in the shinyapps.io console:
- `DRPAWLUK_USER` — username
- `DRPAWLUK_PASSWORD` — password

Local fallback defaults: `admin` / `drpawluk2025` (override in `.Renviron`).

## Data Sources
All data files live in `data/` (gitignored — never commit financial data).

| File | Source | Notes |
|------|--------|-------|
| `braintree_cache.rds` | Braintree export (slimmed) | Built from 200MB CSV on first local run |
| `HEP PAYPAL *.CSV` | PayPal Health Energy Partners | 2025 + 2026 files |
| `High Tech Wellness Paypal *.CSV` | PayPal High Tech Wellness | 2025 + 2026 files |
| `Stripe Payment Report All Transaction.csv` | Stripe | Mar–Jun 2026 |
| `Health Energy Partners LLC_*.xlsx` | QuickBooks exports | P&L, Purchases, TX List |

**Braintree note:** The original CSV (~200MB, 415k rows) stays local only.
`braintree_cache.rds` (32KB) holds only the 1,043 settled transactions + monthly
failure-count stats. On first local run with the CSV present, the RDS is auto-built.

## App Structure
```
global.R        — data loading, normalization, shared objects
ui.R            — bs4Dash layout, login panel, 6 tabs
server.R        — all reactive logic, chart rendering, table rendering
www/custom.css  — light-theme CSS overrides
data/           — (gitignored) all data files go here
```

## Tabs
1. **Overview** — 4 KPIs, monthly trend, source charts
2. **Transaction Volume** — counts, avg values, Braintree failure rate
3. **Profit & Loss** — QuickBooks P&L, vendor spend
4. **Insights & Alerts** — YoY comparison, MoM %, refund rates, waterfall
5. **COGS & Logistics** — supplier breakdown, shipping vendors
6. **Operations Costs** — placeholder tab (under construction)
7. **Raw Data Explorer** — filterable full transaction table + CSV export

## QuickBooks Data (Jan–Jun 19, 2026)
- Gross Revenue: **$440,448.70**
- COGS: **$237,814.19** (53.9% ratio)
- Gross Profit: **$202,634.51**
- Total Expenses: **$262,995.72**
- **Net Income: -$61,008.40**

## Data Files (all in data/, gitignored — deployed to shinyapps.io manually)
| File | Source | Period | Rows |
|------|--------|--------|------|
| `HEP PAYPAL Jan 1, 2026 - Jun 1, 2026.CSV` | PayPal HEP | Jan–Jun 2026 | 198 |
| `High Tech Wellness Paypal Jan 1, 2026 - Jun 1, 2026.CSV` | PayPal HTW | Jan–Jun 2026 | 34 |
| `Stripe Payment Report All Transaction.csv` | Stripe | Mar–Jun 2026 | 275 |
| `Braintree transaction_search.csv` | Braintree | Jan–Feb 2026 | 322 |
| `braintree_cache.rds` | Braintree (auto-built) | Jan–Feb 2026 | 193 settled |
| `Health Energy Partners LLC_Profit and Loss.xlsx` | QuickBooks | Jan–Jun 19, 2026 | – |
| `Health Energy Partners LLC_Purchases by Vendor Detail.xlsx` | QuickBooks | Jan–Jun 19, 2026 | 29 vendors |
| `Health Energy Partners LLC_Transaction List by Date.xlsx` | QuickBooks | Jun 2026 | 103 rows |

## Known Gaps
- 2025 PayPal files not available — export from PayPal Activity Report if needed
- Stripe reports go to alex@drpawluk.com, not juan@magiteklabs.co

## Data Loading Notes
- PayPal: `setnames` regex MUST NOT use `\s` inside `[...]` — R's TRE treats it as literal 's',
  stripping trailing 's' from "Status"→"Statu", "Gross"→"Gro". Fixed with `trimws()`.
- Vendor amounts: column 10 of the xlsx (not 9)
- Transaction List: skip=3 rows (header row 4, data starts row 5)

## Deployment
```r
rsconnect::setAccountInfo(
  name   = 'magiteklabs',
  token  = '<token>',
  secret = '<secret>'
)
rsconnect::deployApp(
  appDir  = ".",
  appName = "DRPAWLUK_FINANCES",
  account = "magiteklabs"
)
```

## Local Run
```r
shiny::runApp(".")
```
First run reads Braintree CSV (~30s). Subsequent runs use RDS cache (~3s).

## Required Packages
```r
install.packages(c(
  "bs4Dash","shiny","shinyjs","shinycssloaders",
  "data.table","vroom","readxl","dplyr","lubridate",
  "plotly","DT","scales","htmltools","rsconnect"
))
```
