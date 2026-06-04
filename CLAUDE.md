# DRPAWLUK_FINANCES — Claude Code Context

## What This App Does
R Shiny financial dashboard for DrPawluk / Health Energy Partners LLC.
Covers January 2025 – June 2026 across four payment processors.

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
6. **Raw Data Explorer** — filterable full transaction table + CSV export

## Key Findings (as of Jun 2026)
- Revenue down **-38.6% YoY** (Q1 2025 → Q1 2026)
- Net Income: **-$180,009** over the full period
- COGS ratio: **70.9%** (critical threshold is 65%)
- PayPal HEP refund rate: **~11.3%** (danger zone, suspend threshold is 12%)
- Braintree shows **99.8% failure rate** due to active bot/fraud attack
- Stripe launched Mar 2026, growing fast (only healthy revenue trend)

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
