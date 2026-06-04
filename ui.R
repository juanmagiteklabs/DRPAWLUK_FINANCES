# ---- shared constants ----
CHART_H   <- "340px"
CARD_H    <- "420px"
TALL_H    <- "520px"
TABLE_SCY <- "310px"

card <- function(..., width = 6, height = CARD_H) {
  box(..., width = width, collapsible = FALSE,
      style = paste0("height:", height, "; overflow:hidden;"))
}

# ---- Login panel (same pattern as ShipStation_Dashboard) ----
login_panel <- tags$div(
  id    = "login_panel",
  style = "position:fixed; top:0; left:0; width:100%; height:100%; z-index:9999;
           background:#F4F6F9; display:flex; align-items:center; justify-content:center;",
  tags$div(
    style = "background:#FFFFFF; border:1px solid #DEE2E6; border-radius:10px;
             padding:40px 48px; width:380px; box-shadow:0 4px 20px rgba(0,0,0,.08);
             font-family:'IBM Plex Mono',monospace;",
    tags$div(style = "text-align:center; margin-bottom:28px;",
      tags$div(style = "font-size:11px; letter-spacing:.15em; text-transform:uppercase;
                        color:#6C757D; margin-bottom:6px;", "DRPAWLUK FINANCES"),
      tags$h4(style = "margin:0; color:#1A1A2E; font-weight:600;", "Sign In"),
      tags$p(style = "color:#6C757D; font-size:12px; margin-top:4px;",
             "Restricted access — authorized users only")
    ),
    tags$div(style = "margin-bottom:14px;",
      tags$label("USERNAME", style="font-size:10px;letter-spacing:.1em;color:#6C757D;"),
      textInput("login_user", label=NULL, placeholder="Enter username", width="100%")
    ),
    tags$div(style = "margin-bottom:20px;",
      tags$label("PASSWORD", style="font-size:10px;letter-spacing:.1em;color:#6C757D;"),
      passwordInput("login_pass", label=NULL, placeholder="Enter password", width="100%")
    ),
    actionButton("login_btn", "Sign In", width="100%", class="btn-login"),
    uiOutput("login_error_msg"),
    tags$script(HTML(
      "$(document).on('keypress','#login_user',function(e){if(e.which===13){$('#login_pass').focus();}});
       $(document).on('keypress','#login_pass',function(e){if(e.which===13){$('#login_btn').click();}});"
    ))
  )
)

ui <- tagList(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .btn-login {
      background:#00A878 !important; color:#fff !important;
      border:none !important; border-radius:5px !important;
      font-family:'IBM Plex Mono',monospace !important;
      font-size:12px !important; font-weight:600 !important;
      letter-spacing:.08em !important; text-transform:uppercase !important;
    }
    .btn-login:hover { background:#008f65 !important; }
  "))),
  login_panel,
  shinyjs::hidden(tags$div(id = "main_app",

bs4DashPage(
  dark = FALSE, help = FALSE, scrollToTop = FALSE,
  title = "DrPawluk Finances",

  # ---- HEADER ----
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$span(
        style = "font-family:'IBM Plex Mono',monospace; font-size:13px;
                 letter-spacing:0.1em; text-transform:uppercase; color:#FFFFFF;",
        "DRPAWLUK FINANCES"),
      color = "gray-dark"),
    skin = "dark", status = "gray-dark", border = FALSE),

  # ---- SIDEBAR ----
  sidebar = dashboardSidebar(
    skin = "dark", status = "gray-dark", elevation = 4, collapsed = FALSE,

    div(style = "padding:14px 16px 4px;",
      tags$p(style = "font-size:10px; letter-spacing:.12em; text-transform:uppercase;
                      color:#AAAAAA; margin-bottom:4px;", "DATE RANGE"),
      dateRangeInput("date_range", label = NULL,
                     start = "2025-01-01", end = "2026-06-01",
                     min   = "2025-01-01", max = "2026-06-01",
                     format = "M d, yy", separator = " – ")
    ),

    div(style = "padding:10px 16px 4px;",
      tags$p(style = "font-size:10px; letter-spacing:.12em; text-transform:uppercase;
                      color:#AAAAAA; margin-bottom:4px;", "PAYMENT SOURCE"),
      checkboxGroupInput("sources", label = NULL,
                         choices = SOURCES_ALL, selected = SOURCES_AVAILABLE)
    ),

    div(style = "padding:10px 16px 14px;",
      actionButton(
        "apply_filters", "Apply Filters",
        icon  = icon("filter"),
        class = "btn-apply",
        width = "100%"
      )
    ),

    tags$hr(style = "border-color:#2A2A2A; margin:4px 0 8px;"),

    sidebarMenu(id = "main_menu",
      menuItem("Overview",           tabName = "overview", icon = icon("chart-line")),
      menuItem("Transaction Volume",  tabName = "volume",   icon = icon("exchange-alt")),
      menuItem("Profit & Loss",       tabName = "pl",       icon = icon("balance-scale")),
      menuItem("Insights & Alerts",   tabName = "insights", icon = icon("exclamation-circle")),
      menuItem("COGS & Logistics",    tabName = "cogs",     icon = icon("truck")),
      menuItem("Raw Data Explorer",   tabName = "rawdata",  icon = icon("table"))
    )
  ),

  # ---- BODY ----
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    if (length(missing_files) > 0)
      div(class = "warn-banner",
          icon("exclamation-triangle"),
          tags$strong(" Missing: "),
          paste(missing_files, collapse = ", ")),

    tabItems(

      # =========================================================
      # TAB 1 — OVERVIEW
      # =========================================================
      tabItem(tabName = "overview",

        fluidRow(
          valueBoxOutput("kpi_gross_revenue", width = 3),
          valueBoxOutput("kpi_refunds",        width = 3),
          valueBoxOutput("kpi_net_revenue",    width = 3),
          valueBoxOutput("kpi_tx_count",       width = 3)
        ),

        fluidRow(
          card(width = 8, height = CARD_H,
            title = tags$span(class = "ct", icon("chart-line"), " MONTHLY REVENUE TREND"),
            withSpinner(plotlyOutput("chart_monthly_trend", height = CHART_H),
                        color = "#00C896", type = 4)
          ),
          card(width = 4, height = CARD_H,
            title = tags$span(class = "ct", icon("chart-pie"), " REVENUE BY SOURCE"),
            withSpinner(plotlyOutput("chart_source_pie", height = CHART_H),
                        color = "#00C896", type = 4)
          )
        ),

        fluidRow(
          card(width = 12, height = CARD_H,
            title = tags$span(class = "ct", icon("chart-bar"), " NET REVENUE BY SOURCE — MONTHLY"),
            withSpinner(plotlyOutput("chart_source_bar", height = CHART_H),
                        color = "#00C896", type = 4)
          )
        )
      ),

      # =========================================================
      # TAB 2 — TRANSACTION VOLUME
      # =========================================================
      tabItem(tabName = "volume",

        fluidRow(
          valueBoxOutput("kpi_total_txn",    width = 3),
          valueBoxOutput("kpi_failed_txn",   width = 3),
          valueBoxOutput("kpi_failure_rate", width = 3),
          valueBoxOutput("kpi_avg_value",    width = 3)
        ),

        fluidRow(
          card(width = 7, height = CARD_H,
            title = tags$span(class = "ct", icon("layer-group"),
                              " MONTHLY TRANSACTIONS BY SOURCE"),
            withSpinner(plotlyOutput("chart_vol_stacked", height = CHART_H),
                        color = "#1E90FF", type = 4)
          ),
          card(width = 5, height = CARD_H,
            title = tags$span(class = "ct", icon("dollar-sign"),
                              " AVG TRANSACTION VALUE BY SOURCE"),
            withSpinner(plotlyOutput("chart_avg_value", height = CHART_H),
                        color = "#1E90FF", type = 4)
          )
        ),

        fluidRow(
          card(width = 12, height = CARD_H,
            title = tags$span(class = "ct", icon("times-circle"),
                              " BRAINTREE FAILED & DECLINED BY MONTH"),
            withSpinner(plotlyOutput("chart_failed", height = CHART_H),
                        color = "#FF4C4C", type = 4)
          )
        )
      ),

      # =========================================================
      # TAB 3 — PROFIT & LOSS
      # =========================================================
      tabItem(tabName = "pl",

        fluidRow(
          valueBoxOutput("kpi_pl_revenue", width = 3),
          valueBoxOutput("kpi_pl_cogs",    width = 3),
          valueBoxOutput("kpi_pl_profit",  width = 3),
          valueBoxOutput("kpi_pl_net",     width = 3)
        ),

        fluidRow(
          card(width = 7, height = CARD_H,
            title = tags$span(class = "ct", icon("calendar"),
                              " MONTHLY REVENUE & EXPENSES"),
            withSpinner(plotlyOutput("chart_monthly_pl", height = CHART_H),
                        color = "#00C896", type = 4)
          ),
          card(width = 5, height = CARD_H,
            title = tags$span(class = "ct", icon("file-invoice-dollar"),
                              " P&L SUMMARY"),
            withSpinner(DTOutput("table_pl_summary"),
                        color = "#00C896", type = 4)
          )
        ),

        fluidRow(
          card(width = 8, height = TALL_H,
            title = tags$span(class = "ct", icon("building"), " TOP VENDOR SPEND"),
            withSpinner(plotlyOutput("chart_vendor_spend", height = "450px"),
                        color = "#1E90FF", type = 4)
          ),
          card(width = 4, height = TALL_H,
            title = tags$span(class = "ct", icon("list"), " TOP 15 VENDORS"),
            withSpinner(DTOutput("table_vendor_top"),
                        color = "#1E90FF", type = 4)
          )
        )
      ),

      # =========================================================
      # TAB 4 — INSIGHTS & ALERTS
      # =========================================================
      tabItem(tabName = "insights",

        fluidRow(
          valueBoxOutput("kpi_alert_net",    width = 3),
          valueBoxOutput("kpi_alert_cogs",   width = 3),
          valueBoxOutput("kpi_alert_yoy",    width = 3),
          valueBoxOutput("kpi_alert_refund", width = 3)
        ),

        fluidRow(
          card(width = 6, height = CARD_H,
            title = tags$span(class="ct", icon("calendar"), " YoY MONTHLY REVENUE — 2025 vs 2026"),
            withSpinner(plotlyOutput("chart_yoy", height = CHART_H), color="#1971C2", type=4)
          ),
          card(width = 6, height = CARD_H,
            title = tags$span(class="ct", icon("percent"), " MONTH-OVER-MONTH REVENUE CHANGE"),
            withSpinner(plotlyOutput("chart_mom", height = CHART_H), color="#E03131", type=4)
          )
        ),

        fluidRow(
          card(width = 6, height = CARD_H,
            title = tags$span(class="ct", icon("fire"), " MONTHLY REVENUE vs EXPENSES (QuickBooks)"),
            withSpinner(plotlyOutput("chart_burn_rate", height = CHART_H), color="#E03131", type=4)
          ),
          card(width = 6, height = CARD_H,
            title = tags$span(class="ct", icon("exclamation-triangle"), " REFUND RATE BY PROCESSOR"),
            withSpinner(plotlyOutput("chart_refund_rates", height = CHART_H), color="#E67700", type=4)
          )
        ),

        fluidRow(
          card(width = 6, height = CARD_H,
            title = tags$span(class="ct", icon("stripe"), " STRIPE MONTHLY GROWTH"),
            withSpinner(plotlyOutput("chart_stripe_growth", height = CHART_H), color="#00A878", type=4)
          ),
          card(width = 6, height = CARD_H,
            title = tags$span(class="ct", icon("exclamation-circle"), " LARGE REFUNDS  ≥ $1,000"),
            withSpinner(DTOutput("table_large_refunds"), color="#E03131", type=4)
          )
        ),

        fluidRow(
          box(
            title  = tags$span(class="ct", icon("stream"), " EXPENSE WATERFALL — GROSS REVENUE TO NET INCOME"),
            status = "gray-dark", width = 12, collapsible = FALSE,
            style  = "overflow:hidden;",
            withSpinner(plotlyOutput("chart_waterfall", height = "400px"), color="#1971C2", type=4)
          )
        )
      ),

      # =========================================================
      # TAB 5 — COGS & LOGISTICS
      # =========================================================
      tabItem(tabName = "cogs",

        # --- Row 1: KPI boxes (4 equal) ---
        fluidRow(
          valueBoxOutput("kpi_cogs_product",    width = 3),
          valueBoxOutput("kpi_cogs_shipping",   width = 3),
          valueBoxOutput("kpi_cogs_fees",       width = 3),
          valueBoxOutput("kpi_cogs_total",      width = 3)
        ),

        # --- Row 2: P&L COGS breakdown + Processing fees donut ---
        fluidRow(
          card(width = 6, height = CARD_H,
            title = tags$span(class = "ct", icon("list-ol"),
                              " COGS LINE ITEMS (QuickBooks P&L)"),
            withSpinner(plotlyOutput("chart_cogs_breakdown", height = CHART_H),
                        color = "#1971C2", type = 4)
          ),
          card(width = 6, height = CARD_H,
            title = tags$span(class = "ct", icon("credit-card"),
                              " PROCESSING FEES SPLIT"),
            withSpinner(plotlyOutput("chart_processing_fees", height = CHART_H),
                        color = "#E67700", type = 4)
          )
        ),

        # --- Row 3: Product COGS by supplier + monthly PO trend ---
        fluidRow(
          card(width = 6, height = CARD_H,
            title = tags$span(class = "ct", icon("industry"),
                              " PRODUCT COGS BY SUPPLIER"),
            withSpinner(plotlyOutput("chart_cogs_supplier", height = CHART_H),
                        color = "#1971C2", type = 4)
          ),
          card(width = 6, height = CARD_H,
            title = tags$span(class = "ct", icon("calendar-alt"),
                              " MONTHLY COGS PURCHASE ORDERS"),
            withSpinner(plotlyOutput("chart_cogs_monthly", height = CHART_H),
                        color = "#1971C2", type = 4)
          )
        ),

        # --- Row 4: Logistics vendors chart + logistics table ---
        fluidRow(
          card(width = 6, height = CARD_H,
            title = tags$span(class = "ct", icon("truck"),
                              " LOGISTICS & SHIPPING VENDORS"),
            withSpinner(plotlyOutput("chart_logistics", height = CHART_H),
                        color = "#1971C2", type = 4)
          ),
          card(width = 6, height = CARD_H,
            title = tags$span(class = "ct", icon("table"),
                              " LOGISTICS VENDOR DETAIL"),
            withSpinner(DTOutput("table_logistics"),
                        color = "#1971C2", type = 4)
          )
        ),

        # --- Row 5: Full COGS purchase order transactions ---
        fluidRow(
          box(
            title = tags$span(class = "ct", icon("receipt"),
                              " ALL COGS PURCHASE ORDERS — TRANSACTION DETAIL"),
            status = "gray-dark", width = 12, collapsible = FALSE,
            style = "overflow: hidden;",
            withSpinner(DTOutput("table_cogs_detail"),
                        color = "#1971C2", type = 4)
          )
        )
      ),

      # =========================================================
      # TAB 5 — RAW DATA
      # =========================================================
      tabItem(tabName = "rawdata",

        fluidRow(
          column(12,
            div(class = "filter-bar",
              div(class = "filter-item",
                tags$label("MIN AMOUNT ($)", class = "filter-label"),
                numericInput("filter_amount_min", NULL, value = NULL, min = -1e6)
              ),
              div(class = "filter-item",
                tags$label("MAX AMOUNT ($)", class = "filter-label"),
                numericInput("filter_amount_max", NULL, value = NULL, max = 1e6)
              ),
              div(class = "filter-item filter-wide",
                tags$label("STATUS", class = "filter-label"),
                selectizeInput("filter_status", NULL, choices = NULL,
                               multiple = TRUE,
                               options = list(placeholder = "All statuses"))
              ),
              div(class = "filter-item filter-btn",
                downloadButton("btn_export_csv", "Export CSV", class = "btn-export")
              )
            )
          )
        ),

        fluidRow(
          box(
            title = tags$span(class = "ct", icon("table"), " ALL TRANSACTIONS"),
            status = "gray-dark", width = 12, collapsible = FALSE,
            style = "overflow:hidden;",
            withSpinner(DTOutput("table_raw"), color = "#FFD700", type = 4)
          )
        )
      )
    )
  ),

  footer = dashboardFooter(
    left  = tags$span(style = "color:#AAAAAA;font-size:11px;",
                      "DrPawluk Finances Dashboard"),
    right = tags$span(style = "color:#AAAAAA;font-size:11px;",
                      "Jan 2025 – Jun 2026")
  )
)   # end bs4DashPage
))) # end tags$div + shinyjs::hidden + tagList
