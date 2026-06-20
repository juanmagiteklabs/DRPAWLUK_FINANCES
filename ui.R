# ---- shared constants ----
CHART_H   <- "340px"
CARD_H    <- "420px"
TALL_H    <- "520px"
TABLE_SCY <- "310px"

card <- function(..., width = 6, height = CARD_H) {
  box(..., width = width, collapsible = FALSE,
      style = paste0("height:", height, "; overflow:hidden;"))
}

# ---- KPI components ----
kpi_box <- function(value_id, label, icon_name, accent_color) {
  tags$div(
    class = "col-sm-3",
    tags$div(
      class = "ss-kpi-box",
      style = paste0("border-top: 4px solid ", accent_color, ";"),
      tags$div(
        class = "ss-kpi-icon",
        style = paste0(
          "background:", accent_color, "1A;",
          "color:", accent_color, ";"
        ),
        icon(icon_name)
      ),
      tags$div(
        class = "ss-kpi-content",
        tags$div(class = "ss-kpi-value", textOutput(value_id, inline = TRUE)),
        tags$div(class = "ss-kpi-label", label)
      )
    )
  )
}

kpi_row <- function(...) {
  tags$div(
    class = "row ss-kpi-row",
    style = "margin-bottom:1.4rem;",
    ...
  )
}

# ---- Login panel ----
login_panel <- tags$div(
  id    = "login_panel",
  style = "position:fixed; top:0; left:0; width:100%; height:100%; z-index:9999;
           background:#F4F8FD; display:flex; align-items:center; justify-content:center;",
  tags$div(
    style = "background:#FFFFFF; border:1px solid #D6E6F5; border-radius:10px;
             padding:40px 48px; width:380px; box-shadow:0 4px 20px rgba(75,79,143,0.12);
             font-family:'Source Sans Pro','Segoe UI',system-ui,sans-serif;",
    tags$div(style = "text-align:center; margin-bottom:28px;",
      tags$div(style = "display:flex; align-items:center; justify-content:center; margin-bottom:12px;",
        tags$div(
          style = paste0(
            "width:40px; height:40px; border-radius:50%; flex-shrink:0;",
            "background:linear-gradient(135deg,#9BC3E6 0%,#4B4F8F 100%);",
            "display:flex; align-items:center; justify-content:center;",
            "margin-right:10px; box-shadow:0 2px 8px rgba(75,79,143,0.35);"
          ),
          tags$span(style = "font-size:18px; line-height:1;", "\U0001F4B0")
        ),
        tags$span(
          style = "font-weight:700; font-size:1rem; letter-spacing:0.04em; color:#4B4F8F; text-transform:uppercase;",
          "DrPawluk Finances"
        )
      ),
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

  # ---- Global loading indicator (bottom-right, fades out when app ready) ----
  tags$div(
    id = "global_loading",
    style = paste0(
      "position:fixed; bottom:22px; right:22px; z-index:99999;",
      "background:#2C2F5B; color:#ffffff; border:1px solid rgba(155,195,230,0.4);",
      "border-radius:10px; padding:10px 16px; font-size:0.82rem; font-weight:600;",
      "display:flex; align-items:center; gap:10px;",
      "box-shadow:0 6px 24px rgba(44,47,91,0.5);"
    ),
    tags$span(
      class = "spinner-border spinner-border-sm",
      style = "color:#9BC3E6; width:16px; height:16px; border-width:2px;",
      role = "status"
    ),
    "Loading dashboard..."
  ),
  tags$script(HTML(
    "$(document).on('shiny:connected', function() {
       $('#global_loading').fadeOut(600);
     });"
  )),

  tags$head(tags$style(HTML("
    .btn-login {
      background:#4B4F8F !important; color:#fff !important;
      border:none !important; border-radius:5px !important;
      font-family:'Source Sans Pro','Segoe UI',system-ui,sans-serif !important;
      font-size:12px !important; font-weight:600 !important;
      letter-spacing:.08em !important; text-transform:uppercase !important;
    }
    .btn-login:hover { background:#383C72 !important; }
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
        style = "font-family:'Source Sans Pro','Segoe UI',system-ui,sans-serif; font-size:13px;
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
                     start = "2026-01-01", end = "2026-06-30",
                     min   = "2025-01-01", max = "2026-12-31",
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
      menuItem("Overview",           tabName = "overview",  icon = icon("chart-line")),
      menuItem("Transaction Volume",  tabName = "volume",    icon = icon("exchange-alt")),
      menuItem("Profit & Loss",       tabName = "pl",        icon = icon("balance-scale")),
      menuItem("Insights & Alerts",   tabName = "insights",  icon = icon("exclamation-circle")),
      menuItem("COGS & Logistics",    tabName = "cogs",      icon = icon("truck")),
      menuItem("Operations Costs",    tabName = "ops",       icon = icon("cogs")),
      menuItem("Raw Data Explorer",   tabName = "rawdata",   icon = icon("table"))
    )
  ),

  # ---- BODY ----
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:ital,wght@0,300;0,400;0,700;1,400&display=swap"),
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

        kpi_row(
          kpi_box("kpi_gross_revenue", "Gross Revenue",       "dollar-sign",       "#4B4F8F"),
          kpi_box("kpi_refunds",       "Total Refunds",        "rotate-left",       "#EF4444"),
          kpi_box("kpi_net_revenue",   "Net Revenue",          "chart-line",        "#10B981"),
          kpi_box("kpi_tx_count",      "Total Transactions",   "exchange-alt",      "#9BC3E6")
        ),

        fluidRow(
          card(width = 8, height = CARD_H,
            title = tags$span(class = "ct", icon("chart-line"), " MONTHLY REVENUE TREND"),
            withSpinner(plotlyOutput("chart_monthly_trend", height = CHART_H),
                        color = "#9BC3E6", type = 4)
          ),
          card(width = 4, height = CARD_H,
            title = tags$span(class = "ct", icon("chart-pie"), " REVENUE BY SOURCE"),
            withSpinner(plotlyOutput("chart_source_pie", height = CHART_H),
                        color = "#9BC3E6", type = 4)
          )
        ),

        fluidRow(
          card(width = 12, height = CARD_H,
            title = tags$span(class = "ct", icon("chart-bar"), " NET REVENUE BY SOURCE — MONTHLY"),
            withSpinner(plotlyOutput("chart_source_bar", height = CHART_H),
                        color = "#9BC3E6", type = 4)
          )
        )
      ),

      # =========================================================
      # TAB 2 — TRANSACTION VOLUME
      # =========================================================
      tabItem(tabName = "volume",

        kpi_row(
          kpi_box("kpi_total_txn",    "Successful Transactions", "check-circle",         "#10B981"),
          kpi_box("kpi_failed_txn",   "Failed / Declined",        "circle-xmark",         "#EF4444"),
          kpi_box("kpi_failure_rate", "Braintree Failure Rate",   "exclamation-triangle", "#F59E0B"),
          kpi_box("kpi_avg_value",    "Avg Transaction Value",    "calculator",           "#4B4F8F")
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

        kpi_row(
          kpi_box("kpi_pl_revenue", "Gross Revenue",      "dollar-sign",    "#4B4F8F"),
          kpi_box("kpi_pl_cogs",    "Cost of Goods Sold", "boxes-stacked",  "#F59E0B"),
          kpi_box("kpi_pl_profit",  "Gross Profit",        "chart-line",     "#10B981"),
          kpi_box("kpi_pl_net",     "Net Income",          "scale-balanced", "#EF4444")
        ),

        fluidRow(
          card(width = 7, height = CARD_H,
            title = tags$span(class = "ct", icon("calendar"),
                              " MONTHLY REVENUE & EXPENSES"),
            withSpinner(plotlyOutput("chart_monthly_pl", height = CHART_H),
                        color = "#9BC3E6", type = 4)
          ),
          card(width = 5, height = CARD_H,
            title = tags$span(class = "ct", icon("file-invoice-dollar"),
                              " P&L SUMMARY"),
            withSpinner(DTOutput("table_pl_summary"),
                        color = "#9BC3E6", type = 4)
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

        kpi_row(
          kpi_box("kpi_alert_net",    "Net Income (QB)",        "heart-pulse",          "#EF4444"),
          kpi_box("kpi_alert_cogs",   "COGS % of Revenue",      "boxes-stacked",        "#F59E0B"),
          kpi_box("kpi_alert_yoy",    "Revenue YoY (Q1)",        "arrow-trend-down",     "#EF4444"),
          kpi_box("kpi_alert_refund", "PayPal HEP Refund Rate",  "exclamation-triangle", "#F59E0B")
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
        kpi_row(
          kpi_box("kpi_cogs_product",  "Product COGS",       "boxes-stacked", "#EF4444"),
          kpi_box("kpi_cogs_shipping", "Shipping & Freight", "truck",          "#F59E0B"),
          kpi_box("kpi_cogs_fees",     "Processing Fees",    "credit-card",    "#4B4F8F"),
          kpi_box("kpi_cogs_total",    "Total COGS",          "dollar-sign",    "#EF4444")
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
      # TAB — OPERATIONS COSTS (placeholder)
      # =========================================================
      tabItem(tabName = "ops",
        fluidRow(
          box(
            title = tags$span(class = "ct", icon("cogs"), " OPERATIONS COSTS"),
            status = "gray-dark", width = 12, collapsible = FALSE,
            tags$div(
              style = "padding:60px 0; text-align:center; color:#6B7280;",
              icon("cogs", style = "font-size:3rem; color:#9BC3E6; margin-bottom:18px; display:block;"),
              tags$h4(style = "color:#4B4F8F; font-weight:600;", "Operations Costs"),
              tags$p(style = "font-size:0.95rem;", "This section is under construction.")
            )
          )
        )
      ),

      # =========================================================
      # TAB — RAW DATA
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
