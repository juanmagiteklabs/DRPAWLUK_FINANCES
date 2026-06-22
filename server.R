server <- function(input, output, session) {

  # ============================================================
  # AUTHENTICATION  (env vars: DRPAWLUK_USER / DRPAWLUK_PASSWORD)
  # ============================================================
  correct_user <- Sys.getenv("DRPAWLUK_USER",     unset = "admin")
  correct_pass <- Sys.getenv("DRPAWLUK_PASSWORD",  unset = "drpawluk2025")

  observeEvent(input$login_btn, {
    user <- trimws(input$login_user %||% "")
    pass <- trimws(input$login_pass %||% "")
    if (user == correct_user && pass == correct_pass) {
      shinyjs::hide("login_panel")
      shinyjs::show("main_app")
      output$login_error_msg <- renderUI(NULL)
    } else {
      output$login_error_msg <- renderUI(
        tags$p(style = "color:#E03131; font-size:12px; margin-top:10px; text-align:center;",
               icon("circle-exclamation"), " Invalid username or password.")
      )
    }
  })

  session$onSessionEnded(function() {
    shinyjs::show("login_panel")
    shinyjs::hide("main_app")
  })

  # ============================================================
  # REACTIVE: FILTERED DATA  (triggered by Apply button)
  # ============================================================
  filtered_txn <- reactive({
    req(input$date_range, input$sources)
    all_transactions[
      date >= input$date_range[1] & date <= input$date_range[2] &
      source %in% input$sources
    ]
  }) |> bindEvent(input$apply_filters, input$sources, ignoreNULL = FALSE, ignoreInit = FALSE)

  filtered_revenue <- reactive({
    filtered_txn()[
      toupper(currency) %in% c("USD", "") &
      status %in% SUCCESSFUL_STATUSES &
      !grepl("conversion|currency conversion", type, ignore.case = TRUE) &
      (is.na(balance_impact) | balance_impact == "credit")
    ]
  })

  observe({
    statuses <- sort(unique(all_transactions$status))
    updateSelectizeInput(session, "filter_status",
                         choices = statuses[!is.na(statuses)], selected = NULL)
  })

  # ============================================================
  # HELPER: apply dark plotly theme
  # ============================================================
  light_plot <- function(p) {
    p |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font      = list(color = "#1A1A2E", family = "'Source Sans Pro','Segoe UI',system-ui"),
        xaxis     = list(gridcolor = "#E9ECEF", zerolinecolor = "#CED4DA",
                         tickfont  = list(color = "#6C757D")),
        yaxis     = list(gridcolor = "#E9ECEF", zerolinecolor = "#CED4DA",
                         tickfont  = list(color = "#6C757D")),
        legend    = list(font = list(color = "#1A1A2E"), bgcolor = "rgba(0,0,0,0)",
                         orientation = "h", x = 0, y = -0.18),
        hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#CED4DA",
                          font = list(color = "#1A1A2E")),
        margin = list(t = 30, r = 10, b = 55, l = 65)
      ) |>
      config(displayModeBar = FALSE)
  }

  # alias kept for any remaining calls
  dark_plot <- light_plot

  empty_plot <- function(msg = "No data available") {
    light_plot(plot_ly() |>
      layout(annotations = list(list(
        text = msg, x = 0.5, y = 0.5,
        xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(color = "#6C757D", size = 13)
      ))))
  }

  # ============================================================
  # TAB 1: KPI VALUE BOXES
  # ============================================================
  output$kpi_gross_revenue <- renderText({
    rev  <- filtered_revenue()
    sale <- rev[is_refund == FALSE & amount > 0]
    fmt_currency(sum(sale$amount, na.rm = TRUE))
  })

  output$kpi_refunds <- renderText({
    rev <- filtered_revenue()
    ref <- rev[is_refund == TRUE]
    fmt_currency(sum(abs(ref$amount), na.rm = TRUE))
  })

  output$kpi_net_revenue <- renderText({
    rev     <- filtered_revenue()
    gross   <- sum(rev[is_refund == FALSE & amount > 0]$amount, na.rm = TRUE)
    refunds <- sum(abs(rev[is_refund == TRUE]$amount), na.rm = TRUE)
    fmt_currency(gross - refunds)
  })

  output$kpi_tx_count <- renderText({
    rev <- filtered_revenue()
    fmt_number(nrow(rev))
  })

  # ============================================================
  # TAB 1: MONTHLY TREND
  # ============================================================
  output$chart_monthly_trend <- renderPlotly({
    rev <- filtered_revenue()
    if (nrow(rev) == 0) return(empty_plot())

    monthly <- rev[, .(
      gross   = sum(amount[is_refund == FALSE & amount > 0], na.rm = TRUE),
      refunds = sum(abs(amount[is_refund == TRUE]), na.rm = TRUE),
      net     = sum(amount[is_refund == FALSE & amount > 0], na.rm = TRUE) -
                sum(abs(amount[is_refund == TRUE]), na.rm = TRUE)
    ), by = month][order(month)]

    dark_plot(
      plot_ly(monthly, x = ~month) |>
        add_trace(y = ~gross,   name = "Gross",  type = "scatter", mode = "lines+markers",
                  line = list(color = COLORS$green, width = 2.5),
                  marker = list(color = COLORS$green, size = 6)) |>
        add_trace(y = ~net,     name = "Net",    type = "scatter", mode = "lines+markers",
                  line = list(color = COLORS$blue, width = 2, dash = "dot"),
                  marker = list(color = COLORS$blue, size = 5)) |>
        add_trace(y = ~refunds, name = "Refunds",type = "scatter", mode = "lines+markers",
                  line = list(color = COLORS$red, width = 2),
                  marker = list(color = COLORS$red, size = 5)) |>
        layout(yaxis = list(tickprefix = "$", tickformat = ",.0f",
                            gridcolor = "#2A2A2A", zerolinecolor = "#2A2A2A",
                            tickfont = list(color = "#AAAAAA")),
               hovermode = "x unified")
    )
  })

  # ============================================================
  # TAB 1: REVENUE BY SOURCE PIE
  # ============================================================
  output$chart_source_pie <- renderPlotly({
    rev <- filtered_revenue()
    if (nrow(rev) == 0) return(empty_plot())

    by_src <- rev[is_refund == FALSE & amount > 0,
                  .(total = sum(amount, na.rm = TRUE)), by = source]

    dark_plot(
      plot_ly(by_src, labels = ~source, values = ~total,
              type = "pie", hole = 0.5,
              textinfo = "label+percent",
              hovertemplate = "<b>%{label}</b><br>$%{value:,.0f}<extra></extra>",
              marker = list(
                colors = c(COLORS$green, COLORS$blue, COLORS$yellow, "#9B59B6"),
                line   = list(color = "#0A0A0A", width = 2)
              )) |>
        layout(showlegend = TRUE,
               legend = list(orientation = "v", x = 1.02, y = 0.5))
    )
  })

  # ============================================================
  # TAB 1: NET REVENUE BY SOURCE STACKED BAR
  # ============================================================
  output$chart_source_bar <- renderPlotly({
    rev <- filtered_revenue()
    if (nrow(rev) == 0) return(empty_plot())

    src_colors <- c("Braintree" = COLORS$blue, "PayPal HEP" = COLORS$green,
                    "PayPal HTW" = COLORS$yellow, "Stripe" = "#9B59B6")

    monthly_src <- rev[, .(
      net = sum(amount[is_refund == FALSE & amount > 0], na.rm = TRUE) -
            sum(abs(amount[is_refund == TRUE]), na.rm = TRUE)
    ), by = .(month, source)][order(month)]

    p <- plot_ly()
    for (src in unique(monthly_src$source)) {
      d <- monthly_src[source == src]
      p <- add_trace(p, x = d$month, y = d$net, name = src, type = "bar",
                     marker = list(color = src_colors[src]),
                     hovertemplate = paste0("<b>", src, "</b><br>%{x|%b %Y}: $%{y:,.0f}<extra></extra>"))
    }
    dark_plot(layout(p, barmode = "stack",
                     yaxis = list(tickprefix = "$", tickformat = ",.0f"),
                     hovermode = "x unified"))
  })

  # ============================================================
  # TAB 2: VOLUME KPIs
  # ============================================================
  output$kpi_total_txn <- renderText({
    fmt_number(nrow(filtered_revenue()[is_refund == FALSE]))
  })

  output$kpi_failed_txn <- renderText({
    req(input$date_range)
    n <- sum(bt_failure_stats[
      !is.na(month) & month >= input$date_range[1] & month <= input$date_range[2]
    ]$N, na.rm = TRUE)
    fmt_number(n)
  })

  output$kpi_failure_rate <- renderText({
    req(input$date_range)
    failed <- sum(bt_failure_stats[
      !is.na(month) & month >= input$date_range[1] & month <= input$date_range[2]
    ]$N, na.rm = TRUE)
    rev_n  <- nrow(filtered_revenue()[source == "Braintree"])
    total  <- failed + rev_n
    rate   <- if (total > 0) round(failed / total * 100, 1) else 0
    paste0(rate, "%")
  })

  output$kpi_avg_value <- renderText({
    rev  <- filtered_revenue()
    sale <- rev[is_refund == FALSE & amount > 0]
    avg  <- if (nrow(sale) > 0) mean(sale$amount, na.rm = TRUE) else NA
    fmt_currency(avg)
  })

  # ============================================================
  # TAB 2: MONTHLY COUNT STACKED BAR
  # ============================================================
  output$chart_vol_stacked <- renderPlotly({
    rev <- filtered_revenue()
    if (nrow(rev) == 0) return(empty_plot())

    src_colors <- c("Braintree" = COLORS$blue, "PayPal HEP" = COLORS$green,
                    "PayPal HTW" = COLORS$yellow, "Stripe" = "#9B59B6")

    by_src <- rev[is_refund == FALSE, .N, by = .(month, source)][order(month)]
    p <- plot_ly()
    for (src in unique(by_src$source)) {
      d <- by_src[source == src]
      p <- add_trace(p, x = d$month, y = d$N, name = src, type = "bar",
                     marker = list(color = src_colors[src]),
                     hovertemplate = paste0("<b>", src, "</b><br>%{x|%b %Y}: %{y}<extra></extra>"))
    }
    dark_plot(layout(p, barmode = "stack",
                     yaxis = list(title = "Count", gridcolor = "#2A2A2A"),
                     hovermode = "x unified"))
  })

  # ============================================================
  # TAB 2: AVG VALUE BY SOURCE
  # ============================================================
  output$chart_avg_value <- renderPlotly({
    rev <- filtered_revenue()
    if (nrow(rev) == 0) return(empty_plot())

    avg_src <- rev[is_refund == FALSE & amount > 0,
                   .(avg_val = mean(amount, na.rm = TRUE),
                     n       = .N), by = source][order(avg_val)]

    n_rows <- nrow(avg_src)
    if (n_rows == 0) return(empty_plot("No sale transactions"))

    dark_plot(
      plot_ly(avg_src, y = ~source, x = ~avg_val, type = "bar", orientation = "h",
              marker = list(color = colorRampPalette(c(COLORS$blue, COLORS$green))(n_rows)),
              hovertemplate = "<b>%{y}</b><br>Avg: $%{x:,.2f}<extra></extra>") |>
        layout(xaxis = list(tickprefix = "$", tickformat = ",.0f"),
               yaxis = list(tickfont = list(color = "#FFFFFF")))
    )
  })

  # ============================================================
  # TAB 2: BRAINTREE FAILURES OVER TIME
  # ============================================================
  output$chart_failed <- renderPlotly({
    req(input$date_range)
    stats <- bt_failure_stats[
      !is.na(month) & month >= input$date_range[1] & month <= input$date_range[2]
    ]
    if (nrow(stats) == 0 || !("Braintree" %in% input$sources))
      return(empty_plot("No Braintree failure data for selected range"))

    status_colors <- c(
      "gateway_rejected"    = COLORS$red,
      "processor_declined"  = "#FF8C00",
      "failed"              = "#CC0000",
      "voided"              = COLORS$yellow,
      "settlement_declined" = "#FF69B4"
    )
    p <- plot_ly()
    for (st in unique(stats$status)) {
      d   <- stats[status == st]
      col <- if (!is.na(status_colors[st])) status_colors[[st]] else COLORS$red
      p   <- add_trace(p, x = d$month, y = d$N, name = st, type = "bar",
                       marker = list(color = col),
                       hovertemplate = paste0("<b>", st, "</b><br>%{x|%b %Y}: %{y:,}<extra></extra>"))
    }
    dark_plot(layout(p, barmode = "stack",
                     yaxis = list(tickformat = ",d"),
                     hovermode = "x unified"))
  })

  # ============================================================
  # TAB 3: P&L KPIs
  # ============================================================
  output$kpi_pl_revenue <- renderText({ fmt_currency(pl_metrics$gross_revenue) })
  output$kpi_pl_cogs    <- renderText({ fmt_currency(pl_metrics$cogs) })
  output$kpi_pl_profit  <- renderText({ fmt_currency(pl_metrics$gross_profit) })
  output$kpi_pl_net     <- renderText({ fmt_currency(pl_metrics$net_income) })

  # ============================================================
  # TAB 3: MONTHLY P&L CHART
  # ============================================================
  output$chart_monthly_pl <- renderPlotly({
    if (is.null(monthly_pl) || nrow(monthly_pl) == 0)
      return(empty_plot("No monthly P&L data"))

    dark_plot(
      plot_ly(monthly_pl, x = ~month) |>
        add_trace(y = ~gross_revenue, name = "Revenue",
                  type = "bar", marker = list(color = paste0(COLORS$green, "AA")),
                  hovertemplate = "Revenue: $%{y:,.0f}<extra></extra>") |>
        add_trace(y = ~-total_expenses, name = "Expenses",
                  type = "bar", marker = list(color = paste0(COLORS$red, "AA")),
                  customdata = ~total_expenses,
                  hovertemplate = "Expenses: $%{customdata:,.0f}<extra></extra>") |>
        add_trace(y = ~net_income, name = "Net Income",
                  type = "scatter", mode = "lines+markers",
                  line   = list(color = COLORS$blue, width = 2.5),
                  marker = list(color = COLORS$blue, size = 7),
                  hovertemplate = "Net: $%{y:,.0f}<extra></extra>") |>
        layout(barmode = "relative",
               yaxis = list(tickprefix = "$", tickformat = ",.0f",
                            zerolinecolor = "#555555"),
               hovermode = "x unified")
    )
  })

  # ============================================================
  # TAB 3: P&L SUMMARY TABLE
  # ============================================================
  output$table_pl_summary <- renderDT({
    if (is.null(pl_table) || nrow(pl_table) == 0)
      return(datatable(data.frame(Message = "P&L data not available")))

    key_rows <- c("Total for Income", "Total for Cost of Goods Sold", "Gross Profit",
                  "Total for Expenses", "Net Operating Income", "Net Income")

    # Skip pure header rows (no numeric value AND not a key summary row)
    display_df <- pl_table[!is.na(pl_table$value) | pl_table$label %in% key_rows, ]

    # Format amount column — handle sign separately to avoid formatC flag issue
    display_df$Amount <- ifelse(
      is.na(display_df$value), "",
      ifelse(
        display_df$value < 0,
        paste0("-$", formatC(abs(display_df$value), format = "f", digits = 2, big.mark = ",")),
        paste0( "$", formatC(    display_df$value,  format = "f", digits = 2, big.mark = ","))
      )
    )

    display_df <- display_df[, c("label", "Amount")]
    names(display_df) <- c("Account", "Amount")

    datatable(display_df, rownames = FALSE, class = "compact stripe",
              options = list(
                pageLength = 50, dom = "tp",
                scrollY = "310px", scrollCollapse = TRUE,
                columnDefs = list(list(className = "dt-right", targets = 1))
              )
    ) |>
    formatStyle("Account",
      fontWeight = styleEqual(key_rows, rep("bold", length(key_rows))),
      backgroundColor = styleEqual(key_rows, rep("#F0F4FF", length(key_rows)))
    )
  })

  # ============================================================
  # TAB 3: VENDOR SPEND CHART
  # ============================================================
  output$chart_vendor_spend <- renderPlotly({
    if (is.null(vendor_data) || nrow(vendor_data) == 0)
      return(empty_plot("Vendor data not available"))

    vd <- head(vendor_data[order(-vendor_data$amount), ], 30)
    vd <- vd[order(vd$amount), ]
    n  <- nrow(vd)

    dark_plot(
      plot_ly(vd,
        y = ~factor(vendor, levels = vendor), x = ~amount,
        type = "bar", orientation = "h",
        marker = list(color = colorRampPalette(c(COLORS$blue, COLORS$green))(n)),
        hovertemplate = "<b>%{y}</b><br>$%{x:,.0f}<extra></extra>") |>
        layout(xaxis = list(tickprefix = "$", tickformat = ",.0f"),
               yaxis = list(tickfont = list(color = "#FFFFFF", size = 10),
                            automargin = TRUE),
               margin = list(l = 180, r = 10, t = 20, b = 50))
    )
  })

  # ============================================================
  # TAB 3: VENDOR TABLE (top 15)
  # ============================================================
  output$table_vendor_top <- renderDT({
    if (is.null(vendor_data) || nrow(vendor_data) == 0)
      return(datatable(data.frame(Message = "No data")))
    df <- head(vendor_data, 15)
    df$`Total Spent` <- paste0("$", formatC(df$amount, format = "f", digits = 2, big.mark = ","))
    df <- df[, c("vendor", "Total Spent")]
    names(df)[1] <- "Vendor"
    datatable(df,
              rownames  = FALSE,
              class     = "compact stripe",
              options   = list(
                pageLength = 15, dom = "tp",
                scrollY = "310px", scrollCollapse = TRUE,
                columnDefs = list(list(className = "dt-right", targets = 1))
              )
    )
  })

  # ============================================================
  # TAB 4: COGS & LOGISTICS
  # ============================================================
  output$kpi_cogs_product  <- renderText({ fmt_currency(cogs_metrics$product_cogs) })
  output$kpi_cogs_shipping <- renderText({ fmt_currency(cogs_metrics$shipping_cost) })
  output$kpi_cogs_fees     <- renderText({ fmt_currency(cogs_metrics$processing_fees_total) })
  output$kpi_cogs_total    <- renderText({ fmt_currency(cogs_metrics$total_cogs) })

  output$chart_cogs_breakdown <- renderPlotly({
    if (is.null(cogs_breakdown) || nrow(cogs_breakdown) == 0)
      return(empty_plot("COGS data not available"))

    vd <- cogs_breakdown[order(cogs_breakdown$value), ]
    n  <- nrow(vd)
    bar_colors <- colorRampPalette(c("#ADB5BD", COLORS$red))(n)

    light_plot(
      plot_ly(vd,
        y = ~factor(label_clean, levels = label_clean),
        x = ~value,
        type = "bar", orientation = "h",
        marker = list(color = bar_colors),
        hovertemplate = "<b>%{y}</b><br>$%{x:,.0f}<extra></extra>"
      ) |>
      layout(
        xaxis = list(tickprefix = "$", tickformat = ",.0f"),
        yaxis = list(tickfont = list(color = "#1A1A2E", size = 11),
                     automargin = TRUE),
        margin = list(l = 160, r = 10, t = 20, b = 50)
      )
    )
  })

  output$chart_processing_fees <- renderPlotly({
    if (is.null(processing_fees) || nrow(processing_fees) == 0)
      return(empty_plot("Processing fee data not available"))

    fee_colors <- c(COLORS$blue, COLORS$yellow, COLORS$red)

    light_plot(
      plot_ly(processing_fees,
        labels = ~label_clean,
        values = ~value,
        type   = "pie", hole = 0.52,
        textinfo = "label+percent",
        hovertemplate = "<b>%{label}</b><br>$%{value:,.0f}<extra></extra>",
        marker = list(
          colors = fee_colors[seq_len(nrow(processing_fees))],
          line   = list(color = "#FFFFFF", width = 2)
        )
      ) |>
      layout(showlegend = TRUE,
             legend = list(orientation = "v", x = 1.0, y = 0.5,
                           font = list(size = 11)))
    )
  })

  # ---- Product COGS by supplier ----
  output$chart_cogs_supplier <- renderPlotly({
    if (is.null(cogs_by_supplier) || nrow(cogs_by_supplier) == 0)
      return(empty_plot("No supplier PO data found"))

    # Collapse to one row per vendor (multiple splits → sum)
    df <- cogs_by_supplier |>
      dplyr::group_by(name) |>
      dplyr::summarise(total = sum(total, na.rm = TRUE),
                       n_orders = sum(n_orders), .groups = "drop") |>
      dplyr::filter(!is.na(name) & name != "") |>
      dplyr::arrange(total)

    n  <- nrow(df)
    bar_colors <- colorRampPalette(c(COLORS$blue, "#0D3B8C"))(n)

    light_plot(
      plot_ly(df,
        y = ~factor(name, levels = name),
        x = ~total,
        type = "bar", orientation = "h",
        marker = list(color = bar_colors),
        text  = ~paste0(n_orders, " POs"),
        textposition = "outside",
        hovertemplate = "<b>%{y}</b><br>$%{x:,.0f} · %{text}<extra></extra>"
      ) |>
      layout(
        xaxis = list(tickprefix = "$", tickformat = ",.0f"),
        yaxis = list(tickfont = list(color = "#1A1A2E", size = 11),
                     automargin = TRUE),
        margin = list(l = 10, r = 60, t = 20, b = 50)
      )
    )
  })

  # ---- Monthly COGS purchase order trend ----
  output$chart_cogs_monthly <- renderPlotly({
    if (is.null(cogs_monthly) || nrow(cogs_monthly) == 0)
      return(empty_plot("No monthly COGS data"))

    acct_colors <- c(
      "Product COGS"        = COLORS$red,
      "Shipping & Delivery" = COLORS$blue,
      "Freight IN"          = COLORS$yellow,
      "Cost of Goods Sold"  = "#9B59B6",
      "Packaging"           = COLORS$green,
      "Commissions"         = "#E67700"
    )

    p <- plot_ly()
    for (acct in unique(cogs_monthly$account_clean)) {
      d   <- cogs_monthly[cogs_monthly$account_clean == acct, ]
      col <- if (!is.na(acct_colors[acct])) acct_colors[[acct]] else "#ADB5BD"
      p   <- add_trace(p, x = d$month, y = d$total, name = acct,
                       type = "bar", marker = list(color = col),
                       hovertemplate = paste0("<b>", acct, "</b><br>%{x|%b %Y}: $%{y:,.0f}<extra></extra>"))
    }
    light_plot(layout(p, barmode = "stack",
                      yaxis = list(tickprefix = "$", tickformat = ",.0f"),
                      hovermode = "x unified"))
  })

  output$chart_logistics <- renderPlotly({
    if (is.null(logistics_vendors) || nrow(logistics_vendors) == 0)
      return(empty_plot("No logistics vendor data"))

    vd <- logistics_vendors[order(logistics_vendors$amount), ]
    n  <- nrow(vd)
    bar_colors <- colorRampPalette(c(COLORS$blue, "#0D3B8C"))(n)

    light_plot(
      plot_ly(vd,
        y = ~factor(vendor, levels = vendor),
        x = ~amount,
        type = "bar", orientation = "h",
        marker = list(color = bar_colors),
        hovertemplate = "<b>%{y}</b><br>$%{x:,.0f}<extra></extra>"
      ) |>
      layout(
        xaxis = list(tickprefix = "$", tickformat = ",.0f"),
        yaxis = list(tickfont = list(color = "#1A1A2E", size = 11),
                     automargin = TRUE),
        margin = list(l = 160, r = 10, t = 20, b = 50)
      )
    )
  })

  output$table_logistics <- renderDT({
    if (is.null(logistics_vendors) || nrow(logistics_vendors) == 0)
      return(datatable(data.frame(Message = "No data")))

    df <- logistics_vendors
    df$`Amount Spent` <- paste0("$", formatC(df$amount, format = "f", digits = 2, big.mark = ","))
    df <- df[, c("vendor", "Amount Spent")]
    names(df)[1] <- "Vendor"

    datatable(df, rownames = FALSE, class = "compact stripe",
              options = list(
                pageLength = 20, dom = "tp",
                scrollY = "420px", scrollCollapse = TRUE,
                columnDefs = list(list(className = "dt-right", targets = 1))
              )
    )
  })

  output$table_cogs_detail <- renderDT({
    if (is.null(cogs_txn_detail) || nrow(cogs_txn_detail) == 0)
      return(datatable(data.frame(
        Message = "No COGS purchase order transactions found in Transaction List")))

    df <- cogs_txn_detail
    df$Date <- format(df$Date, "%Y-%m-%d")

    datatable(df, rownames = FALSE, filter = "top",
              class = "compact stripe",
              options = list(
                pageLength = 20, dom = "frtip",
                scrollX = TRUE, scrollY = "420px",
                columnDefs = list(
                  list(className = "dt-right", targets = ncol(df) - 1),
                  list(width = "90px",  targets = 0),
                  list(width = "110px", targets = 1),
                  list(width = "70px",  targets = 2)
                )
              )
    ) |>
    formatCurrency("Amount", currency = "$", digits = 2)
  })

  # ============================================================
  # TAB — INSIGHTS & ALERTS
  # ============================================================

  refund_rates_reactive <- reactive({
    sources_sel <- input$sources
    dr <- input$date_range
    txn <- all_transactions[
      date >= dr[1] & date <= dr[2] & source %in% sources_sel &
      toupper(currency) %in% c("USD", "") &
      !grepl("conversion", type, ignore.case = TRUE)
    ]

    sources_to_check <- c("Braintree", "PayPal HEP", "PayPal HTW", "Stripe")
    lapply(sources_to_check, function(src) {
      rev_amt <- sum(txn[source == src & is_refund == FALSE & amount > 0 &
                         status %in% SUCCESSFUL_STATUSES]$amount, na.rm = TRUE)
      ref_amt <- sum(abs(txn[source == src & is_refund == TRUE]$amount), na.rm = TRUE)
      rate    <- if (rev_amt > 0) round(ref_amt / rev_amt * 100, 1) else 0
      list(source = src, rev = rev_amt, refunds = ref_amt, rate = rate)
    }) |> setNames(sources_to_check)
  })

  output$kpi_alert_net <- renderText({ fmt_currency(pl_metrics$net_income) })

  output$kpi_alert_cogs <- renderText({
    ratio <- if (!is.na(pl_metrics$cogs) && !is.na(pl_metrics$gross_revenue))
               round(pl_metrics$cogs / pl_metrics$gross_revenue * 100, 1) else NA
    ifelse(is.na(ratio), "N/A", paste0(ratio, "%"))
  })

  output$kpi_alert_yoy <- renderText({
    chg <- yoy_q1_chg
    ifelse(is.na(chg), "N/A", paste0(ifelse(chg >= 0, "+", ""), chg, "%"))
  })

  output$kpi_alert_refund <- renderText({
    rates    <- refund_rates_reactive()
    hep_rate <- rates[["PayPal HEP"]]$rate
    paste0(hep_rate, "%")
  })

  output$chart_yoy <- renderPlotly({
    if (is.null(yoy_data) || nrow(yoy_data) == 0) return(empty_plot())
    month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec")
    d2025 <- yoy_data[year == 2025L]
    d2026 <- yoy_data[year == 2026L]
    p <- plot_ly() |>
      add_trace(x = month_labels[d2025$month_num], y = d2025$revenue,
                name = "2025", type = "bar",
                marker = list(color = paste0(COLORS$blue, "CC")),
                hovertemplate = "2025 %{x}: $%{y:,.0f}<extra></extra>") |>
      add_trace(x = month_labels[d2026$month_num], y = d2026$revenue,
                name = "2026", type = "bar",
                marker = list(color = paste0(COLORS$green, "CC")),
                hovertemplate = "2026 %{x}: $%{y:,.0f}<extra></extra>")
    light_plot(layout(p, barmode = "group",
                      yaxis = list(tickprefix = "$", tickformat = ",.0f"),
                      xaxis = list(categoryorder = "array",
                                   categoryarray = month_labels),
                      hovermode = "x unified"))
  })

  output$chart_mom <- renderPlotly({
    rev <- filtered_revenue()
    if (nrow(rev) == 0) return(empty_plot())
    monthly <- rev[is_refund == FALSE & amount > 0,
                   .(revenue = sum(amount, na.rm=TRUE)), by=month][order(month)]
    if (nrow(monthly) < 2) return(empty_plot("Need at least 2 months"))
    monthly[, prev_rev  := shift(revenue, 1)]
    monthly[, mom_pct   := round((revenue - prev_rev) / prev_rev * 100, 1)]
    monthly[, roll3     := frollmean(revenue, 3, align = "right")]
    monthly <- monthly[!is.na(mom_pct)]
    bar_colors <- ifelse(monthly$mom_pct >= 0, COLORS$green, COLORS$red)
    p <- plot_ly() |>
      add_trace(x = ~monthly$month, y = ~monthly$mom_pct,
                type = "bar", marker = list(color = bar_colors),
                name = "MoM %",
                hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>") |>
      add_trace(x = ~monthly$month, y = rep(0, nrow(monthly)),
                type = "scatter", mode = "lines",
                line = list(color = "#ADB5BD", width = 1, dash = "dot"),
                name = "Zero", showlegend = FALSE) |>
      layout(yaxis = list(ticksuffix = "%", zeroline = TRUE,
                          zerolinecolor = "#ADB5BD", zerolinewidth = 1.5),
             hovermode = "x unified")
    light_plot(p)
  })

  output$chart_burn_rate <- renderPlotly({
    if (is.null(txlist_monthly) || nrow(txlist_monthly) == 0)
      return(empty_plot("QuickBooks monthly data not available"))
    df    <- as.data.table(txlist_monthly)
    cols  <- ifelse(df$qb_net >= 0, COLORS$green, COLORS$red)
    p <- plot_ly(df, x = ~month) |>
      add_trace(y = ~qb_revenue, name = "QB Revenue",
                type = "bar", marker = list(color = paste0(COLORS$blue, "88")),
                hovertemplate = "Revenue: $%{y:,.0f}<extra></extra>") |>
      add_trace(y = ~-qb_expenses, name = "QB Expenses",
                type = "bar", marker = list(color = paste0(COLORS$red, "88")),
                customdata = ~qb_expenses,
                hovertemplate = "Expenses: $%{customdata:,.0f}<extra></extra>") |>
      add_trace(y = ~qb_net, name = "Net",
                type = "scatter", mode = "lines+markers",
                line   = list(color = COLORS$blue, width = 2.5),
                marker = list(color = ifelse(df$qb_net >= 0, COLORS$green, COLORS$red),
                              size  = 7),
                hovertemplate = "Net: $%{y:,.0f}<extra></extra>")
    light_plot(layout(p, barmode = "relative",
                      yaxis = list(tickprefix = "$", tickformat = ",.0f",
                                   zerolinecolor = "#6C757D", zerolinewidth = 1.5),
                      hovermode = "x unified"))
  })

  output$chart_refund_rates <- renderPlotly({
    rates <- refund_rates_reactive()
    df <- rbindlist(lapply(rates, function(r) {
      data.table(source = r$source, rate = r$rate, rev = r$rev)
    }))
    df <- df[rev > 0][order(-rate)]
    if (nrow(df) == 0) return(empty_plot("No refund data"))

    thresholds <- list(
      list(type="line", x0=0, x1=1, xref="paper", y0=8, y1=8,
           line=list(color=COLORS$red, width=1.5, dash="dot")),
      list(type="line", x0=0, x1=1, xref="paper", y0=5, y1=5,
           line=list(color=COLORS$yellow, width=1.5, dash="dot"))
    )
    bar_cols <- ifelse(df$rate > 8, COLORS$red,
                ifelse(df$rate > 5, COLORS$yellow, COLORS$green))

    p <- plot_ly(df,
      x = ~source, y = ~rate, type = "bar",
      marker = list(color = bar_cols),
      hovertemplate = "<b>%{x}</b><br>Refund rate: %{y:.1f}%<extra></extra>"
    ) |>
    layout(
      shapes = thresholds,
      yaxis  = list(ticksuffix = "%", title = "Refund Rate"),
      annotations = list(
        list(x=1, y=8.3, xref="paper", yref="y", text="Danger (8%)",
             showarrow=FALSE, font=list(color=COLORS$red, size=10)),
        list(x=1, y=5.3, xref="paper", yref="y", text="Warning (5%)",
             showarrow=FALSE, font=list(color=COLORS$yellow, size=10))
      )
    )
    light_plot(p)
  })

  output$chart_stripe_growth <- renderPlotly({
    rev <- filtered_revenue()
    stripe_m <- rev[source == "Stripe" & is_refund == FALSE & amount > 0,
                    .(revenue = sum(amount, na.rm=TRUE), n = .N), by=month][order(month)]
    if (nrow(stripe_m) == 0)
      return(empty_plot("No Stripe data in selected range"))
    p <- plot_ly(stripe_m, x = ~month) |>
      add_trace(y = ~revenue, type = "bar",
                marker = list(color = paste0(COLORS$green, "BB")),
                name = "Revenue",
                hovertemplate = "%{x|%b %Y}: $%{y:,.0f}<extra></extra>") |>
      add_trace(y = ~n, type = "scatter", mode = "lines+markers",
                yaxis = "y2", name = "Transactions",
                line   = list(color = COLORS$blue, width = 2),
                marker = list(color = COLORS$blue, size = 6),
                hovertemplate = "%{x|%b %Y}: %{y} txns<extra></extra>")
    light_plot(layout(p,
      yaxis  = list(tickprefix = "$", tickformat = ",.0f"),
      yaxis2 = list(overlaying = "y", side = "right",
                    showgrid = FALSE, tickfont = list(color = COLORS$blue)),
      hovermode = "x unified"
    ))
  })

  output$table_large_refunds <- renderDT({
    dr  <- input$date_range
    src <- input$sources
    df  <- as.data.frame(large_refunds_all[
      date >= dr[1] & date <= dr[2] & source %in% src
    ])
    if (nrow(df) == 0)
      return(datatable(data.frame(Message = "No large refunds in selected range")))
    df$amount <- abs(df$amount)
    df$date   <- format(df$date, "%Y-%m-%d")
    names(df) <- c("Date","Source","Amount","Customer","Email","Description","Status","Txn ID")
    datatable(df[, c("Date","Source","Amount","Customer","Status","Description")],
      rownames = FALSE, class = "compact stripe",
      options  = list(
        pageLength = 15, dom = "ftp",
        scrollY = "310px", scrollCollapse = TRUE,
        columnDefs = list(list(className = "dt-right", targets = 2))
      )
    ) |>
    formatCurrency("Amount", currency = "$", digits = 2)
  })

  output$chart_waterfall <- renderPlotly({
    if (is.null(waterfall_data)) return(empty_plot("Waterfall data not available"))
    bar_colors <- ifelse(waterfall_data$measure == "absolute", COLORS$blue,
                  ifelse(waterfall_data$measure == "total",
                         ifelse(waterfall_data$value >= 0, COLORS$green, COLORS$red),
                  ifelse(waterfall_data$value >= 0, paste0(COLORS$green, "CC"),
                                                    paste0(COLORS$red, "CC"))))
    p <- plot_ly(
      type     = "waterfall",
      name     = "P&L",
      orientation = "v",
      x        = ~waterfall_data$label,
      y        = ~waterfall_data$value,
      measure  = ~waterfall_data$measure,
      text     = ~paste0(ifelse(waterfall_data$value >= 0, "+", ""),
                         scales::dollar(waterfall_data$value, accuracy = 1)),
      textposition = "outside",
      increasing   = list(marker = list(color = paste0(COLORS$green, "CC"))),
      decreasing   = list(marker = list(color = paste0(COLORS$red,   "CC"))),
      totals       = list(marker = list(color = COLORS$blue)),
      connector    = list(line = list(color = "#CED4DA")),
      hovertemplate = "<b>%{x}</b><br>$%{y:,.0f}<extra></extra>"
    )
    light_plot(layout(p,
      yaxis  = list(tickprefix = "$", tickformat = ",.0f"),
      xaxis  = list(tickfont = list(size = 11)),
      margin = list(t = 50, b = 80, l = 80, r = 20)
    ))
  })

  # ============================================================
  # TAB 5: RAW DATA TABLE
  # ============================================================
  raw_table_data <- reactive({
    dt <- filtered_txn()
    if (!is.null(input$filter_amount_min) && !is.na(input$filter_amount_min))
      dt <- dt[amount >= input$filter_amount_min]
    if (!is.null(input$filter_amount_max) && !is.na(input$filter_amount_max))
      dt <- dt[amount <= input$filter_amount_max]
    if (length(input$filter_status) > 0)
      dt <- dt[status %in% input$filter_status]

    df <- as.data.frame(dt[, .(date, source, status, type, amount, fee,
                                net_amount, is_refund, currency,
                                description, customer_name, customer_email,
                                transaction_id)])
    names(df) <- c("Date","Source","Status","Type","Amount","Fee","Net",
                   "Refund","Currency","Description","Customer","Email","Txn ID")
    df
  })

  output$table_raw <- renderDT({
    df <- raw_table_data()
    datatable(df, rownames = FALSE, filter = "top",
              class = "dt-dark stripe hover compact",
              options = list(
                pageLength = 25, dom = "frtip",
                scrollX = TRUE, scrollY = "500px",
                columnDefs = list(
                  list(className = "dt-right", targets = c(4,5,6)),
                  list(width = "85px",  targets = 0),
                  list(width = "110px", targets = 1),
                  list(width = "85px",  targets = 2),
                  list(width = "80px",  targets = 4:6)
                )
              )
    ) |>
    formatCurrency(c("Amount","Fee","Net"), currency = "$", digits = 2)
  })

  output$btn_export_csv <- downloadHandler(
    filename = function() paste0("drpawluk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) write.csv(raw_table_data(), file, row.names = FALSE)
  )

  # ============================================================
  # RAW DATA — OUTGOING PAYMENTS TABLE
  # ============================================================
  outgoing_table_data <- reactive({
    req(input$date_range)
    dr <- input$date_range
    dt <- all_transactions[
      date >= dr[1] & date <= dr[2] &
      (balance_impact == "debit" | is_refund == TRUE | amount < 0)
    ]
    df <- as.data.frame(dt[, .(
      date, source, status, type,
      amount, fee, net_amount,
      balance_impact, is_refund,
      description, customer_name, transaction_id
    )])
    names(df) <- c("Date","Source","Status","Type","Amount","Fee","Net",
                   "Balance Impact","Refund","Description","Customer","Txn ID")
    df$Date <- format(df$Date, "%Y-%m-%d")
    df
  })

  output$table_outgoing <- renderDT({
    df <- outgoing_table_data()
    if (nrow(df) == 0)
      return(datatable(data.frame(Message = "No outgoing payments in selected date range"),
                       rownames = FALSE, options = list(dom = "t")))
    datatable(df, rownames = FALSE, filter = "top",
              class = "compact stripe hover",
              options = list(
                pageLength = 25, dom = "frtip",
                scrollX = TRUE, scrollY = "450px",
                columnDefs = list(
                  list(className = "dt-right", targets = c(4, 5, 6)),
                  list(width = "85px",  targets = 0),
                  list(width = "110px", targets = 1),
                  list(width = "85px",  targets = 2)
                )
              )
    ) |>
    formatCurrency(c("Amount", "Fee", "Net"), currency = "$", digits = 2)
  })

  output$btn_export_outgoing_csv <- downloadHandler(
    filename = function() paste0("drpawluk_outgoing_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) write.csv(outgoing_table_data(), file, row.names = FALSE)
  )
}
