# =============================================================================
# ETW2001 Assignment 3 - Delivery Promise Performance Dashboard
# End user: Regional Head of E-Commerce Operations
#
# Web version: runs in the browser via shinylive (GitHub Pages) and also
# locally in RStudio. Reads lines.rds (built from data.csv by
# ../prepare_data.R) from the same folder as this file.
# =============================================================================

# Install missing packages when running locally (not needed in the browser,
# where shinylive provides them)
if (R.version$os != "emscripten") {
  required_pkgs <- c("shiny", "bslib", "data.table", "plotly", "DT", "jsonlite")
  missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                         logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
}

library(shiny)
library(bslib)
library(data.table)
library(plotly)
library(DT)
library(jsonlite)

# -----------------------------------------------------------------------------
# 1. DATA PREPARATION (runs once at start-up)
# -----------------------------------------------------------------------------
# Order-item (line) level table, pre-built from data.csv by prepare_data.R
lines <- as.data.table(readRDS("lines.rds"))

lines[, cancelled := delivery == "Shipping canceled"]
lines[, delay := days_real - days_sched]
lines[, late := as.integer(delay > 0 & !cancelled)]

# Order level table: one row per order (shipping is decided per order)
orders <- lines[, .(
  order_date = first(order_date), market = first(market),
  region = first(region), country = first(country),
  segment = first(segment), ship_mode = first(ship_mode),
  days_real = first(days_real), days_sched = first(days_sched),
  delay = first(delay), late = first(late), cancelled = first(cancelled),
  order_status = first(order_status),
  items = .N, sales = sum(sales), profit = sum(profit),
  loss_making = as.integer(sum(profit) < 0)
), by = order_id]

delay_levels <- c("2 days early", "1 day early", "On time",
                  "1 day late", "2 days late", "3-4 days late")
bucket_delay <- function(d) {
  factor(fifelse(d <= -2, "2 days early",
         fifelse(d == -1, "1 day early",
         fifelse(d == 0, "On time",
         fifelse(d == 1, "1 day late",
         fifelse(d == 2, "2 days late", "3-4 days late"))))),
         levels = delay_levels)
}
orders[, delay_band := bucket_delay(delay)]

mode_levels <- c("Same Day", "First Class", "Second Class", "Standard Class")
orders[, ship_mode := factor(ship_mode, levels = mode_levels)]
lines[,  ship_mode := factor(ship_mode, levels = mode_levels)]

date_min <- min(orders$order_date); date_max <- max(orders$order_date)
markets  <- sort(unique(orders$market))
segments <- sort(unique(orders$segment))

# Colours: one accent for "problem" (late), muted greys/blues for context
col_late   <- "#C8102E"   # late / problem
col_ok     <- "#2F6690"   # on time / early
col_neutral <- "#9AA5B1"
col_bench  <- "#3A3A3A"
mode_cols  <- c("Same Day" = "#7FB7BE", "First Class" = "#C8102E",
                "Second Class" = "#E07A5F", "Standard Class" = "#2F6690")
delay_cols <- c("2 days early" = "#1F4E79", "1 day early" = "#5B8DB8",
                "On time" = "#C9D6DF", "1 day late" = "#F4A582",
                "2 days late" = "#D6604D", "3-4 days late" = "#8E0C24")

pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
money <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", formatC(x / 1e6, format = "f", digits = 2), "M"),
  ifelse(abs(x) >= 1e3, paste0("$", formatC(x / 1e3, format = "f", digits = 1), "K"),
         paste0("$", formatC(x, format = "f", digits = 0))))
}
plot_cfg <- function(p) {
  config(p, displaylogo = FALSE,
         modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                    "toggleSpikelines", "hoverCompareCartesian"))
}

# -----------------------------------------------------------------------------
# 2. USER INTERFACE
# -----------------------------------------------------------------------------
theme <- bs_theme(version = 5, bootswatch = "flatly",
                  primary = "#2F6690", base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
                  "font-size-base" = "0.92rem")

sidebar_filters <- sidebar(
  width = 270, open = "desktop",
  h6("Global filters", class = "text-uppercase text-muted mb-0"),
  helpText("Apply to every chart on every page."),
  dateRangeInput("dates", "Order date", start = date_min, end = date_max,
                 min = date_min, max = date_max, format = "dd M yyyy"),
  checkboxGroupInput("markets", "Market", choices = markets, selected = markets),
  selectInput("segment", "Customer segment", choices = c("All", segments)),
  checkboxGroupInput("modes", "Shipping mode", choices = mode_levels,
                     selected = mode_levels),
  hr(),
  h6("Drill-down focus", class = "text-uppercase text-muted mb-1"),
  uiOutput("focus_badge"),
  actionButton("reset", "Reset all filters", icon = icon("rotate-left"),
               class = "btn-outline-secondary btn-sm w-100 mt-2")
)

kpi_row <- layout_column_wrap(
  width = 1/5, fill = FALSE,
  value_box("Orders shipped", textOutput("kpi_orders"),
            theme = "light", p(textOutput("kpi_sales", inline = TRUE))),
  value_box("Late-delivery rate", textOutput("kpi_late"),
            theme = value_box_theme(bg = "#FBE9EC", fg = col_late),
            p(textOutput("kpi_late_ctx", inline = TRUE))),
  value_box("Avg delay of late orders", textOutput("kpi_delay"),
            theme = "light", p("days beyond promise")),
  value_box("Profit margin", textOutput("kpi_margin"),
            theme = "light", p(textOutput("kpi_profit", inline = TRUE))),
  value_box("Loss-making orders", textOutput("kpi_loss"),
            theme = "light", p("orders with negative profit"))
)

page_overview <- nav_panel(
  "1 · Overview", icon = icon("gauge"),
  kpi_row,
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        span(strong("V1. Is performance changing over time?")),
        span(class = "d-flex gap-2",
          selectInput("trend_metric", NULL, width = "170px",
                      choices = c("Late-delivery rate" = "late",
                                  "Orders" = "orders", "Sales" = "sales",
                                  "Profit margin" = "margin")),
          selectInput("trend_grain", NULL, width = "110px",
                      choices = c("Month" = "month", "Quarter" = "quarter")))),
      radioButtons("trend_split", "Split lines by", inline = TRUE,
                   choices = c("None" = "none", "Shipping mode" = "ship_mode", "Market" = "market")),
      plotlyOutput("v1_trend", height = "345px"),
      card_footer(class = "small text-muted",
        "Periods with fewer than 30 orders are left blank. Note: markets are recorded in different periods (try 'Split by Market'), so time trends partly reflect a change of market mix.")
    ),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        strong("V2. Where are deliveries late?"),
        selectInput("geo_level", NULL, width = "150px",
                    choices = c("Order region" = "region", "Market" = "market",
                                "Country (top 20)" = "country"))),
      plotlyOutput("v2_geo", height = "380px"),
      card_footer(class = "small text-muted",
        "Click a dot to focus the whole dashboard on that location. Dot size = orders; line = company average.")
    )
  )
)

page_rootcause <- nav_panel(
  "2 · Root cause", icon = icon("magnifying-glass-chart"),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        strong("V3. Promise vs reality: delivery days against the promised schedule"),
        radioButtons("v3_scale", NULL, inline = TRUE,
                     choices = c("Share %" = "pct", "Orders" = "n"))),
      plotlyOutput("v3_delay", height = "300px"),
      card_footer(class = "small text-muted",
        "Bars show actual minus promised days. Blue = early, grey = on time, red = late. Click a bar to focus on that shipping mode.")
    ),
    card(
      card_header(strong("Scenario: what if promises matched capability?")),
      p(class = "small text-muted mb-1",
        "Change the promised (scheduled) days per mode to see the late-delivery rate the same shipments would have had."),
      layout_column_wrap(width = 1/2, fill = FALSE,
        numericInput("sc_same",  "Same Day",       0, min = 0, max = 5, step = 1),
        numericInput("sc_first", "First Class",    1, min = 0, max = 5, step = 1),
        numericInput("sc_second","Second Class",   2, min = 0, max = 6, step = 1),
        numericInput("sc_std",   "Standard Class", 4, min = 0, max = 7, step = 1)
      ),
      tableOutput("scenario_tbl"),
      uiOutput("scenario_msg")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(full_screen = TRUE,
      card_header(strong("V4. Late-delivery rate by region and shipping mode")),
      plotlyOutput("v4_heat", height = "430px"),
      card_footer(class = "small text-muted",
        "Each cell = % of orders delivered late. Click a cell to list its orders on the right.")
    ),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        uiOutput("drill_title", inline = TRUE),
        downloadButton("drill_dl", "CSV", class = "btn-sm btn-outline-primary")),
      DTOutput("drill_tbl")
    )
  )
)

page_commercial <- nav_panel(
  "3 · Commercial impact", icon = icon("sack-dollar"),
  layout_columns(
    col_widths = c(8, 4),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        strong("V5. How much revenue rides on late deliveries?"),
        span(class = "d-flex gap-2",
          radioButtons("v5_measure", NULL, inline = TRUE,
                       choices = c("Sales" = "sales", "Profit" = "profit")),
          radioButtons("v5_view", NULL, inline = TRUE,
                       choices = c("Values" = "abs", "Share %" = "pct")))),
      sliderInput("v5_topn", "Categories shown (ranked by selected measure)",
                  min = 5, max = 25, value = 12, step = 1, width = "100%"),
      plotlyOutput("v5_cat", height = "420px")
    ),
    card(
      card_header(strong("Exposure summary")),
      uiOutput("exposure_text"),
      hr(),
      p(class = "small text-muted",
        "Exposure = value of order lines whose delivery arrived after the promised date. ",
        "Cancelled shipments are excluded from delivery measures.")
    )
  )
)

page_about <- nav_panel(
  "Guide", icon = icon("circle-info"),
  card(
    card_header(strong("How to use this dashboard")),
    tags$ol(
      tags$li(strong("Overview: "), "check headline KPIs and the trend, then see whether lateness is concentrated in particular places."),
      tags$li(strong("Root cause: "), "compare actual against promised delivery days by shipping mode, test revised promises in the scenario panel, and click heatmap cells to pull the underlying orders."),
      tags$li(strong("Commercial impact: "), "size the sales and profit attached to late deliveries by product category."),
      tags$li("Sidebar filters apply everywhere. Clicking a location (V2) or a shipping mode (V3) adds a drill-down focus; use the badge or Reset to clear it.")
    ),
    p(class = "small text-muted",
      "Data: DataCo supply-chain dataset, Jan 2015 - Jan 2018. ",
      "Delivery measures use orders (one shipping decision per order); category measures use order lines.")
  )
)

ui <- page_navbar(
  title = span(icon("truck-fast"), " Delivery Promise Performance"),
  header = tags$head(tags$style(HTML("table.dataTable { font-size: 0.78rem; } table.dataTable td { white-space: nowrap; }"))),
  theme = theme, sidebar = sidebar_filters, fillable = FALSE, bg = "#1F3A5F",
  page_overview, page_rootcause, page_commercial, page_about,
  nav_spacer(),
  nav_item(span(class = "small", style = "color:#C9D6DF", icon("user-tie"), " Regional Head of E-Commerce Operations"))
)

# -----------------------------------------------------------------------------
# 3. SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  focus <- reactiveValues(level = NULL, value = NULL, mode = NULL)
  heat_cell <- reactiveVal(NULL)

  observeEvent(input$reset, {
    updateDateRangeInput(session, "dates", start = date_min, end = date_max)
    updateCheckboxGroupInput(session, "markets", selected = markets)
    updateSelectInput(session, "segment", selected = "All")
    updateCheckboxGroupInput(session, "modes", selected = mode_levels)
    focus$level <- NULL; focus$value <- NULL; focus$mode <- NULL
    heat_cell(NULL)
  })
  observeEvent(input$clear_geo,  { focus$level <- NULL; focus$value <- NULL })
  observeEvent(input$clear_mode, { focus$mode <- NULL })

  # --- base filtered data (sidebar only) --------------------------------------
  base_filter <- function(dt) {
    req(input$dates, length(input$markets) > 0, length(input$modes) > 0)
    out <- dt[order_date >= input$dates[1] & order_date <= input$dates[2] &
              market %in% input$markets & ship_mode %in% input$modes]
    if (input$segment != "All") out <- out[segment == input$segment]
    out
  }
  apply_focus <- function(dt, geo = TRUE, mode = TRUE) {
    if (geo && !is.null(focus$level)) dt <- dt[get(focus$level) == focus$value]
    if (mode && !is.null(focus$mode)) dt <- dt[ship_mode == focus$mode]
    dt
  }

  ord_base  <- reactive(base_filter(orders))
  line_base <- reactive(base_filter(lines))
  ord_f     <- reactive(apply_focus(ord_base()))
  line_f    <- reactive(apply_focus(line_base()))
  shipped   <- reactive(ord_f()[cancelled == FALSE])

  # company benchmark for the same sidebar filters, no drill-down
  bench_late <- reactive(ord_base()[cancelled == FALSE, mean(late)])

  output$focus_badge <- renderUI({
    tags_list <- list()
    if (!is.null(focus$level))
      tags_list <- c(tags_list, list(span(class = "badge bg-primary me-1",
        paste0(focus$value, " "), actionLink("clear_geo", icon("xmark"), style = "color:white"))))
    if (!is.null(focus$mode))
      tags_list <- c(tags_list, list(span(class = "badge bg-danger me-1",
        paste0(focus$mode, " "), actionLink("clear_mode", icon("xmark"), style = "color:white"))))
    if (length(tags_list) == 0) return(p(class = "small text-muted mb-0", "None. Click V2 or V3 to focus."))
    div(tags_list)
  })

  # --- KPIs ---------------------------------------------------------------------
  output$kpi_orders <- renderText(format(nrow(shipped()), big.mark = ","))
  output$kpi_sales  <- renderText(paste(money(sum(ord_f()$sales)), "sales"))
  output$kpi_late   <- renderText(pct(mean(shipped()$late)))
  output$kpi_late_ctx <- renderText({
    diff <- mean(shipped()$late) - bench_late()
    if (is.null(focus$level) && is.null(focus$mode)) "of shipped orders arrived late"
    else paste0(ifelse(diff >= 0, "+", ""), round(100 * diff, 1), " pts vs filtered average")
  })
  output$kpi_delay  <- renderText({
    s <- shipped()[late == 1]; if (nrow(s) == 0) "-" else formatC(mean(s$delay), format = "f", digits = 2)
  })
  output$kpi_margin <- renderText(pct(sum(ord_f()$profit) / sum(ord_f()$sales)))
  output$kpi_profit <- renderText(paste(money(sum(ord_f()$profit)), "profit"))
  output$kpi_loss   <- renderText(pct(mean(ord_f()$loss_making)))

  # --- V1 trend -----------------------------------------------------------------
  output$v1_trend <- renderPlotly({
    d <- copy(ord_f())
    d[, period := if (input$trend_grain == "month")
                    as.Date(format(order_date, "%Y-%m-01"))
                  else as.Date(paste0(format(order_date, "%Y-"),
                         sprintf("%02d", (as.integer(format(order_date, "%m")) - 1) %/% 3 * 3 + 1), "-01"))]
    split <- input$trend_split
    grp <- if (split == "none") "period" else c("period", split)
    agg <- d[, .(late = mean(late[!cancelled]), orders = .N,
                 sales = sum(sales), margin = sum(profit) / sum(sales)), by = grp]
    # complete the grid so empty periods show as gaps rather than joined lines
    all_p <- sort(unique(agg$period))
    if (split == "none") agg <- merge(data.table(period = all_p), agg, all.x = TRUE)
    else {
      grid <- CJ(period = all_p, g = unique(agg[[split]])); setnames(grid, "g", split)
      agg <- merge(grid, agg, by = c("period", split), all.x = TRUE)
    }
    agg[is.na(orders), orders := 0]
    agg[orders < 30, c("late", "margin") := NA_real_]
    if (input$trend_metric %in% c("orders", "sales")) agg[orders == 0, c("orders", "sales") := NA_real_]
    setorder(agg, period)
    m <- input$trend_metric
    ylab <- c(late = "Late-delivery rate", orders = "Orders", sales = "Sales ($)",
              margin = "Profit margin")[m]
    fmt <- if (m %in% c("late", "margin")) ".0%" else ",.0f"
    if (split != "none") {
      cols <- if (split == "ship_mode") mode_cols else
        setNames(c("#8C6D31", "#2F6690", "#C8102E", "#3A7D44", "#7B4B94"), markets)
      p <- plot_ly(agg, x = ~period, y = agg[[m]], color = agg[[split]], colors = cols,
                   connectgaps = FALSE,
                   type = "scatter", mode = "lines+markers", marker = list(size = 4),
                   hovertemplate = paste0("%{x|%b %Y}<br>%{y:", fmt, "}<extra>%{fullData.name}</extra>"))
    } else {
      p <- plot_ly(agg, x = ~period, y = agg[[m]], type = "scatter", mode = "lines+markers",
                   connectgaps = FALSE, line = list(color = if (m == "late") col_late else col_ok, width = 2.5),
                   marker = list(size = 5, color = if (m == "late") col_late else col_ok),
                   name = ylab, hovertemplate = paste0("%{x|%b %Y}<br>%{y:", fmt, "}<extra></extra>"))
      if (m == "late") {
        avg <- mean(shipped()$late)
        p <- p %>% add_lines(x = range(agg$period), y = c(avg, avg), inherit = FALSE,
                             line = list(dash = "dot", color = col_bench, width = 1),
                             name = "Period average", hoverinfo = "skip")
      }
    }
    yr <- if (m == "late") list(tickformat = ".0%", range = c(0, 1), title = ylab)
          else if (m == "margin") list(tickformat = ".0%", title = ylab)
          else list(title = ylab, rangemode = "tozero")
    p %>% layout(xaxis = list(title = ""), yaxis = yr, legend = list(orientation = "h", y = -0.15),
                 margin = list(t = 10), hovermode = "x unified") %>% plot_cfg()
  })

  # --- V2 geography (dot plot) --------------------------------------------------
  geo_data <- reactive({
    lvl <- input$geo_level
    d <- apply_focus(ord_base(), geo = FALSE)[cancelled == FALSE]
    agg <- d[, .(late = mean(late), orders = .N, sales = sum(sales)), by = c(lvl)]
    setnames(agg, lvl, "loc")
    if (lvl == "country") agg <- agg[order(-orders)][1:min(20, .N)]
    agg[order(late)]
  })
  output$v2_geo <- renderPlotly({
    agg <- geo_data(); avg <- bench_late()
    if (!is.null(focus$mode)) avg <- apply_focus(ord_base(), geo = FALSE)[cancelled == FALSE, mean(late)]
    agg[, loc := factor(loc, levels = loc)]
    agg[, colr := ifelse(late > avg, col_late, col_ok)]
    sel <- if (!is.null(focus$value) && focus$level == input$geo_level) focus$value else NA
    agg[, lw := ifelse(loc == sel & !is.na(sel), 3, 0)]
    plot_ly(agg, x = ~late, y = ~loc, type = "scatter", mode = "markers", source = "geo",
            customdata = ~as.character(loc),
            marker = list(size = ~pmax(6, 26 * sqrt(orders / max(orders))), color = ~colr,
                          opacity = 0.85, line = list(width = ~lw, color = "#000")),
            text = ~paste0("<b>", loc, "</b><br>Late: ", pct(late), "<br>Orders: ",
                           format(orders, big.mark = ","), "<br>Sales: ", money(sales)),
            hoverinfo = "text") %>%
      layout(showlegend = FALSE,
             shapes = list(list(type = "line", x0 = avg, x1 = avg, y0 = 0, y1 = 1, yref = "paper",
                                line = list(dash = "dot", color = col_bench, width = 1))),
             xaxis = list(title = "Late-delivery rate", tickformat = ".0%",
                          range = c(max(0, min(agg$late, avg) - 0.05), min(1, max(agg$late, avg) + 0.05))),
             yaxis = list(title = "", tickfont = list(size = 10)),
             annotations = list(list(x = avg, y = 1.02, yref = "paper", text = paste("avg", pct(avg)),
                                     showarrow = FALSE, font = list(size = 10, color = col_bench))),
             margin = list(t = 20, l = 10)) %>% plot_cfg() %>% event_register("plotly_click")
  })
  click_of <- function(src) {
    x <- input[[paste0("plotly_click-", src)]]
    if (is.null(x)) NULL else jsonlite::fromJSON(x)
  }
  observeEvent(input[["plotly_click-geo"]], {
    v <- click_of("geo")$customdata[1]
    if (is.null(v)) return()
    if (!is.null(focus$value) && identical(focus$value, v)) { focus$level <- NULL; focus$value <- NULL }
    else { focus$level <- input$geo_level; focus$value <- v }
  })

  # --- V3 promise vs reality (diverging stacked bar) ---------------------------
  output$v3_delay <- renderPlotly({
    d <- apply_focus(ord_base(), mode = FALSE)[cancelled == FALSE]
    agg <- d[, .N, by = .(ship_mode, delay_band)]
    agg <- merge(CJ(ship_mode = factor(mode_levels, levels = mode_levels),
                    delay_band = factor(delay_levels, levels = delay_levels)),
                 agg, all.x = TRUE)
    agg[is.na(N), N := 0]
    agg[, share := N / sum(N), by = ship_mode]
    tot <- d[, .(promise = first(days_sched), actual = mean(days_real), late = mean(late), n = .N), by = ship_mode]
    agg <- merge(agg, tot, by = "ship_mode", all.x = TRUE)
    agg[, lab := paste0(ship_mode, "<br><span style='font-size:10px'>promise ",
                        promise, "d · actual ", formatC(actual, format = "f", digits = 1), "d</span>")]
    agg <- agg[!is.na(promise)]
    agg[, lab := factor(lab, levels = unique(agg[order(-as.integer(ship_mode))]$lab))]
    # diverging: early bands negative, on-time centred, late positive
    agg[, val := if (input$v3_scale == "pct") share else N]
    agg[delay_band %in% c("2 days early", "1 day early"), val := -val]
    agg[delay_band == "On time", val := val]  # on time drawn to the left of zero (half) and right (half)
    half <- agg[delay_band == "On time"]
    agg <- rbind(agg[delay_band != "On time"],
                 copy(half)[, val := -val / 2], copy(half)[, val := val / 2])
    agg[, delay_band := factor(delay_band, levels = c("On time", "1 day early", "2 days early",
                                                      "1 day late", "2 days late", "3-4 days late"))]
    setorder(agg, delay_band)
    fmt <- if (input$v3_scale == "pct") ".0%" else ",.0f"
    p <- plot_ly(source = "mode")
    shown <- character(0)
    for (b in levels(agg$delay_band)) {
      sub <- agg[delay_band == b]
      if (nrow(sub) == 0) next
      for (part in split(sub, sign(sub$val) >= 0)) {
        p <- p %>% add_bars(data = part, x = ~val, y = ~lab, orientation = "h", name = b,
                            legendgroup = b, showlegend = !(b %in% shown), legendrank = match(b, delay_levels),
                            marker = list(color = delay_cols[[b]]),
                            customdata = ~as.character(ship_mode),
                            text = ~paste0(ship_mode, " · ", b, ": ",
                                           ifelse(b == "On time", pct(share), pct(share)), " (",
                                           format(N, big.mark = ","), " orders)"),
                            hoverinfo = "text", textposition = "none")
        shown <- c(shown, b)
      }
    }
    p %>% layout(barmode = "relative",
                 xaxis = list(title = if (input$v3_scale == "pct") "← early   share of orders   late →"
                                      else "← early   orders   late →",
                              tickformat = fmt, zeroline = TRUE, zerolinewidth = 2, zerolinecolor = "#333"),
                 yaxis = list(title = ""), legend = list(orientation = "h", y = -0.25, traceorder = "normal"),
                 margin = list(t = 10)) %>% plot_cfg() %>% event_register("plotly_click")
  })
  observeEvent(input[["plotly_click-mode"]], {
    v <- click_of("mode")$customdata[1]
    if (is.null(v)) return()
    focus$mode <- if (identical(focus$mode, v)) NULL else v
  })

  # --- Scenario ---------------------------------------------------------------
  scenario <- reactive({
    d <- apply_focus(ord_base(), mode = FALSE)[cancelled == FALSE]
    newp <- c("Same Day" = input$sc_same, "First Class" = input$sc_first,
              "Second Class" = input$sc_second, "Standard Class" = input$sc_std)
    newp[is.na(newp)] <- 0
    d[, new_promise := newp[as.character(ship_mode)]]
    d[, .(Orders = .N, `Current promise` = first(days_sched),
          `Current late` = mean(late), `New promise` = first(new_promise),
          `New late` = mean(days_real > new_promise)), by = ship_mode][order(ship_mode)]
  })
  output$scenario_tbl <- renderTable({
    s <- copy(scenario())
    tot <- s[, .(ship_mode = "All modes", Orders = sum(Orders), `Current promise` = NA, `Current late` = sum(`Current late` * Orders) / sum(Orders),
                 `New promise` = NA, `New late` = sum(`New late` * Orders) / sum(Orders))]
    s <- rbind(s, tot, fill = TRUE)
    s[, `Current late` := pct(`Current late`)][, `New late` := pct(`New late`)]
    s[, `Current promise` := as.character(`Current promise`)][, `New promise` := as.character(`New promise`)]
    s[, Orders := format(Orders, big.mark = ",")]
    setnames(s, "ship_mode", "Mode")
    s
  }, striped = TRUE, spacing = "xs", width = "100%", na = "")
  output$scenario_msg <- renderUI({
    s <- scenario()
    cur <- sum(s$`Current late` * s$Orders) / sum(s$Orders)
    new <- sum(s$`New late` * s$Orders) / sum(s$Orders)
    p(class = "small", strong(pct(cur)), " → ", strong(pct(new), style = paste0("color:", ifelse(new < cur, col_ok, col_late))),
      " of shipped orders late. A longer promise lowers lateness but may reduce the appeal of premium shipping.")
  })

  # --- V4 heatmap -----------------------------------------------------------------
  output$v4_heat <- renderPlotly({
    d <- apply_focus(ord_base(), geo = FALSE, mode = FALSE)[cancelled == FALSE]
    agg <- d[, .(late = mean(late), n = .N), by = .(region, ship_mode)]
    reg_order <- d[, .(late = mean(late)), by = region][order(late)]$region
    agg[, region := factor(region, levels = reg_order)]
    z <- dcast(agg, region ~ ship_mode, value.var = "late")
    nn <- dcast(agg, region ~ ship_mode, value.var = "n")
    zm <- as.matrix(z[, -1]); nm <- as.matrix(nn[, -1])
    txt <- matrix(paste0("<b>", z$region, "</b> · ", rep(colnames(zm), each = nrow(zm)),
                         "<br>Late: ", pct(zm), "<br>Orders: ", format(nm, big.mark = ",")),
                  nrow = nrow(zm))
    plot_ly(x = colnames(zm), y = as.character(z$region), z = zm, type = "heatmap", source = "heat",
            colorscale = list(c(0, "#F7F7F7"), c(0.4, "#FDDBC7"), c(0.7, "#D6604D"), c(1, "#67001F")),
            zmin = 0, zmax = 1, text = txt, hoverinfo = "text",
            colorbar = list(title = "Late", tickformat = ".0%", len = 0.8)) %>%
      layout(annotations = lapply(seq_along(zm), function(i) {
        r <- (i - 1) %% nrow(zm) + 1; c <- (i - 1) %/% nrow(zm) + 1
        list(x = colnames(zm)[c], y = as.character(z$region)[r], showarrow = FALSE,
             text = ifelse(is.na(zm[r, c]), "", round(100 * zm[r, c])),
             font = list(size = 10, color = ifelse(!is.na(zm[r, c]) & zm[r, c] > 0.6, "white", "#333")))
      })) %>%
      layout(xaxis = list(title = "", side = "top"), yaxis = list(title = "", tickfont = list(size = 10)),
             margin = list(t = 30)) %>% plot_cfg() %>% event_register("plotly_click")
  })
  observeEvent(input[["plotly_click-heat"]], {
    e <- click_of("heat")
    heat_cell(list(region = e$y[1], mode = e$x[1]))
  })

  drill_data <- reactive({
    hc <- heat_cell()
    d <- apply_focus(ord_base(), geo = FALSE, mode = FALSE)
    if (!is.null(hc)) d <- d[region == hc$region & ship_mode == hc$mode]
    else d <- d[late == 1]
    d[order(-delay, -sales), .(`Order ID` = order_id, Date = format(order_date, "%d %b %y"), Country = country,
                               Mode = ship_mode, Promised = days_sched,
                               Actual = days_real, `Days late` = pmax(delay, 0),
                               Status = order_status, Sales = round(sales, 2), Profit = round(profit, 2))]
  })
  output$drill_title <- renderUI({
    hc <- heat_cell()
    if (is.null(hc)) strong("Order drill-down: all late orders (click a heatmap cell)")
    else strong(paste0("Order drill-down: ", hc$region, " · ", hc$mode))
  })
  output$drill_tbl <- renderDT({
    datatable(drill_data(), rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE, dom = "ftip",
                             language = list(search = "Search orders:")),
              class = "compact stripe") %>%
      formatCurrency(c("Sales", "Profit"), digits = 0) %>%
      formatStyle("Days late", color = styleInterval(0, c("#333", col_late)), fontWeight = "bold")
  })
  output$drill_dl <- downloadHandler(
    filename = function() paste0("orders_drilldown_", Sys.Date(), ".csv"),
    content = function(f) fwrite(drill_data(), f)
  )

  # --- V5 category exposure ---------------------------------------------------
  v5_data <- reactive({
    d <- line_f()[cancelled == FALSE]
    m <- input$v5_measure
    agg <- d[, .(total = sum(get(m)), late_val = sum(get(m)[late == 1]),
                 ontime_val = sum(get(m)[late == 0]), lines = .N), by = category]
    agg <- agg[order(-total)][1:min(input$v5_topn, .N)]
    agg[order(total)]
  })
  output$v5_cat <- renderPlotly({
    agg <- copy(v5_data()); m <- input$v5_measure
    agg[, category := factor(category, levels = category)]
    lab <- if (m == "sales") "Sales" else "Profit"
    if (input$v5_view == "pct") {
      agg[, `:=`(a = ontime_val / total, b = late_val / total)]; fmt <- ".0%"
    } else { agg[, `:=`(a = ontime_val, b = late_val)]; fmt <- "$,.0f" }
    plot_ly(agg, y = ~category, x = ~a, type = "bar", orientation = "h",
            name = "Delivered early / on time", marker = list(color = col_ok),
            text = ~paste0(category, "<br>On time/early: ", money(ontime_val), " (", pct(ontime_val / total), ")"),
            hoverinfo = "text", textposition = "none") %>%
      add_bars(x = ~b, name = "Delivered late", marker = list(color = col_late),
               text = ~paste0(category, "<br>Late: ", money(late_val), " (", pct(late_val / total), ")"),
               hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "stack", xaxis = list(title = paste(lab, if (input$v5_view == "pct") "share" else "($)"),
                                             tickformat = fmt),
             yaxis = list(title = ""), legend = list(orientation = "h", x = 0, y = 1.1, traceorder = "reversed"),
             margin = list(t = 45)) %>% plot_cfg()
  })
  output$exposure_text <- renderUI({
    d <- line_f()[cancelled == FALSE]
    late_sales <- sum(d$sales[d$late == 1]); late_profit <- sum(d$profit[d$late == 1])
    by_mode <- d[late == 1, .(s = sum(sales)), by = ship_mode][order(-s)]
    tagList(
      h3(money(late_sales), style = paste0("color:", col_late, ";margin-bottom:0")),
      p(class = "text-muted", paste0("sales delivered late (", pct(late_sales / sum(d$sales)), " of shipped sales)")),
      h4(money(late_profit), style = "margin-bottom:0"),
      p(class = "text-muted", "profit attached to late deliveries"),
      p(strong("Late sales by shipping mode")),
      tags$ul(lapply(seq_len(nrow(by_mode)), function(i)
        tags$li(paste0(by_mode$ship_mode[i], ": ", money(by_mode$s[i]), " (", pct(by_mode$s[i] / late_sales), ")"))))
    )
  })
}

shinyApp(ui, server)
