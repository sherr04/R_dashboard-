# =============================================================================
# prepare_data.R - builds the small data file used by the web dashboard
#
# The raw data.csv (96 MB) is too large to load in a browser, so this script
# keeps only the 14 columns the dashboard uses and saves them, compressed, as
# app/lines.rds (a few MB). Run it from the repository root whenever data.csv
# changes:   Rscript prepare_data.R
# =============================================================================
library(data.table)

raw <- fread("data.csv", encoding = "Latin-1", showProgress = FALSE)

lines <- raw[, .(
  order_id     = `Order Id`,
  order_date   = as.Date(as.POSIXct(`order date (DateOrders)`,
                                    format = "%m/%d/%Y %H:%M", tz = "UTC")),
  market       = Market,
  region       = trimws(gsub("\\s+", " ", `Order Region`)),
  country      = `Order Country`,
  segment      = `Customer Segment`,
  ship_mode    = `Shipping Mode`,
  category     = `Category Name`,
  days_real    = `Days for shipping (real)`,
  days_sched   = `Days for shipment (scheduled)`,
  delivery     = `Delivery Status`,
  order_status = `Order Status`,
  sales        = round(`Order Item Total`, 2),
  profit       = round(`Order Profit Per Order`, 2)
)]

dir.create("app", showWarnings = FALSE)
saveRDS(as.data.frame(lines), "app/lines.rds", compress = "gzip")
cat("Saved app/lines.rds:", format(file.size("app/lines.rds"), big.mark = ","), "bytes,",
    format(nrow(lines), big.mark = ","), "rows\n")
