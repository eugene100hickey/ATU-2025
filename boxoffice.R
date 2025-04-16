numeric_cleaner <- function(x) {
  # Removes everything but numbers and the negative sign and the period,
  # then makes numeric.
  suppressWarnings(as.numeric(gsub("[^[:digit:]\\.\\-]", "", x)))
}

dates <- Sys.Date()-7

url_start <- "https://www.the-numbers.com/box-office-chart/daily/"
url_dates <- gsub("-", "/", dates)

results <- vector("list", length = length(dates))

useragent <- paste0(
  "Mozilla/5.0 (compatible; a bot using the R boxoffice",
  " package; https://github.com/jacobkap/boxoffice/)")

page <- httr::GET(paste0(url_start, url_dates[1]),
          httr::user_agent(useragent))

page <- httr::content(page, "parsed", encoding = "UTF-8")

page <- rvest::html_nodes(page, paste0("#box_office_daily_table"))
page <- rvest::html_table(page)
page <- page[[1]]

page <- page[, names(page) != ""]
# Removes the % change from last week to stay consistent in old column order.
page$`%LW` <- NULL

names(page) <- c("movie",
                 "distributor",
                 "gross",
                 "percent_change",
                 "theaters",
                 "per_theater",
                 "total_gross",
                 "days")

# Fixes strange ... when runs out of space to ... that is readable to R.
page$movie       <- iconv(page$movie, "latin1", "ASCII", sub = "")
page$distributor <- iconv(page$distributor, "latin1", "ASCII", sub = "")


# Makes numeric and removes $ and , values from columns -------------------
page[, 3:ncol(page)]  <- sapply(page[3:ncol(page)], numeric_cleaner)
