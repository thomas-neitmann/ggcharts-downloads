#' @export
download_logs <- function(pkg = "ggcharts", from = Sys.Date() - 9, to = Sys.Date() - 2) {
  dates <- as.Date(from:to, origin = "1970-01-01")

  cl <- parallel::makeCluster(parallel::detectCores())
  downloads <- parallel::parLapply(cl, dates, function(date) {
    base_url <- "http://cran-logs.rstudio.com/2020/"
    file <- paste0(as.character(date), ".csv.gz")
    url <- paste0(base_url, file)
    utils::download.file(url, file)
    downloads <- data.table::fread(file)
    file.remove(file)
    downloads[package == "ggcharts"]
  })
  parallel::stopCluster(cl)

  data.table::rbindlist(downloads)
}

#' @export
compute_daily_downloads <- function(downloads) {
  daily_downloads <- downloads[, .N, by = date]
  daily_downloads[, date := as.Date(date)]
  daily_downloads[, cumulative_N := cumsum(N)]
  daily_downloads
}

#' @export
compute_downloads_by_country <- function(downloads) {
  downloads_by_country <- downloads[, .N, by = country]
  data.table::setnames(downloads_by_country, "country", "code")
  downloads_by_country[, country := countrycode::countrycode(code, "iso2c", "country.name")]
  downloads_by_country
}
