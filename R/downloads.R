#' @export
download_logs <- function(pkg = "ggcharts",
                          from = Sys.Date() - 9,
                          to = Sys.Date() - 2,
                          cache = TRUE) {
  if (cache) {
    file <- paste0(pkg, "_cache.rds")
    path <- file.path(system.file(package = "ggchartsdownloads"), file)
    if (file.exists(path)) {
      old_downloads <- readRDS(path)
    }
  }

  dates <- as.Date(from:to, origin = "1970-01-01")
  if (exists("old_downloads")) {
    new_dates <- dates[!dates %in% as.Date(old_downloads$date)]
  } else {
    new_dates <- dates
  }

  if (length(new_dates)) {
    n_cores <- min(length(new_dates), parallel::detectCores())
    cl <- parallel::makeCluster(n_cores)
    downloads <- parallel::parLapply(cl, new_dates, function(date) {
      base_url <- "http://cran-logs.rstudio.com/2020/"
      file <- paste0(as.character(date), ".csv.gz")
      url <- paste0(base_url, file)
      utils::download.file(url, file)
      downloads <- data.table::fread(file)
      file.remove(file)
      downloads[package == "ggcharts"]
    })
    parallel::stopCluster(cl)

    downloads <- data.table::rbindlist(downloads)
    if (exists("old_downloads")) {
      downloads <- rbind(old_downloads, downloads)
    }
  } else {
    downloads <- old_downloads
  }

  if (cache) {
    saveRDS(downloads, path)
  }

  downloads[as.Date(date) %in% dates]
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
