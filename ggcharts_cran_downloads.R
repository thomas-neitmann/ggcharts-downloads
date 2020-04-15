library(ggplot2)
library(patchwork)
Sys.setlocale("LC_TIME", "English")

start_date <- as.Date("2020-03-26")
end_date <- Sys.Date() - 2
dates <- as.Date(start_date:end_date, origin = "1970-01-01")

cl <- parallel::makeCluster(parallel::detectCores())
downloads <- parallel::parLapply(cl, dates, function(date) {
  base_url <- "http://cran-logs.rstudio.com/2020/"
  file <- paste0(as.character(date), ".csv.gz")
  url <- paste0(base_url, file)
  download.file(url, file)
  downloads <- data.table::fread(file)
  file.remove(file)
  downloads[package == "ggcharts"]
})
parallel::stopCluster(cl)

downloads <- data.table::rbindlist(downloads)

daily_downloads <- downloads[, .N, by = date]
daily_downloads[, date := as.Date(date)]
daily_downloads[, cumulative_N := cumsum(N)]
max_downloads <- daily_downloads[cumulative_N == max(cumulative_N)]

downloads_by_country <- downloads[, .N, by = country]
downloads_by_country[, country := countrycode::countrycode(country, "iso2c", "country.name")]

theme_set(theme_classic(base_size = 18))
color <- "steelblue"
p1 <- ggplot(daily_downloads, aes(date, N)) +
  geom_line(color = color, size = 1.5) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  ylim(0, NA) +
  labs(
    x = NULL,
    y = NULL,
    title = "Daily Downloads"
  )

p2 <- ggplot() +
  geom_line(
    data = daily_downloads,
    mapping = aes(date, cumulative_N),
    color = color,
    size = 1.5
  ) +
  geom_point(
    data = max_downloads,
    mapping = aes(date, cumulative_N),
    size = 5,
    color = color
  ) +
  geom_text(
    data = max_downloads,
    mapping = aes(date, cumulative_N, label = cumulative_N),
    vjust = 2,
    size = 7
  ) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  labs(
    x = NULL,
    y = NULL,
    title = "Cumulative Downloads"
  )

p3 <- ggplot(daily_downloads, aes(N)) +
  geom_histogram(bins = 7, fill = color, color = "black") +
  scalesextra::scale_y_tight() +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of Daily Downloads"
  )

p4 <- ggcharts::bar_chart(
  data = downloads_by_country[!is.na(country)],
  x = country,
  y = N,
  bar_color = color,
  limit = 10
) +
  geom_text(aes(label = N), hjust = -.2, size = 6) +
  labs(
    x = NULL,
    title = "Countries with Highest Total Number of Downloads"
  ) +
  scalesextra::scale_y_tight(limits = c(0, 250)) +
  theme_classic(base_size = 18) +
  ggeasy::easy_remove_x_axis()


f <- function(date) format(date, "%b %d, %Y")
p1 + p2 + p3 + p4 +
  plot_annotation(
    title = "ggcharts is on the Rise",
    subtitle = "A Summary of Downloads from the RStudio CRAN Mirror",
    caption = glue::glue("Source: RStudio CRAN Logs ({f(start_date)} to {f(end_date)})"),
    theme = theme_classic(base_size = 24) + theme(plot.title = element_text(face = "bold"), plot.caption = element_text(size = 14))
  )
