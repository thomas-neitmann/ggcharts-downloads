#' @export
plot_daily_downloads <- function(daily_downloads, color = "steelblue") {
  ggplot(daily_downloads, aes(date, N)) +
    geom_line(color = color, size = 1.5) +
    scale_x_date(date_labels = "%b %d") +
    ylim(0, NA) +
    theme_classic(base_size = 18) +
    labs(
      x = NULL,
      y = NULL,
      title = "Daily Downloads"
    )
}

#' @export
plot_cumulative_downloads <- function(daily_downloads, color = "steelblue") {
  max_downloads <- daily_downloads[cumulative_N == max(cumulative_N)]

  ggplot() +
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
      vjust = -.75,
      size = 7
    ) +
    scale_x_date(date_labels = "%b %d") +
    scale_y_continuous(expand = expansion(c(.05, .2))) +
    theme_classic(base_size = 18) +
    labs(
      x = NULL,
      y = NULL,
      title = "Cumulative Downloads"
    )
}

#' @export
hist_daily_downloads <- function(daily_downloads, color = "steelblue") {
  ggplot(daily_downloads, aes(N)) +
    geom_histogram(bins = 7, fill = color, color = "black") +
    scalesextra::scale_y_tight() +
    theme_classic(base_size = 18) +
    labs(
      x = NULL,
      y = NULL,
      title = "Distribution of Daily Downloads"
    )
}

#' @export
plot_downloads_by_country <- function(downloads_by_country, color = "steelblue") {
  ggcharts::bar_chart(
    data = downloads_by_country[!is.na(country)],
    x = country,
    y = N,
    bar_color = color,
    limit = 10
  ) +
    geom_text(aes(label = N), hjust = -.2, size = 6) +
    labs(
      x = NULL,
      title = "Countries with Highest Total\nNumber of Downloads"
    ) +
    scale_y_continuous(expand = expand_scale(c(0, .15))) +
    theme_classic(base_size = 18) +
    ggeasy::easy_remove_x_axis()
}
