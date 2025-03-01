library(tidyverse)
library(packageRank)
library(plotly)
library(lubridate)

sorted_packages <- read_csv("generated_data/all_packages.csv")
top_packages <- c(sorted_packages$package[2:31]) #exclude ggplot2
cran_packages <- c(sorted_packages$package[!is.na(sorted_packages$downloads)])
  
cd_time_series <- function(pkg) {
  cranDownloads(packages = pkg, to = 2025)$cranlogs.data
}

dc_top_packages <- map_dfr(top_packages, cd_time_series)
dc_cran_packages <- map_dfr(cran_packages, cd_time_series)

#plot for time series of download count across dates
dc_history_plot <- ggplot(dc_top_packages, aes(x = date, y = count, color = package)) + 
  geom_smooth(se = FALSE, linewidth = .75) +
  labs(title = "Downloads Across Time", x = "Date", y = "Download Count")

dc_history_plotly <- ggplotly(dc_history_plot)
dc_history_plotly

#plot for max download count dates
max_date_df <- dc_top_packages %>%
  group_by(package) %>%
  slice(which.max(count)) %>%
  select(package, max_date = date, max_downloads = count)

max_dc_plot <- ggplot(max_date_df, aes(x = max_date, y = max_downloads, color = package)) +
  geom_point() +
  labs(title = "Max Download Dates by Package", x = "Max Download Date", y = "Download Count")

max_dc_plotly <- ggplotly(max_dc_plot)
max_dc_plotly

#plot for average download counts per date
average_dc <- dc_cran_packages %>%
  group_by(date) %>%
  summarize(average_dc = mean(count, na.rm = TRUE))

average_dc_plot <- ggplot(average_dc, aes(x = date, y = average_dc)) + 
  geom_point(size = 0.05) +
  labs(title = "Average Downloads by Date", x = "Date", y = "Download Count")

average_dc_plotly <- ggplotly(average_dc_plot)
average_dc_plotly

#create seasonal plots by month and day of week
seasonal_data <- dc_top_packages %>%
  mutate(month = month(date, label = TRUE),
         day_of_week = wday(date, label = TRUE))

dc_monthly <- ggplot(seasonal_data, aes(x = month, y = count)) +
  geom_boxplot() +
  ylim(0, 5000) +
  labs(title = "Months in Downloads", x = "Month", y = "Download Count")
dc_monthly

dc_daily <- ggplot(seasonal_data, aes(x = day_of_week, y = count)) +
  geom_boxplot() +
  ylim(0, 5000) +
  labs(title = "Days of the Week in Downloads", x = "Day of Week", y = "Download Count")
dc_daily
