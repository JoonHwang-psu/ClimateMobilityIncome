##### Climate data and Figure

library(ggplot2)


# 1. Loading climate data

wetbulb <- read.csv("https://raw.githubusercontent.com/JoonHwang-psu/ClimateMobilityIncome/main/climate_wetbulb_3hour.csv")
rain <- read.csv("https://raw.githubusercontent.com/JoonHwang-psu/ClimateMobilityIncome/main/climate_rainfall_3hour.csv")

wetbulb$date <- as.Date(wetbulb$date)
rain$date <- as.Date(rain$date)


# 2. Monthly heat thresholds
# Historical period: January 1964 through February 2017.

baseline <- wetbulb[wetbulb$date >= as.Date("1964-01-01") &
                     wetbulb$date < as.Date("2017-03-01"), ]
baseline$Month <- as.integer(format(baseline$date, "%m"))

thresholds <- data.frame(Month = 1:12,
                         observed_timestamps = NA_integer_,
                         source_records = NA_integer_,
                         p90_wb = NA_real_)

for (month in 1:12) {
  x <- baseline[baseline$Month == month, ]
  thresholds$observed_timestamps[month] <- nrow(x)
  thresholds$source_records[month] <- sum(x$source_records)
  thresholds$p90_wb[month] <- quantile(rep(x$wetbulb_c, x$source_records),
                                      probs = .90, type = 7)
}

write.csv(thresholds, "climate_thresholds.csv", row.names = FALSE)


# 3. Daily climate measures
# Study period: March 2017 through September 2019.

wetbulb <- wetbulb[wetbulb$date >= as.Date("2017-03-01") &
                    wetbulb$date <= as.Date("2019-09-30"), ]
wetbulb$Month <- as.integer(format(wetbulb$date, "%m"))
wetbulb$threshold <- thresholds$p90_wb[wetbulb$Month]
wetbulb$hot_hours <- 3 * (wetbulb$wetbulb_c > wetbulb$threshold)

daily_max <- aggregate(wetbulb_c ~ date, data = wetbulb, FUN = max)
names(daily_max)[2] <- "max_wb"

daily_hours <- aggregate(hot_hours ~ date, data = wetbulb, FUN = sum)
names(daily_hours)[2] <- "n_hours_hot_p90"

daily_rain <- aggregate(rainfall_mm ~ date, data = rain, FUN = sum)
names(daily_rain)[2] <- "total_rain"

daily <- merge(daily_max, daily_hours, by = "date")
daily <- merge(daily, daily_rain, by = "date")
daily <- daily[order(daily$date), ]
daily$Month <- as.integer(format(daily$date, "%m"))

## 3.1. Consecutive hot, rainy, and dry days
# Each run includes the current day and continues across calendar months.

daily$consec_hot_days_p90 <- 0L
daily$rain_days_length <- 0L
daily$dry_days_length <- 0L

hot_run <- 0L
wet_run <- 0L
dry_run <- 0L

for (i in 1:nrow(daily)) {
  hot_run <- if (daily$n_hours_hot_p90[i] > 0) hot_run + 1L else 0L
  wet_run <- if (daily$total_rain[i] > 0) wet_run + 1L else 0L
  dry_run <- if (daily$total_rain[i] == 0) dry_run + 1L else 0L

  daily$consec_hot_days_p90[i] <- hot_run
  daily$rain_days_length[i] <- wet_run
  daily$dry_days_length[i] <- dry_run
}

daily <- daily[, c("date", "Month", "max_wb", "n_hours_hot_p90",
                   "consec_hot_days_p90", "total_rain", "rain_days_length",
                   "dry_days_length")]
write.csv(daily, "climate_daily.csv", row.names = FALSE)


# 4. Monthly medians and interquartile ranges
# Each calendar day contributes once; months are pooled across study years.

measures <- c(max_wb = "max_wb", hot = "consec_hot_days_p90",
              hours = "n_hours_hot_p90", rainfall = "total_rain",
              wet = "rain_days_length", dry = "dry_days_length")

monthly <- data.frame(Month_num = 1:12, Month_lab = month.abb)

for (measure in names(measures)) {
  monthly[[paste0(measure, "_med")]] <- NA_real_
  monthly[[paste0(measure, "_q25")]] <- NA_real_
  monthly[[paste0(measure, "_q75")]] <- NA_real_

  for (month in 1:12) {
    x <- daily[daily$Month == month, measures[[measure]]]
    monthly[month, paste0(measure, c("_med", "_q25", "_q75"))] <-
      quantile(x, probs = c(.50, .25, .75), type = 7)
  }
}

monthly$p90_wb <- thresholds$p90_wb
write.csv(monthly, "climate_monthly.csv", row.names = FALSE)


# 5. Figure 2

## Common formatting for the six panels
climate_theme <- theme_classic(base_size = 12) +
  theme(panel.grid.major.y = element_line(linewidth = .25, colour = "grey85"),
        plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(size = 9))

month_axis <- scale_x_continuous(breaks = 1:12, labels = month.abb,
                                limits = c(.7, 12.3), expand = c(0, 0))

## A. Maximum wet-bulb temperature
p1 <- ggplot(monthly, aes(Month_num, max_wb_med)) +
  annotate("rect", xmin = 3.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#DCEEF8", alpha = .7) +
  geom_errorbar(aes(ymin = max_wb_q25, ymax = max_wb_q75), width = .15, linewidth = .5) +
  geom_line(linewidth = .7) + geom_point(size = 1.8) +
  geom_line(aes(y = p90_wb), colour = "red", linewidth = .6) +
  month_axis + climate_theme +
  labs(x = NULL, y = expression(degree*C), title = "A) Daily maximum wet-bulb temperature") +
  theme(axis.text.x = element_blank())

## B. Consecutive hot days
p2 <- ggplot(monthly, aes(Month_num, hot_med)) +
  annotate("rect", xmin = 3.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#DCEEF8", alpha = .7) +
  geom_errorbar(aes(ymin = hot_q25, ymax = hot_q75), width = .15, linewidth = .5) +
  geom_line(linewidth = .7) + geom_point(size = 1.8) +
  month_axis + climate_theme +
  labs(x = NULL, y = "Consecutive days", title = "B) Consecutive hot days") +
  theme(axis.text.x = element_blank())

## C. Hours of unusual heat
p3 <- ggplot(monthly, aes(Month_num, hours_med)) +
  annotate("rect", xmin = 3.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#DCEEF8", alpha = .7) +
  geom_errorbar(aes(ymin = hours_q25, ymax = hours_q75), width = .15, linewidth = .5) +
  geom_line(linewidth = .7) + geom_point(size = 1.8) +
  month_axis + climate_theme +
  labs(x = NULL, y = "Hours/day", title = "C) Hours above the heat threshold per day")

## D. Total daily rainfall
p4 <- ggplot(monthly, aes(Month_num, rainfall_med)) +
  annotate("rect", xmin = 3.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#DCEEF8", alpha = .7) +
  geom_errorbar(aes(ymin = rainfall_q25, ymax = rainfall_q75), width = .15, linewidth = .5) +
  geom_line(linewidth = .7) + geom_point(size = 1.8) +
  month_axis + climate_theme +
  labs(x = NULL, y = "Rainfall (mm)", title = "D) Total daily rainfall") +
  theme(axis.text.x = element_blank())

## E. Wet spells
p5 <- ggplot(monthly, aes(Month_num, wet_med)) +
  annotate("rect", xmin = 3.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#DCEEF8", alpha = .7) +
  geom_errorbar(aes(ymin = wet_q25, ymax = wet_q75), width = .15, linewidth = .5) +
  geom_line(linewidth = .7) + geom_point(size = 1.8) +
  month_axis + climate_theme +
  labs(x = NULL, y = "Consecutive days", title = "E) Wet spells") +
  theme(axis.text.x = element_blank())

## F. Dry spells
p6 <- ggplot(monthly, aes(Month_num, dry_med)) +
  annotate("rect", xmin = 3.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#DCEEF8", alpha = .7) +
  geom_errorbar(aes(ymin = dry_q25, ymax = dry_q75), width = .15, linewidth = .5) +
  geom_line(linewidth = .7) + geom_point(size = 1.8) +
  month_axis + climate_theme +
  labs(x = NULL, y = "Consecutive days", title = "F) Dry spells")

Figure2 <- patchwork::wrap_plots(p1, p4, p2, p5, p3, p6, ncol = 2, nrow = 3)

ggsave("Figure2.png", Figure2, width = 10, height = 8.42, units = "in", dpi = 300, bg = "white")
ggsave("Figure2.pdf", Figure2, width = 10, height = 8.42, units = "in", bg = "white")
