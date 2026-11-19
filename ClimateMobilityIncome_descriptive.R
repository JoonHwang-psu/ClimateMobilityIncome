##### Descriptive statistics for the climate, mobility, and income study
##### Run ClimateMobilityIncome_climate.R first.

library(sandwich)
library(boot)

# 1. Loading the data

income <- read.csv("https://raw.githubusercontent.com/JoonHwang-psu/ClimateMobilityIncome/main/shodagor_analysis.csv")
climate <- read.csv("climate_daily.csv")

climate_variables <- c("max_wb", "consec_hot_days_p90", "n_hours_hot_p90",
                       "total_rain", "rain_days_length", "dry_days_length")
income[climate_variables] <- climate[match(income$date, climate$date), climate_variables]
income$date <- as.Date(income$date)
income$year <- as.integer(format(income$date, "%Y"))
income$season <- ifelse(as.integer(format(income$date, "%m")) %in% 4:9, "monsoon", "dry")

fishing <- subset(income, activity == "fishing")
trading <- subset(income, activity == "trading")
monsoon <- subset(income, season == "monsoon")
dry <- subset(income, season == "dry")

fishing_monsoon <- subset(fishing, season == "monsoon")
fishing_dry <- subset(fishing, season == "dry")
trading_monsoon <- subset(trading, season == "monsoon")
trading_dry <- subset(trading, season == "dry")

# 2. Table S1: sample composition and mean (SD)

samples <- list(fishing_monsoon = fishing_monsoon, fishing_dry = fishing_dry,
                fishing_total = fishing, trading_monsoon = trading_monsoon,
                trading_dry = trading_dry, trading_total = trading)

continuous <- c("age", "net_income_bdt", "travel_minutes", "max_wb",
                "consec_hot_days_p90", "n_hours_hot_p90", "total_rain",
                "rain_days_length", "dry_days_length")

Table_S1 <- data.frame(
  panel = c(rep("A", 13), rep("B", 9)),
  category = c("", "", rep("Gender", 2), rep("Location", 4),
               rep("Livelihood diversification", 2), rep("Observation year", 3), rep("", 9)),
  variable = c("Workday observations", "Distinct respondents", "Male", "Female",
               "V1", "V2", "V3", "V4", "Only fishing/trading", "Multiple activities",
               "2017", "2018", "2019", "Age (years)", "Daily net income (BDT)",
               "Travel time (min)", "Max. wet-bulb temp. (deg C)", "Consecutive hot days",
               "Hours of unusual heat per day", "Total daily rainfall (mm)",
               "Wet-spell length (days)", "Dry-spell length (days)"))

for (sample_name in names(samples)) {
  d <- samples[[sample_name]]
  counts <- c(nrow(d), length(unique(d$participant_id)),
              table(factor(d$gender, levels = c("male", "female"))),
              table(factor(d$village, levels = c("V1", "V2", "V3", "V4"))),
              table(factor(d$diversified, levels = 0:1)),
              table(factor(d$year, levels = 2017:2019)))
  percentages <- 100 * counts / nrow(d)
  percentages[1] <- 100 * counts[1] / nrow(income)
  percentages[2] <- 100 * counts[2] / length(unique(income$participant_id))

  means <- sapply(d[continuous], mean)
  standard_deviations <- sapply(d[continuous], sd)
  count_labels <- paste0(formatC(counts, format = "f", digits = 0, big.mark = ","),
                         " (", formatC(percentages, format = "f", digits = 1), ")")
  mean_labels <- paste0(formatC(means, format = "f", digits = 1, big.mark = ","),
                        " (", formatC(standard_deviations, format = "f", digits = 1, big.mark = ","), ")")
  Table_S1[[sample_name]] <- c(count_labels, mean_labels)
}

Table_S1
write.csv(Table_S1, "Table_S1.csv", row.names = FALSE)

# Save the same table as a readable HTML file.
html <- c('<!doctype html><html><head><meta charset="utf-8"><title>Table S1</title>',
          '<style>body{font:15px Arial;margin:35px}table{border-collapse:collapse;width:100%}th,td{border:1px solid #ddd;padding:7px;text-align:right}th:first-child,td:first-child{text-align:left}th{background:#edf2f4}</style></head><body>',
          '<h1>Table S1. Descriptive statistics</h1>')
for (panel in c("A", "B")) {
  d <- Table_S1[Table_S1$panel == panel, 3:9]
  heading <- if (panel == "A") "A. Sample composition: n (%)" else "B. Distributions: mean (SD)"
  html <- c(html, paste0("<h2>", heading, "</h2>"),
            '<table><tr><th>Measure</th><th>Fishing monsoon</th><th>Fishing dry</th><th>Fishing total</th><th>Trading monsoon</th><th>Trading dry</th><th>Trading total</th></tr>')
  for (i in seq_len(nrow(d))) {
    html <- c(html, paste0("<tr><td>", paste(as.character(d[i, ]), collapse = "</td><td>"), "</td></tr>"))
  }
  html <- c(html, "</table>")
}
html <- c(html, '<p>Workday percentages use the full sample of observations; respondent percentages use all participants. Other percentages use the observations in each column. Means and SDs are observation weighted. Participant counts overlap across activities and seasons.</p></body></html>')
writeLines(html, "Table_S1.html")

# 3. Comparing travel time and income across activities and seasons
# Standard errors are clustered by participant; t tests use G - 1 degrees of freedom.

compare_means <- function(d, outcome, group, group0, group1, dimension, stratum) {
  d$comparison_group <- factor(d[[group]], levels = c(group0, group1))
  model <- lm(reformulate("comparison_group", response = outcome), data = d)
  covariance <- vcovCL(model, cluster = as.character(d$participant_id), type = "HC1", cadjust = TRUE)
  difference <- unname(coef(model)[2])
  se <- sqrt(covariance[2, 2])
  respondents <- length(unique(d$participant_id))
  df <- respondents - 1
  x0 <- d[d$comparison_group == group0, outcome]
  x1 <- d[d$comparison_group == group1, outcome]
  data.frame(dimension, stratum, outcome, group0, group1,
             n0 = length(x0), n1 = length(x1), respondents,
             mean0 = mean(x0), mean1 = mean(x1), difference_group1_minus_group0 = difference,
             cluster_robust_se = se, df,
             ci_low = difference - qt(.975, df) * se, ci_high = difference + qt(.975, df) * se,
             t = difference / se, p_unadjusted = 2 * pt(-abs(difference / se), df))
}

activity_comparisons <- rbind(
  compare_means(monsoon, "travel_minutes", "activity", "fishing", "trading", "activity", "monsoon"),
  compare_means(monsoon, "net_income_bdt", "activity", "fishing", "trading", "activity", "monsoon"),
  compare_means(dry, "travel_minutes", "activity", "fishing", "trading", "activity", "dry"),
  compare_means(dry, "net_income_bdt", "activity", "fishing", "trading", "activity", "dry"),
  compare_means(fishing, "travel_minutes", "season", "dry", "monsoon", "season", "fishing"),
  compare_means(fishing, "net_income_bdt", "season", "dry", "monsoon", "season", "fishing"),
  compare_means(trading, "travel_minutes", "season", "dry", "monsoon", "season", "trading"),
  compare_means(trading, "net_income_bdt", "season", "dry", "monsoon", "season", "trading"))
activity_comparisons$p_holm <- p.adjust(activity_comparisons$p_unadjusted, method = "holm")
activity_comparisons
write.csv(activity_comparisons, "descriptive_activity_comparisons.csv", row.names = FALSE)

# 4. Comparing climatic exposures between fishing and trading days
# The six exposures are compared overall, in the monsoon, and in the dry season.
# Resample 14-day calendar blocks within each season, using 9,999 replicates.

calendar <- seq(min(income$date), max(income$date), by = "day")
date_index <- match(income$date, calendar)
calendar_season <- ifelse(as.integer(format(calendar, "%m")) %in% 4:9, "monsoon", "dry")
cells <- c("fishing_monsoon", "fishing_dry", "trading_monsoon", "trading_dry")
column_names <- unlist(lapply(cells, function(x) paste(x, c("n", climate_variables), sep = "__")))
daily_totals <- matrix(0, nrow = length(calendar), ncol = length(column_names),
                       dimnames = list(as.character(calendar), column_names))

for (cell in cells) {
  activity <- if (startsWith(cell, "fishing")) "fishing" else "trading"
  season <- if (endsWith(cell, "monsoon")) "monsoon" else "dry"
  rows <- which(income$activity == activity & income$season == season)
  daily_totals[, paste(cell, "n", sep = "__")] <- tabulate(date_index[rows], nbins = length(calendar))
  sums <- rowsum(as.matrix(income[rows, climate_variables]), group = date_index[rows], reorder = FALSE)
  daily_totals[as.integer(rownames(sums)), paste(cell, climate_variables, sep = "__")] <- sums
}

contexts <- c("overall", "monsoon", "dry")
context_cells <- list(overall = list(fishing = cells[1:2], trading = cells[3:4]),
                      monsoon = list(fishing = cells[1], trading = cells[3]),
                      dry = list(fishing = cells[2], trading = cells[4]))

group_mean <- function(totals, cell_names, variable) {
  exposure_sum <- rowSums(totals[, paste(cell_names, variable, sep = "__"), drop = FALSE])
  observations <- rowSums(totals[, paste(cell_names, "n", sep = "__"), drop = FALSE])
  exposure_sum / observations
}

climate_differences <- function(totals) {
  differences <- matrix(NA_real_, nrow(totals), 18)
  column <- 0
  for (context in contexts) for (variable in climate_variables) {
    column <- column + 1
    groups <- context_cells[[context]]
    differences[, column] <- group_mean(totals, groups$trading, variable) -
                              group_mean(totals, groups$fishing, variable)
  }
  differences
}

totals <- matrix(colSums(daily_totals), nrow = 1, dimnames = list(NULL, column_names))
observed <- as.numeric(climate_differences(totals))
season_segment <- cumsum(c(TRUE, calendar_season[-1] != calendar_season[-length(calendar)]))
segments <- split(seq_along(calendar), season_segment)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(890716)
bootstrap_totals <- matrix(0, nrow = 9999, ncol = ncol(daily_totals),
                           dimnames = list(NULL, column_names))
for (segment in segments) {
  resampled <- tsboot(daily_totals[segment, , drop = FALSE], statistic = colSums,
                      R = 9999, l = 14, sim = "fixed", endcorr = TRUE,
                      n.sim = length(segment), orig.t = TRUE)
  bootstrap_totals <- bootstrap_totals + resampled$t
}

bootstrap_differences <- climate_differences(bootstrap_totals)
bootstrap_se <- apply(bootstrap_differences, 2, sd)
climate_comparisons <- expand.grid(exposure = climate_variables, context = contexts,
                                   stringsAsFactors = FALSE)
climate_comparisons$fishing_mean <- NA_real_
climate_comparisons$trading_mean <- NA_real_
for (i in seq_len(nrow(climate_comparisons))) {
  groups <- context_cells[[climate_comparisons$context[i]]]
  variable <- climate_comparisons$exposure[i]
  climate_comparisons$fishing_mean[i] <- group_mean(totals, groups$fishing, variable)
  climate_comparisons$trading_mean[i] <- group_mean(totals, groups$trading, variable)
}

climate_comparisons$trading_minus_fishing <- observed
climate_comparisons$block_bootstrap_se <- bootstrap_se
climate_comparisons$z <- observed / bootstrap_se
climate_comparisons$p_unadjusted <- 2 * pnorm(-abs(climate_comparisons$z))
climate_comparisons$p_holm <- p.adjust(climate_comparisons$p_unadjusted, method = "holm")
climate_comparisons$ci_low <- observed - qnorm(.975) * bootstrap_se
climate_comparisons$ci_high <- observed + qnorm(.975) * bootstrap_se
climate_comparisons$bootstrap_percentile_low <- apply(bootstrap_differences, 2, quantile, probs = .025)
climate_comparisons$bootstrap_percentile_high <- apply(bootstrap_differences, 2, quantile, probs = .975)
climate_comparisons
write.csv(climate_comparisons, "descriptive_climate_comparisons.csv", row.names = FALSE)
