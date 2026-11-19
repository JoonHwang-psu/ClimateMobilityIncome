##### Climate, daily mobility, and income: supplementary analyses
# Manuscript 3.2 and Supplementary Information 1.4
# Run ClimateMobilityIncome_climate.R first; all files are in the same folder.

library(lme4)

# 1. Setting up the data

income <- read.csv("https://raw.githubusercontent.com/JoonHwang-psu/ClimateMobilityIncome/main/shodagor_analysis.csv", stringsAsFactors = FALSE)

climate <- read.csv("climate_daily.csv", stringsAsFactors = FALSE)
climate_variables <- c("consec_hot_days_p90", "rain_days_length", "dry_days_length",
                       "n_hours_hot_p90", "max_wb", "total_rain")
income <- cbind(income, climate[match(income$date, climate$date), climate_variables])
income$log_travel_time <- log1p(income$travel_minutes)
income$year <- as.integer(substr(income$date, 1, 4))
income$month <- as.integer(substr(income$date, 6, 7))
income$season <- ifelse(income$month %in% 4:9, "monsoon", "dry")
income$participant_id <- factor(income$participant_id)
income$gender <- factor(income$gender, levels = c("male", "female"))
income$village <- factor(income$village, levels = c("V1", "V2", "V3", "V4"))

fishing_monsoon <- droplevels(subset(income, activity == "fishing" & season == "monsoon"))
fishing_dry <- droplevels(subset(income, activity == "fishing" & season == "dry"))
trading_monsoon <- droplevels(subset(income, activity == "trading" & season == "monsoon"))
trading_dry <- droplevels(subset(income, activity == "trading" & season == "dry"))

# Starting random-number states reproduce the manuscript's 1,000 simulations.
simulation_seeds <- readRDS(gzcon(url("https://raw.githubusercontent.com/JoonHwang-psu/ClimateMobilityIncome/main/simulation_seeds.rds", open = "rb")))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# Fishing samples for village-specific analyses
fishing_V1_monsoon <- droplevels(subset(fishing_monsoon, village == "V1"))
fishing_V1_dry <- droplevels(subset(fishing_dry, village == "V1"))
fishing_V2_monsoon <- droplevels(subset(fishing_monsoon, village == "V2"))
fishing_V2_dry <- droplevels(subset(fishing_dry, village == "V2"))
fishing_V3_V4_monsoon <- droplevels(subset(fishing_monsoon, village %in% c("V3", "V4")))
fishing_V3_V4_dry <- droplevels(subset(fishing_dry, village %in% c("V3", "V4")))

## 2. Hours of unusual heat: fishing, monsoon season

med_fishing_monsoon_hot_hours <- lmer(
  log_travel_time ~ n_hours_hot_p90 + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(med_fishing_monsoon_hot_hours)

out_fishing_monsoon_hot_hours <- lmer(
  net_income_bdt ~ n_hours_hot_p90 + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(out_fishing_monsoon_hot_hours)

.Random.seed <- simulation_seeds[["pooled_fishing__monsoon__n_hours_hot_p90"]]
mediation_fishing_monsoon_hot_hours <- mediation::mediate(
  med_fishing_monsoon_hot_hours, out_fishing_monsoon_hot_hours,
  treat = "n_hours_hot_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_monsoon_hot_hours)

## 3. Hours of unusual heat: fishing, dry season

med_fishing_dry_hot_hours <- lmer(
  log_travel_time ~ n_hours_hot_p90 + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(med_fishing_dry_hot_hours)

out_fishing_dry_hot_hours <- lmer(
  net_income_bdt ~ n_hours_hot_p90 + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(out_fishing_dry_hot_hours)

.Random.seed <- simulation_seeds[["pooled_fishing__dry__n_hours_hot_p90"]]
mediation_fishing_dry_hot_hours <- mediation::mediate(
  med_fishing_dry_hot_hours, out_fishing_dry_hot_hours,
  treat = "n_hours_hot_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_dry_hot_hours)

## 4. Maximum wet-bulb temperature: fishing, monsoon season

med_fishing_monsoon_max_wetbulb <- lmer(
  log_travel_time ~ max_wb + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(med_fishing_monsoon_max_wetbulb)

out_fishing_monsoon_max_wetbulb <- lmer(
  net_income_bdt ~ max_wb + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(out_fishing_monsoon_max_wetbulb)

.Random.seed <- simulation_seeds[["pooled_fishing__monsoon__max_wb"]]
mediation_fishing_monsoon_max_wetbulb <- mediation::mediate(
  med_fishing_monsoon_max_wetbulb, out_fishing_monsoon_max_wetbulb,
  treat = "max_wb", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_monsoon_max_wetbulb)

## 5. Maximum wet-bulb temperature: fishing, dry season

med_fishing_dry_max_wetbulb <- lmer(
  log_travel_time ~ max_wb + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(med_fishing_dry_max_wetbulb)

out_fishing_dry_max_wetbulb <- lmer(
  net_income_bdt ~ max_wb + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(out_fishing_dry_max_wetbulb)

.Random.seed <- simulation_seeds[["pooled_fishing__dry__max_wb"]]
mediation_fishing_dry_max_wetbulb <- mediation::mediate(
  med_fishing_dry_max_wetbulb, out_fishing_dry_max_wetbulb,
  treat = "max_wb", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_dry_max_wetbulb)

## 6. Daily rainfall: fishing, monsoon season

med_fishing_monsoon_rainfall <- lmer(
  log_travel_time ~ total_rain + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(med_fishing_monsoon_rainfall)

out_fishing_monsoon_rainfall <- lmer(
  net_income_bdt ~ total_rain + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(out_fishing_monsoon_rainfall)

.Random.seed <- simulation_seeds[["pooled_fishing__monsoon__total_rain"]]
mediation_fishing_monsoon_rainfall <- mediation::mediate(
  med_fishing_monsoon_rainfall, out_fishing_monsoon_rainfall,
  treat = "total_rain", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_monsoon_rainfall)

## 7. Daily rainfall: fishing, dry season

med_fishing_dry_rainfall <- lmer(
  log_travel_time ~ total_rain + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(med_fishing_dry_rainfall)

out_fishing_dry_rainfall <- lmer(
  net_income_bdt ~ total_rain + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(out_fishing_dry_rainfall)

.Random.seed <- simulation_seeds[["pooled_fishing__dry__total_rain"]]
mediation_fishing_dry_rainfall <- mediation::mediate(
  med_fishing_dry_rainfall, out_fishing_dry_rainfall,
  treat = "total_rain", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_dry_rainfall)

## 8. Hours of unusual heat: trading, monsoon season

med_trading_monsoon_hot_hours <- lmer(
  log_travel_time ~ n_hours_hot_p90 + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(med_trading_monsoon_hot_hours)

out_trading_monsoon_hot_hours <- lmer(
  net_income_bdt ~ n_hours_hot_p90 + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(out_trading_monsoon_hot_hours)

.Random.seed <- simulation_seeds[["pooled_trading__monsoon__n_hours_hot_p90"]]
mediation_trading_monsoon_hot_hours <- mediation::mediate(
  med_trading_monsoon_hot_hours, out_trading_monsoon_hot_hours,
  treat = "n_hours_hot_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_monsoon_hot_hours)

## 9. Hours of unusual heat: trading, dry season

med_trading_dry_hot_hours <- lmer(
  log_travel_time ~ n_hours_hot_p90 + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(med_trading_dry_hot_hours)

out_trading_dry_hot_hours <- lmer(
  net_income_bdt ~ n_hours_hot_p90 + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(out_trading_dry_hot_hours)

.Random.seed <- simulation_seeds[["pooled_trading__dry__n_hours_hot_p90"]]
mediation_trading_dry_hot_hours <- mediation::mediate(
  med_trading_dry_hot_hours, out_trading_dry_hot_hours,
  treat = "n_hours_hot_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_dry_hot_hours)

## 10. Maximum wet-bulb temperature: trading, monsoon season

med_trading_monsoon_max_wetbulb <- lmer(
  log_travel_time ~ max_wb + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(med_trading_monsoon_max_wetbulb)

out_trading_monsoon_max_wetbulb <- lmer(
  net_income_bdt ~ max_wb + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(out_trading_monsoon_max_wetbulb)

.Random.seed <- simulation_seeds[["pooled_trading__monsoon__max_wb"]]
mediation_trading_monsoon_max_wetbulb <- mediation::mediate(
  med_trading_monsoon_max_wetbulb, out_trading_monsoon_max_wetbulb,
  treat = "max_wb", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_monsoon_max_wetbulb)

## 11. Maximum wet-bulb temperature: trading, dry season

med_trading_dry_max_wetbulb <- lmer(
  log_travel_time ~ max_wb + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(med_trading_dry_max_wetbulb)

out_trading_dry_max_wetbulb <- lmer(
  net_income_bdt ~ max_wb + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(out_trading_dry_max_wetbulb)

.Random.seed <- simulation_seeds[["pooled_trading__dry__max_wb"]]
mediation_trading_dry_max_wetbulb <- mediation::mediate(
  med_trading_dry_max_wetbulb, out_trading_dry_max_wetbulb,
  treat = "max_wb", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_dry_max_wetbulb)

## 12. Daily rainfall: trading, monsoon season

med_trading_monsoon_rainfall <- lmer(
  log_travel_time ~ total_rain + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(med_trading_monsoon_rainfall)

out_trading_monsoon_rainfall <- lmer(
  net_income_bdt ~ total_rain + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(out_trading_monsoon_rainfall)

.Random.seed <- simulation_seeds[["pooled_trading__monsoon__total_rain"]]
mediation_trading_monsoon_rainfall <- mediation::mediate(
  med_trading_monsoon_rainfall, out_trading_monsoon_rainfall,
  treat = "total_rain", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_monsoon_rainfall)

## 13. Daily rainfall: trading, dry season

med_trading_dry_rainfall <- lmer(
  log_travel_time ~ total_rain + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(med_trading_dry_rainfall)

out_trading_dry_rainfall <- lmer(
  net_income_bdt ~ total_rain + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(out_trading_dry_rainfall)

.Random.seed <- simulation_seeds[["pooled_trading__dry__total_rain"]]
mediation_trading_dry_rainfall <- mediation::mediate(
  med_trading_dry_rainfall, out_trading_dry_rainfall,
  treat = "total_rain", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_dry_rainfall)

## 14. Consecutive hot days: fishing V1, monsoon season

med_fishing_V1_monsoon_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_monsoon, REML = TRUE)
summary(med_fishing_V1_monsoon_hot_days)

out_fishing_V1_monsoon_hot_days <- lmer(
  net_income_bdt ~ log_travel_time + consec_hot_days_p90 +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_monsoon, REML = TRUE)
summary(out_fishing_V1_monsoon_hot_days)

.Random.seed <- simulation_seeds[["V1__monsoon__consec_hot_days_p90"]]
mediation_fishing_V1_monsoon_hot_days <- mediation::mediate(
  med_fishing_V1_monsoon_hot_days, out_fishing_V1_monsoon_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V1_monsoon_hot_days)

## 15. Consecutive hot days: fishing V1, dry season

med_fishing_V1_dry_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_dry, REML = TRUE)
summary(med_fishing_V1_dry_hot_days)

out_fishing_V1_dry_hot_days <- lmer(
  net_income_bdt ~ log_travel_time + consec_hot_days_p90 +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_dry, REML = TRUE)
summary(out_fishing_V1_dry_hot_days)

.Random.seed <- simulation_seeds[["V1__dry__consec_hot_days_p90"]]
mediation_fishing_V1_dry_hot_days <- mediation::mediate(
  med_fishing_V1_dry_hot_days, out_fishing_V1_dry_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V1_dry_hot_days)

## 16. Wet-spell duration: fishing V1, monsoon season

med_fishing_V1_monsoon_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_monsoon, REML = TRUE)
summary(med_fishing_V1_monsoon_wet_spell)

out_fishing_V1_monsoon_wet_spell <- lmer(
  net_income_bdt ~ log_travel_time + rain_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_monsoon, REML = TRUE)
summary(out_fishing_V1_monsoon_wet_spell)

.Random.seed <- simulation_seeds[["V1__monsoon__rain_days_length"]]
mediation_fishing_V1_monsoon_wet_spell <- mediation::mediate(
  med_fishing_V1_monsoon_wet_spell, out_fishing_V1_monsoon_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V1_monsoon_wet_spell)

## 17. Wet-spell duration: fishing V1, dry season

med_fishing_V1_dry_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_dry, REML = TRUE)
summary(med_fishing_V1_dry_wet_spell)

out_fishing_V1_dry_wet_spell <- lmer(
  net_income_bdt ~ log_travel_time + rain_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_dry, REML = TRUE)
summary(out_fishing_V1_dry_wet_spell)

.Random.seed <- simulation_seeds[["V1__dry__rain_days_length"]]
mediation_fishing_V1_dry_wet_spell <- mediation::mediate(
  med_fishing_V1_dry_wet_spell, out_fishing_V1_dry_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V1_dry_wet_spell)

## 18. Dry-spell duration: fishing V1, monsoon season

med_fishing_V1_monsoon_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_monsoon, REML = TRUE)
summary(med_fishing_V1_monsoon_dry_spell)

out_fishing_V1_monsoon_dry_spell <- lmer(
  net_income_bdt ~ log_travel_time + dry_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_monsoon, REML = TRUE)
summary(out_fishing_V1_monsoon_dry_spell)

.Random.seed <- simulation_seeds[["V1__monsoon__dry_days_length"]]
mediation_fishing_V1_monsoon_dry_spell <- mediation::mediate(
  med_fishing_V1_monsoon_dry_spell, out_fishing_V1_monsoon_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V1_monsoon_dry_spell)

## 19. Dry-spell duration: fishing V1, dry season

med_fishing_V1_dry_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_dry, REML = TRUE)
summary(med_fishing_V1_dry_dry_spell)

out_fishing_V1_dry_dry_spell <- lmer(
  net_income_bdt ~ log_travel_time + dry_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V1_dry, REML = TRUE)
summary(out_fishing_V1_dry_dry_spell)

.Random.seed <- simulation_seeds[["V1__dry__dry_days_length"]]
mediation_fishing_V1_dry_dry_spell <- mediation::mediate(
  med_fishing_V1_dry_dry_spell, out_fishing_V1_dry_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V1_dry_dry_spell)

## 20. Consecutive hot days: fishing V2, monsoon season

med_fishing_V2_monsoon_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_monsoon, REML = TRUE)
summary(med_fishing_V2_monsoon_hot_days)

out_fishing_V2_monsoon_hot_days <- lmer(
  net_income_bdt ~ log_travel_time + consec_hot_days_p90 +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_monsoon, REML = TRUE)
summary(out_fishing_V2_monsoon_hot_days)

.Random.seed <- simulation_seeds[["V2__monsoon__consec_hot_days_p90"]]
mediation_fishing_V2_monsoon_hot_days <- mediation::mediate(
  med_fishing_V2_monsoon_hot_days, out_fishing_V2_monsoon_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V2_monsoon_hot_days)

## 21. Consecutive hot days: fishing V2, dry season

med_fishing_V2_dry_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_dry, REML = TRUE)
summary(med_fishing_V2_dry_hot_days)

out_fishing_V2_dry_hot_days <- lmer(
  net_income_bdt ~ log_travel_time + consec_hot_days_p90 +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_dry, REML = TRUE)
summary(out_fishing_V2_dry_hot_days)

.Random.seed <- simulation_seeds[["V2__dry__consec_hot_days_p90"]]
mediation_fishing_V2_dry_hot_days <- mediation::mediate(
  med_fishing_V2_dry_hot_days, out_fishing_V2_dry_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V2_dry_hot_days)

## 22. Wet-spell duration: fishing V2, monsoon season

med_fishing_V2_monsoon_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_monsoon, REML = TRUE)
summary(med_fishing_V2_monsoon_wet_spell)

out_fishing_V2_monsoon_wet_spell <- lmer(
  net_income_bdt ~ log_travel_time + rain_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_monsoon, REML = TRUE)
summary(out_fishing_V2_monsoon_wet_spell)

.Random.seed <- simulation_seeds[["V2__monsoon__rain_days_length"]]
mediation_fishing_V2_monsoon_wet_spell <- mediation::mediate(
  med_fishing_V2_monsoon_wet_spell, out_fishing_V2_monsoon_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V2_monsoon_wet_spell)

## 23. Wet-spell duration: fishing V2, dry season

med_fishing_V2_dry_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_dry, REML = TRUE)
summary(med_fishing_V2_dry_wet_spell)

out_fishing_V2_dry_wet_spell <- lmer(
  net_income_bdt ~ log_travel_time + rain_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_dry, REML = TRUE)
summary(out_fishing_V2_dry_wet_spell)

.Random.seed <- simulation_seeds[["V2__dry__rain_days_length"]]
mediation_fishing_V2_dry_wet_spell <- mediation::mediate(
  med_fishing_V2_dry_wet_spell, out_fishing_V2_dry_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V2_dry_wet_spell)

## 24. Dry-spell duration: fishing V2, monsoon season

med_fishing_V2_monsoon_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_monsoon, REML = TRUE)
summary(med_fishing_V2_monsoon_dry_spell)

out_fishing_V2_monsoon_dry_spell <- lmer(
  net_income_bdt ~ log_travel_time + dry_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_monsoon, REML = TRUE)
summary(out_fishing_V2_monsoon_dry_spell)

.Random.seed <- simulation_seeds[["V2__monsoon__dry_days_length"]]
mediation_fishing_V2_monsoon_dry_spell <- mediation::mediate(
  med_fishing_V2_monsoon_dry_spell, out_fishing_V2_monsoon_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V2_monsoon_dry_spell)

## 25. Dry-spell duration: fishing V2, dry season

med_fishing_V2_dry_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_dry, REML = TRUE)
summary(med_fishing_V2_dry_dry_spell)

out_fishing_V2_dry_dry_spell <- lmer(
  net_income_bdt ~ log_travel_time + dry_days_length +
    age + factor(year) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V2_dry, REML = TRUE)
summary(out_fishing_V2_dry_dry_spell)

.Random.seed <- simulation_seeds[["V2__dry__dry_days_length"]]
mediation_fishing_V2_dry_dry_spell <- mediation::mediate(
  med_fishing_V2_dry_dry_spell, out_fishing_V2_dry_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V2_dry_dry_spell)

## 26. Consecutive hot days: fishing V3 V4, monsoon season

med_fishing_V3_V4_monsoon_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_monsoon, REML = TRUE)
summary(med_fishing_V3_V4_monsoon_hot_days)

out_fishing_V3_V4_monsoon_hot_days <- lmer(
  net_income_bdt ~ log_travel_time + consec_hot_days_p90 +
    age + factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_monsoon, REML = TRUE)
summary(out_fishing_V3_V4_monsoon_hot_days)

.Random.seed <- simulation_seeds[["V3_V4__monsoon__consec_hot_days_p90"]]
mediation_fishing_V3_V4_monsoon_hot_days <- mediation::mediate(
  med_fishing_V3_V4_monsoon_hot_days, out_fishing_V3_V4_monsoon_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V3_V4_monsoon_hot_days)

## 27. Consecutive hot days: fishing V3 V4, dry season

med_fishing_V3_V4_dry_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_dry, REML = TRUE)
summary(med_fishing_V3_V4_dry_hot_days)

out_fishing_V3_V4_dry_hot_days <- lmer(
  net_income_bdt ~ log_travel_time + consec_hot_days_p90 +
    age + factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_dry, REML = TRUE)
summary(out_fishing_V3_V4_dry_hot_days)

.Random.seed <- simulation_seeds[["V3_V4__dry__consec_hot_days_p90"]]
mediation_fishing_V3_V4_dry_hot_days <- mediation::mediate(
  med_fishing_V3_V4_dry_hot_days, out_fishing_V3_V4_dry_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V3_V4_dry_hot_days)

## 28. Wet-spell duration: fishing V3 V4, monsoon season

med_fishing_V3_V4_monsoon_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_monsoon, REML = TRUE)
summary(med_fishing_V3_V4_monsoon_wet_spell)

out_fishing_V3_V4_monsoon_wet_spell <- lmer(
  net_income_bdt ~ log_travel_time + rain_days_length +
    age + factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_monsoon, REML = TRUE)
summary(out_fishing_V3_V4_monsoon_wet_spell)

.Random.seed <- simulation_seeds[["V3_V4__monsoon__rain_days_length"]]
mediation_fishing_V3_V4_monsoon_wet_spell <- mediation::mediate(
  med_fishing_V3_V4_monsoon_wet_spell, out_fishing_V3_V4_monsoon_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V3_V4_monsoon_wet_spell)

## 29. Wet-spell duration: fishing V3 V4, dry season

med_fishing_V3_V4_dry_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_dry, REML = TRUE)
summary(med_fishing_V3_V4_dry_wet_spell)

out_fishing_V3_V4_dry_wet_spell <- lmer(
  net_income_bdt ~ log_travel_time + rain_days_length +
    age + factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_dry, REML = TRUE)
summary(out_fishing_V3_V4_dry_wet_spell)

.Random.seed <- simulation_seeds[["V3_V4__dry__rain_days_length"]]
mediation_fishing_V3_V4_dry_wet_spell <- mediation::mediate(
  med_fishing_V3_V4_dry_wet_spell, out_fishing_V3_V4_dry_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V3_V4_dry_wet_spell)

## 30. Dry-spell duration: fishing V3 V4, monsoon season

med_fishing_V3_V4_monsoon_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_monsoon, REML = TRUE)
summary(med_fishing_V3_V4_monsoon_dry_spell)

out_fishing_V3_V4_monsoon_dry_spell <- lmer(
  net_income_bdt ~ log_travel_time + dry_days_length +
    age + factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_monsoon, REML = TRUE)
summary(out_fishing_V3_V4_monsoon_dry_spell)

.Random.seed <- simulation_seeds[["V3_V4__monsoon__dry_days_length"]]
mediation_fishing_V3_V4_monsoon_dry_spell <- mediation::mediate(
  med_fishing_V3_V4_monsoon_dry_spell, out_fishing_V3_V4_monsoon_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V3_V4_monsoon_dry_spell)

## 31. Dry-spell duration: fishing V3 V4, dry season

med_fishing_V3_V4_dry_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_dry, REML = TRUE)
summary(med_fishing_V3_V4_dry_dry_spell)

out_fishing_V3_V4_dry_dry_spell <- lmer(
  net_income_bdt ~ log_travel_time + dry_days_length +
    age + factor(year) + factor(village) + factor(diversified) + gender + (1 | participant_id),
  data = fishing_V3_V4_dry, REML = TRUE)
summary(out_fishing_V3_V4_dry_dry_spell)

.Random.seed <- simulation_seeds[["V3_V4__dry__dry_days_length"]]
mediation_fishing_V3_V4_dry_dry_spell <- mediation::mediate(
  med_fishing_V3_V4_dry_dry_spell, out_fishing_V3_V4_dry_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_V3_V4_dry_dry_spell)

# 32. Saving results for tables and figures
# Assemble the coefficients and mediation estimates for tables and figures.

export_results <- function(model_pairs, mediation_results) {
  coefficients <- diagnostics <- effects <- list()
  for (id in names(model_pairs)) {
    for (equation in c("mediator", "outcome")) {
      fit <- model_pairs[[id]][[equation]]
      b <- fixef(fit)
      se <- sqrt(diag(vcov(fit)))
      variances <- as.data.frame(VarCorr(fit))$vcov
      coefficients[[paste(id, equation)]] <- data.frame(
        id, equation, term = names(b), estimate = unname(b), se = unname(se),
        p = unname(2 * pnorm(-abs(b / se))))
      diagnostics[[paste(id, equation)]] <- data.frame(
        id, equation, n = nobs(fit), respondents = unname(ngrps(fit)[1]),
        random_variance = variances[1], residual_variance = variances[2],
        icc = variances[1] / sum(variances), singular = isSingular(fit),
        optimizer = fit@optinfo$optimizer,
        convergence = paste(fit@optinfo$conv$lme4$messages, collapse = "; "))
    }
    result <- mediation_results[[id]]
    effects[[id]] <- data.frame(
      id, effect = c("indirect", "direct", "total", "proportion"),
      estimate = c(result$d.avg, result$z.avg, result$tau.coef, result$n.avg),
      lower = c(result$d.avg.ci[1], result$z.avg.ci[1], result$tau.ci[1], result$n.avg.ci[1]),
      upper = c(result$d.avg.ci[2], result$z.avg.ci[2], result$tau.ci[2], result$n.avg.ci[2]),
      p = c(result$d.avg.p, result$z.avg.p, result$tau.p, result$n.avg.p))
  }
  list(coefficients = do.call(rbind, coefficients),
       diagnostics = do.call(rbind, diagnostics), effects = do.call(rbind, effects))
}

model_pairs <- list(
  "pooled_fishing__monsoon__n_hours_hot_p90" = list(mediator = med_fishing_monsoon_hot_hours, outcome = out_fishing_monsoon_hot_hours),
  "pooled_fishing__dry__n_hours_hot_p90" = list(mediator = med_fishing_dry_hot_hours, outcome = out_fishing_dry_hot_hours),
  "pooled_fishing__monsoon__max_wb" = list(mediator = med_fishing_monsoon_max_wetbulb, outcome = out_fishing_monsoon_max_wetbulb),
  "pooled_fishing__dry__max_wb" = list(mediator = med_fishing_dry_max_wetbulb, outcome = out_fishing_dry_max_wetbulb),
  "pooled_fishing__monsoon__total_rain" = list(mediator = med_fishing_monsoon_rainfall, outcome = out_fishing_monsoon_rainfall),
  "pooled_fishing__dry__total_rain" = list(mediator = med_fishing_dry_rainfall, outcome = out_fishing_dry_rainfall),
  "pooled_trading__monsoon__n_hours_hot_p90" = list(mediator = med_trading_monsoon_hot_hours, outcome = out_trading_monsoon_hot_hours),
  "pooled_trading__dry__n_hours_hot_p90" = list(mediator = med_trading_dry_hot_hours, outcome = out_trading_dry_hot_hours),
  "pooled_trading__monsoon__max_wb" = list(mediator = med_trading_monsoon_max_wetbulb, outcome = out_trading_monsoon_max_wetbulb),
  "pooled_trading__dry__max_wb" = list(mediator = med_trading_dry_max_wetbulb, outcome = out_trading_dry_max_wetbulb),
  "pooled_trading__monsoon__total_rain" = list(mediator = med_trading_monsoon_rainfall, outcome = out_trading_monsoon_rainfall),
  "pooled_trading__dry__total_rain" = list(mediator = med_trading_dry_rainfall, outcome = out_trading_dry_rainfall),
  "V1__monsoon__consec_hot_days_p90" = list(mediator = med_fishing_V1_monsoon_hot_days, outcome = out_fishing_V1_monsoon_hot_days),
  "V1__dry__consec_hot_days_p90" = list(mediator = med_fishing_V1_dry_hot_days, outcome = out_fishing_V1_dry_hot_days),
  "V1__monsoon__rain_days_length" = list(mediator = med_fishing_V1_monsoon_wet_spell, outcome = out_fishing_V1_monsoon_wet_spell),
  "V1__dry__rain_days_length" = list(mediator = med_fishing_V1_dry_wet_spell, outcome = out_fishing_V1_dry_wet_spell),
  "V1__monsoon__dry_days_length" = list(mediator = med_fishing_V1_monsoon_dry_spell, outcome = out_fishing_V1_monsoon_dry_spell),
  "V1__dry__dry_days_length" = list(mediator = med_fishing_V1_dry_dry_spell, outcome = out_fishing_V1_dry_dry_spell),
  "V2__monsoon__consec_hot_days_p90" = list(mediator = med_fishing_V2_monsoon_hot_days, outcome = out_fishing_V2_monsoon_hot_days),
  "V2__dry__consec_hot_days_p90" = list(mediator = med_fishing_V2_dry_hot_days, outcome = out_fishing_V2_dry_hot_days),
  "V2__monsoon__rain_days_length" = list(mediator = med_fishing_V2_monsoon_wet_spell, outcome = out_fishing_V2_monsoon_wet_spell),
  "V2__dry__rain_days_length" = list(mediator = med_fishing_V2_dry_wet_spell, outcome = out_fishing_V2_dry_wet_spell),
  "V2__monsoon__dry_days_length" = list(mediator = med_fishing_V2_monsoon_dry_spell, outcome = out_fishing_V2_monsoon_dry_spell),
  "V2__dry__dry_days_length" = list(mediator = med_fishing_V2_dry_dry_spell, outcome = out_fishing_V2_dry_dry_spell),
  "V3_V4__monsoon__consec_hot_days_p90" = list(mediator = med_fishing_V3_V4_monsoon_hot_days, outcome = out_fishing_V3_V4_monsoon_hot_days),
  "V3_V4__dry__consec_hot_days_p90" = list(mediator = med_fishing_V3_V4_dry_hot_days, outcome = out_fishing_V3_V4_dry_hot_days),
  "V3_V4__monsoon__rain_days_length" = list(mediator = med_fishing_V3_V4_monsoon_wet_spell, outcome = out_fishing_V3_V4_monsoon_wet_spell),
  "V3_V4__dry__rain_days_length" = list(mediator = med_fishing_V3_V4_dry_wet_spell, outcome = out_fishing_V3_V4_dry_wet_spell),
  "V3_V4__monsoon__dry_days_length" = list(mediator = med_fishing_V3_V4_monsoon_dry_spell, outcome = out_fishing_V3_V4_monsoon_dry_spell),
  "V3_V4__dry__dry_days_length" = list(mediator = med_fishing_V3_V4_dry_dry_spell, outcome = out_fishing_V3_V4_dry_dry_spell))

mediation_results <- list(
  "pooled_fishing__monsoon__n_hours_hot_p90" = mediation_fishing_monsoon_hot_hours,
  "pooled_fishing__dry__n_hours_hot_p90" = mediation_fishing_dry_hot_hours,
  "pooled_fishing__monsoon__max_wb" = mediation_fishing_monsoon_max_wetbulb,
  "pooled_fishing__dry__max_wb" = mediation_fishing_dry_max_wetbulb,
  "pooled_fishing__monsoon__total_rain" = mediation_fishing_monsoon_rainfall,
  "pooled_fishing__dry__total_rain" = mediation_fishing_dry_rainfall,
  "pooled_trading__monsoon__n_hours_hot_p90" = mediation_trading_monsoon_hot_hours,
  "pooled_trading__dry__n_hours_hot_p90" = mediation_trading_dry_hot_hours,
  "pooled_trading__monsoon__max_wb" = mediation_trading_monsoon_max_wetbulb,
  "pooled_trading__dry__max_wb" = mediation_trading_dry_max_wetbulb,
  "pooled_trading__monsoon__total_rain" = mediation_trading_monsoon_rainfall,
  "pooled_trading__dry__total_rain" = mediation_trading_dry_rainfall,
  "V1__monsoon__consec_hot_days_p90" = mediation_fishing_V1_monsoon_hot_days,
  "V1__dry__consec_hot_days_p90" = mediation_fishing_V1_dry_hot_days,
  "V1__monsoon__rain_days_length" = mediation_fishing_V1_monsoon_wet_spell,
  "V1__dry__rain_days_length" = mediation_fishing_V1_dry_wet_spell,
  "V1__monsoon__dry_days_length" = mediation_fishing_V1_monsoon_dry_spell,
  "V1__dry__dry_days_length" = mediation_fishing_V1_dry_dry_spell,
  "V2__monsoon__consec_hot_days_p90" = mediation_fishing_V2_monsoon_hot_days,
  "V2__dry__consec_hot_days_p90" = mediation_fishing_V2_dry_hot_days,
  "V2__monsoon__rain_days_length" = mediation_fishing_V2_monsoon_wet_spell,
  "V2__dry__rain_days_length" = mediation_fishing_V2_dry_wet_spell,
  "V2__monsoon__dry_days_length" = mediation_fishing_V2_monsoon_dry_spell,
  "V2__dry__dry_days_length" = mediation_fishing_V2_dry_dry_spell,
  "V3_V4__monsoon__consec_hot_days_p90" = mediation_fishing_V3_V4_monsoon_hot_days,
  "V3_V4__dry__consec_hot_days_p90" = mediation_fishing_V3_V4_dry_hot_days,
  "V3_V4__monsoon__rain_days_length" = mediation_fishing_V3_V4_monsoon_wet_spell,
  "V3_V4__dry__rain_days_length" = mediation_fishing_V3_V4_dry_wet_spell,
  "V3_V4__monsoon__dry_days_length" = mediation_fishing_V3_V4_monsoon_dry_spell,
  "V3_V4__dry__dry_days_length" = mediation_fishing_V3_V4_dry_dry_spell)

si_results <- export_results(model_pairs, mediation_results)
si_coefficients <- si_results$coefficients
si_diagnostics <- si_results$diagnostics
si_effects <- si_results$effects

model_names <- strsplit(names(model_pairs), "__", fixed = TRUE)
si_index <- data.frame(
  id = names(model_pairs),
  group = sapply(model_names, "[", 1),
  activity = ifelse(sapply(model_names, "[", 1) == "pooled_trading", "trading", "fishing"),
  season = sapply(model_names, "[", 2),
  exposure = sapply(model_names, "[", 3),
  table = c("S17", "S17", "S18", "S18", "S19", "S19", "S20", "S20", "S21", "S21", "S22", "S22", "S8", "S8", "S9", "S9", "S10", "S10", "S11", "S11", "S12", "S12", "S13", "S13", "S14", "S14", "S15", "S15", "S16", "S16"),
  figure = c("S4", "S4", "S4", "S4", "S4", "S4", "S5", "S5", "S5", "S5", "S5", "S5", "S1", "S1", "S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2", "S2", "S2", "S3", "S3", "S3", "S3", "S3", "S3"))

save(si_coefficients, si_diagnostics, si_effects, si_index,
     file = "si_results.RData")
