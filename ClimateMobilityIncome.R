##### Climate, daily mobility, and income: main analyses
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

## 2. Consecutive hot days: fishing, monsoon season

med_fishing_monsoon_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(med_fishing_monsoon_hot_days)

out_fishing_monsoon_hot_days <- lmer(
  net_income_bdt ~ consec_hot_days_p90 + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(out_fishing_monsoon_hot_days)

.Random.seed <- simulation_seeds[["pooled_fishing__monsoon__consec_hot_days_p90"]]
mediation_fishing_monsoon_hot_days <- mediation::mediate(
  med_fishing_monsoon_hot_days, out_fishing_monsoon_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_monsoon_hot_days)

## 3. Consecutive hot days: fishing, dry season

med_fishing_dry_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(med_fishing_dry_hot_days)

out_fishing_dry_hot_days <- lmer(
  net_income_bdt ~ consec_hot_days_p90 + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(out_fishing_dry_hot_days)

.Random.seed <- simulation_seeds[["pooled_fishing__dry__consec_hot_days_p90"]]
mediation_fishing_dry_hot_days <- mediation::mediate(
  med_fishing_dry_hot_days, out_fishing_dry_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_dry_hot_days)

## 4. Wet-spell duration: fishing, monsoon season

med_fishing_monsoon_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(med_fishing_monsoon_wet_spell)

out_fishing_monsoon_wet_spell <- lmer(
  net_income_bdt ~ rain_days_length + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(out_fishing_monsoon_wet_spell)

.Random.seed <- simulation_seeds[["pooled_fishing__monsoon__rain_days_length"]]
mediation_fishing_monsoon_wet_spell <- mediation::mediate(
  med_fishing_monsoon_wet_spell, out_fishing_monsoon_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_monsoon_wet_spell)

## 5. Wet-spell duration: fishing, dry season

med_fishing_dry_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(med_fishing_dry_wet_spell)

out_fishing_dry_wet_spell <- lmer(
  net_income_bdt ~ rain_days_length + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(out_fishing_dry_wet_spell)

.Random.seed <- simulation_seeds[["pooled_fishing__dry__rain_days_length"]]
mediation_fishing_dry_wet_spell <- mediation::mediate(
  med_fishing_dry_wet_spell, out_fishing_dry_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_dry_wet_spell)

## 6. Dry-spell duration: fishing, monsoon season

med_fishing_monsoon_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(med_fishing_monsoon_dry_spell)

out_fishing_monsoon_dry_spell <- lmer(
  net_income_bdt ~ dry_days_length + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_monsoon, REML = TRUE)
summary(out_fishing_monsoon_dry_spell)

.Random.seed <- simulation_seeds[["pooled_fishing__monsoon__dry_days_length"]]
mediation_fishing_monsoon_dry_spell <- mediation::mediate(
  med_fishing_monsoon_dry_spell, out_fishing_monsoon_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_monsoon_dry_spell)

## 7. Dry-spell duration: fishing, dry season

med_fishing_dry_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(med_fishing_dry_dry_spell)

out_fishing_dry_dry_spell <- lmer(
  net_income_bdt ~ dry_days_length + log_travel_time +
    age + gender + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = fishing_dry, REML = TRUE)
summary(out_fishing_dry_dry_spell)

.Random.seed <- simulation_seeds[["pooled_fishing__dry__dry_days_length"]]
mediation_fishing_dry_dry_spell <- mediation::mediate(
  med_fishing_dry_dry_spell, out_fishing_dry_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_fishing_dry_dry_spell)

## 8. Consecutive hot days: trading, monsoon season

med_trading_monsoon_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(med_trading_monsoon_hot_days)

out_trading_monsoon_hot_days <- lmer(
  net_income_bdt ~ consec_hot_days_p90 + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(out_trading_monsoon_hot_days)

.Random.seed <- simulation_seeds[["pooled_trading__monsoon__consec_hot_days_p90"]]
mediation_trading_monsoon_hot_days <- mediation::mediate(
  med_trading_monsoon_hot_days, out_trading_monsoon_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_monsoon_hot_days)

## 9. Consecutive hot days: trading, dry season

med_trading_dry_hot_days <- lmer(
  log_travel_time ~ consec_hot_days_p90 + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(med_trading_dry_hot_days)

out_trading_dry_hot_days <- lmer(
  net_income_bdt ~ consec_hot_days_p90 + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(out_trading_dry_hot_days)

.Random.seed <- simulation_seeds[["pooled_trading__dry__consec_hot_days_p90"]]
mediation_trading_dry_hot_days <- mediation::mediate(
  med_trading_dry_hot_days, out_trading_dry_hot_days,
  treat = "consec_hot_days_p90", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_dry_hot_days)

## 10. Wet-spell duration: trading, monsoon season

med_trading_monsoon_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(med_trading_monsoon_wet_spell)

out_trading_monsoon_wet_spell <- lmer(
  net_income_bdt ~ rain_days_length + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(out_trading_monsoon_wet_spell)

.Random.seed <- simulation_seeds[["pooled_trading__monsoon__rain_days_length"]]
mediation_trading_monsoon_wet_spell <- mediation::mediate(
  med_trading_monsoon_wet_spell, out_trading_monsoon_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_monsoon_wet_spell)

## 11. Wet-spell duration: trading, dry season

med_trading_dry_wet_spell <- lmer(
  log_travel_time ~ rain_days_length + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(med_trading_dry_wet_spell)

out_trading_dry_wet_spell <- lmer(
  net_income_bdt ~ rain_days_length + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(out_trading_dry_wet_spell)

.Random.seed <- simulation_seeds[["pooled_trading__dry__rain_days_length"]]
mediation_trading_dry_wet_spell <- mediation::mediate(
  med_trading_dry_wet_spell, out_trading_dry_wet_spell,
  treat = "rain_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_dry_wet_spell)

## 12. Dry-spell duration: trading, monsoon season

med_trading_monsoon_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(med_trading_monsoon_dry_spell)

out_trading_monsoon_dry_spell <- lmer(
  net_income_bdt ~ dry_days_length + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_monsoon, REML = TRUE)
summary(out_trading_monsoon_dry_spell)

.Random.seed <- simulation_seeds[["pooled_trading__monsoon__dry_days_length"]]
mediation_trading_monsoon_dry_spell <- mediation::mediate(
  med_trading_monsoon_dry_spell, out_trading_monsoon_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_monsoon_dry_spell)

## 13. Dry-spell duration: trading, dry season

med_trading_dry_dry_spell <- lmer(
  log_travel_time ~ dry_days_length + age +
    factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(med_trading_dry_dry_spell)

out_trading_dry_dry_spell <- lmer(
  net_income_bdt ~ dry_days_length + log_travel_time +
    age + factor(year) + factor(village) + factor(diversified) + (1 | participant_id),
  data = trading_dry, REML = TRUE)
summary(out_trading_dry_dry_spell)

.Random.seed <- simulation_seeds[["pooled_trading__dry__dry_days_length"]]
mediation_trading_dry_dry_spell <- mediation::mediate(
  med_trading_dry_dry_spell, out_trading_dry_dry_spell,
  treat = "dry_days_length", mediator = "log_travel_time",
  control.value = 0, treat.value = 1, sims = 1000)
summary(mediation_trading_dry_dry_spell)

# 14. Saving results for tables and figures
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
  "pooled_fishing__monsoon__consec_hot_days_p90" = list(mediator = med_fishing_monsoon_hot_days, outcome = out_fishing_monsoon_hot_days),
  "pooled_fishing__dry__consec_hot_days_p90" = list(mediator = med_fishing_dry_hot_days, outcome = out_fishing_dry_hot_days),
  "pooled_fishing__monsoon__rain_days_length" = list(mediator = med_fishing_monsoon_wet_spell, outcome = out_fishing_monsoon_wet_spell),
  "pooled_fishing__dry__rain_days_length" = list(mediator = med_fishing_dry_wet_spell, outcome = out_fishing_dry_wet_spell),
  "pooled_fishing__monsoon__dry_days_length" = list(mediator = med_fishing_monsoon_dry_spell, outcome = out_fishing_monsoon_dry_spell),
  "pooled_fishing__dry__dry_days_length" = list(mediator = med_fishing_dry_dry_spell, outcome = out_fishing_dry_dry_spell),
  "pooled_trading__monsoon__consec_hot_days_p90" = list(mediator = med_trading_monsoon_hot_days, outcome = out_trading_monsoon_hot_days),
  "pooled_trading__dry__consec_hot_days_p90" = list(mediator = med_trading_dry_hot_days, outcome = out_trading_dry_hot_days),
  "pooled_trading__monsoon__rain_days_length" = list(mediator = med_trading_monsoon_wet_spell, outcome = out_trading_monsoon_wet_spell),
  "pooled_trading__dry__rain_days_length" = list(mediator = med_trading_dry_wet_spell, outcome = out_trading_dry_wet_spell),
  "pooled_trading__monsoon__dry_days_length" = list(mediator = med_trading_monsoon_dry_spell, outcome = out_trading_monsoon_dry_spell),
  "pooled_trading__dry__dry_days_length" = list(mediator = med_trading_dry_dry_spell, outcome = out_trading_dry_dry_spell))

mediation_results <- list(
  "pooled_fishing__monsoon__consec_hot_days_p90" = mediation_fishing_monsoon_hot_days,
  "pooled_fishing__dry__consec_hot_days_p90" = mediation_fishing_dry_hot_days,
  "pooled_fishing__monsoon__rain_days_length" = mediation_fishing_monsoon_wet_spell,
  "pooled_fishing__dry__rain_days_length" = mediation_fishing_dry_wet_spell,
  "pooled_fishing__monsoon__dry_days_length" = mediation_fishing_monsoon_dry_spell,
  "pooled_fishing__dry__dry_days_length" = mediation_fishing_dry_dry_spell,
  "pooled_trading__monsoon__consec_hot_days_p90" = mediation_trading_monsoon_hot_days,
  "pooled_trading__dry__consec_hot_days_p90" = mediation_trading_dry_hot_days,
  "pooled_trading__monsoon__rain_days_length" = mediation_trading_monsoon_wet_spell,
  "pooled_trading__dry__rain_days_length" = mediation_trading_dry_wet_spell,
  "pooled_trading__monsoon__dry_days_length" = mediation_trading_monsoon_dry_spell,
  "pooled_trading__dry__dry_days_length" = mediation_trading_dry_dry_spell)

main_results <- export_results(model_pairs, mediation_results)
main_coefficients <- main_results$coefficients
main_diagnostics <- main_results$diagnostics
main_effects <- main_results$effects

model_names <- strsplit(names(model_pairs), "__", fixed = TRUE)
main_index <- data.frame(
  id = names(model_pairs),
  group = sapply(model_names, "[", 1),
  activity = ifelse(sapply(model_names, "[", 1) == "pooled_trading", "trading", "fishing"),
  season = sapply(model_names, "[", 2),
  exposure = sapply(model_names, "[", 3),
  table = c("S2", "S2", "S3", "S3", "S4", "S4", "S5", "S5", "S6", "S6", "S7", "S7"),
  figure = c("3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4"))

save(main_coefficients, main_diagnostics, main_effects, main_index,
     file = "main_results.RData")
