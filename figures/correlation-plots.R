q <- forecasts %>%
  filter(setName == "4. AI Extinction Risk") %>%
  filter(grepl("2100", questionName)) %>%
  filter(grepl("Your", questionName)) %>%
  filter(isCurrent == TRUE) %>%
  filter(forecast != 50)

q <- merge(q, rs_rank, by = "userId")

p <- ggplot(q, aes(x = forecast, y = avgRank)) +
  geom_point() + # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add the fit-line
  labs(x = "Forecast", y = "Average Reciprocal Scoring Rank") + # Label the axes
  ggtitle("AI Extinction Risk Forecast vs. Reciprocal Scoring Performance") + # Add a title
  scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100)) +
  theme_bw()

ggsave("fa6.png", p, width = 2500, height = 1566, units = "px")

q <- q %>%
  mutate(pct_above_true = NA)

q_users <- unique(q$userId)
# Get percentage of intersubjective forecasts abv true value
for (i in 1:length(q_users)) {
  print(i)
  user_forecasts <- data.frame()
  for (j in 1:nrow(set_summary)) {
    set <- set_summary$setName[j]
    year <- set_summary$year[j]
    s1_superForecast <- forecasts %>%
      filter(userId == q_users[i]) %>%
      filter(setName == set) %>%
      filter(grepl(year, questionName)) %>%
      filter(grepl("Super", questionName)) %>%
      filter(stage == 1) %>%
      filter(timestampId == max(timestampId)) %>%
      filter(forecast != 50) %>%
      select(forecast) %>%
      unlist()
    if (length(s1_superForecast) == 0) {
      s1_superForecast <- NA
    }
    s1_expertForecast <- forecasts %>%
      filter(userId == q_users[i]) %>%
      filter(setName == set) %>%
      filter(grepl(year, questionName)) %>%
      filter(grepl("Expert", questionName)) %>%
      filter(stage == 1) %>%
      filter(timestampId == max(timestampId)) %>%
      filter(forecast != 50) %>%
      select(forecast) %>%
      unlist()
    if (length(s1_expertForecast) == 0) {
      s1_expertForecast <- NA
    }
    newRow <- set_summary %>%
      filter(setName == set) %>%
      filter(year == set_summary$year[j]) %>%
      mutate(s1_superForecast = s1_superForecast) %>%
      mutate(s1_expertForecast = s1_expertForecast)
    user_forecasts <- rbind(user_forecasts, newRow)
  }
  sum_above_true <- sum(user_forecasts %>%
    summarise(
      count_super_above_median = sum(s1_superForecast > supersMedian, na.rm = TRUE),
      count_expert_above_median = sum(s1_expertForecast > domainExpertsMedian, na.rm = TRUE)
    ) %>%
    unlist(), na.rm = TRUE)
  valid_answers <- c(user_forecasts$s1_superForecast, user_forecasts$s1_expertForecast)
  valid_answers <- valid_answers[!is.na(valid_answers)]
  pct_above_true <- sum_above_true / length(valid_answers)
  q[q$userId == q_users[i], ]$pct_above_true <- pct_above_true
}

q$pct_above_true <- q$pct_above_true * 100

p <- ggplot(q, aes(x = forecast, y = pct_above_true)) +
  geom_point() + # Add points
  geom_smooth(method = "loess", se = FALSE, color = "blue") + # Add the fit-line
  labs(x = "Forecast", y = "Percent of Intersubjective Forecasts Above True Value") + # Label the axes
  ggtitle("AI Extinction Risk Forecasts vs. Percentage of Intersubjective Forecasts Above True Value") + # Add a title
  scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100)) +
  theme_bw()
ggsave("fa7.png", p, width = 2500, height = 1566, units = "px")
