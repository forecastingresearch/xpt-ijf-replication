sets_for_summary <- meta %>%
  filter(questionType == "multiYearReciprocal")

get_stat_summary <- function(set, year, defaultForecast = 50) {
  setq_forecasts <- forecasts %>%
    filter(setName == set) %>%
    filter(grepl(year, questionName)) %>%
    filter(grepl("Your", questionName)) %>%
    filter(stage == 1)
  setq_users <- unique(setq_forecasts$userId)
  setq_final_forecasts <- data.frame()
  for (i in 1:length(setq_users)) {
    user_final_forecast <- setq_forecasts %>%
      filter(userId == setq_users[i]) %>%
      filter(timestampId == max(timestampId))
    setq_final_forecasts <- rbind(setq_final_forecasts, user_final_forecast)
  }
  # Remove default forecasts
  setq_final_forecasts <- setq_final_forecasts %>%
    filter(forecast != defaultForecast)
  return(setq_final_forecasts)
}

# Get super median, expert median, domain expert median
summarize_set <- function(set_forecasts, domain) {
  tbl_for_summary <- set_forecasts %>%
    rowwise() %>%
    mutate(group = ifelse(userId %in% supers,
      "supers",
      "experts"
    ))
  if (domain != "") {
    domain_experts <- experts %>%
      rowwise() %>%
      filter(domain %in% c(specialty1, specialty2, specialty3))
    tbl_for_summary <- tbl_for_summary %>%
      rowwise() %>%
      mutate(group = ifelse(userId %in% domain_experts$userId,
        "domain experts",
        group
      ))
  } else {
    tbl_for_summary$group[tbl_for_summary$group == "experts"] <- "domain experts"
  }
  # Use summarize to get medians and then pivot
  set_summary <- tbl_for_summary %>%
    group_by(group) %>%
    summarize(median = median(forecast)) %>%
    pivot_wider(
      names_from = group,
      values_from = median,
      names_glue = "{tolower(gsub(' ', '', group))}Median"
    ) %>%
    mutate(
      setName = set,
      year = year,
      domainExpertsMedian = domainexpertsMedian
    ) %>%
    select(setName, year, supersMedian, domainExpertsMedian)
  return(set_summary)
}

# Loop
set_summary <- data.frame()
for (i in 1:nrow(sets_for_summary)) {
  set <- sets_for_summary$setName[i]
  domain <- sets_for_summary$specialty[i]
  years <- sets_for_summary[i, ] %>%
    select(year1, year2, year3) %>%
    unlist()
  for (j in 1:length(years)) {
    year <- years[j]
    print(set)
    print(year)
    set_forecasts <- get_stat_summary(set, year)
    # Get Stage 1 summary for set, year
    set_summary <- rbind(set_summary, summarize_set(set_forecasts, domain))
  }
}
