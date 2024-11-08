log_score <- function(actual, forecast) {
  return(log(1 - abs(actual / 100 - forecast / 100)))
}

RSRanking_unincentivized <- data.frame()

# For each of the question sets (1. Genetically Engineered Pathogens, etc.)
for (i in 1:length(unique(sets_for_summary$setName))) {
  currentSetName <- unique(sets_for_summary$setName)[i]
  print(currentSetName)
  # Get years
  years <- sets_for_summary[i, ] %>% select(year1, year2, year3)
  years <- as.character(years)
  years <- years[years != ""]
  # Get belief sets
  beliefSets <- sets_for_summary[i, ] %>% select(
    yourBeliefs, expertBeliefs,
    superBeliefs
  )
  beliefSets <- as.character(beliefSets)
  beliefSets <- beliefSets[beliefSets != ""]
  defaultForecast <- sets_for_summary[i, ]$defaultForecast50
  specialty <- sets_for_summary[i, ]$specialty
  expectedRisk <- sets_for_summary[i, ]$expectedRisk
  forecastMin <- sets_for_summary[i, ]$forecastMin
  forecastMax <- sets_for_summary[i, ]$forecastMax
  # For each stage
  for (j in 1:length(unique(sets_for_summary$stage))) {
    print(paste("Stage:", j))
    currentStage <- j
    # For each year
    for (k in 1:length(years)) {
      print(years[k])
      sn <- currentSetName
      otherBeliefSets <- beliefSets[!grepl("Your", beliefSets)]
      medianBeliefs <- set_summary %>%
        filter(setName == sn) %>%
        filter(year == years[k])
      # For experts and supers
      for (l in 1:length(otherBeliefSets)) {
        print(otherBeliefSets[l])
        # Get reciprocal forecasts
        phase_csv <- forecasts %>%
          filter(setName == sn) %>%
          filter(grepl(years[k], questionName)) %>%
          filter(grepl(otherBeliefSets[l], questionName)) %>%
          filter(stage == 1)
        csv_users <- unique(phase_csv$userId)
        final_csv <- data.frame()
        for (m in 1:length(csv_users)) {
          user_final_forecast <- phase_csv %>%
            filter(userId == csv_users[m]) %>%
            filter(timestampId == max(timestampId))
          final_csv <- rbind(final_csv, user_final_forecast)
        }
        phase_csv <- final_csv
        # Remove default forecasts
        phase_csv <- phase_csv %>%
          filter(forecast != defaultForecast)
        # Add groups
        phase_csv <- phase_csv %>%
          rowwise() %>%
          mutate(group = case_when(
            userId %in% supers ~ "supers",
            userId %in% experts$userId ~ "experts"
          ))
        # Add reciprocal scores
        if (grepl("Experts", otherBeliefSets[l])) {
          phase_csv <- phase_csv %>%
            mutate(score_unincentivized = log_score(medianBeliefs$domainExpertsMedian, forecast))
        } else if (grepl("Superforecasters", otherBeliefSets[l])) {
          phase_csv <- phase_csv %>%
            mutate(score_unincentivized = log_score(medianBeliefs$supersMedian, forecast))
        } else {
          print("ERROR!!!")
        }
        if (nrow(phase_csv[is.nan(phase_csv$score_unincentivized), ]) > 0) {
          phase_csv[is.nan(phase_csv$score_unincentivized), ]$score_unincentivized <- -Inf
        }
        # Add ranks
        phase_csv <- phase_csv %>%
          ungroup() %>%
          mutate(rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep"))
        # Score only for stage 1
        if (j == 1) {
          for (m in 1:nrow(phase_csv)) {
            if (phase_csv$userId[m] %in% RSRanking_unincentivized$userId) {
              increment <- RSRanking_unincentivized[RSRanking_unincentivized$userId == phase_csv$userId[m], ]$numQuestions + 1
              RSRanking_unincentivized[RSRanking_unincentivized$userId == phase_csv$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == phase_csv$userId[m], ]$rankSum + phase_csv[phase_csv$userId == phase_csv$userId[m], ]$rank_unincentivized
              RSRanking_unincentivized[RSRanking_unincentivized$userId == phase_csv$userId[m], ]$numQuestions <- increment
            } else { # initializing (this is the first time we've seen this user)
              RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                userId = phase_csv$userId[m],
                group = phase_csv$group[m],
                rankSum = phase_csv$rank_unincentivized[m],
                numQuestions = 1
              ))
            }
          }
        }
      }
    }
  }
}
# get avgRank
RSRanking_unincentivized <- RSRanking_unincentivized %>% mutate(avgRank = rankSum / numQuestions)
