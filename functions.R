rs_quintile_plot <- function(tbl, title, subtitle) {
  #' Boxplot for reciprocal score quintiles
  #'
  #' @importFrom ncar Round
  #' @export
  
  plot <- ggplot(tbl, aes(x = quintile, y = forecast, color = userType)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Quintile") +
    labs(title = title, subtitle = subtitle) +
    theme_bw() +
    coord_trans(y = pseudo_log_trans(base = 10), ylim = c(0, 100)) +
    scale_y_continuous(breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100)) +
    scale_color_manual(values = unlist(group_colors)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge(), aes(x = quintile, y = forecast, group = userType)) +
    stat_summary(
      fun.y = median, geom = "label", aes(label = Round(..y.., 4)),
      position = position_dodge2(width = 0.75, preserve = "single"),
      vjust = 0.5,
      size = 3,
      fill = "white",
      show.legend = FALSE
    )
  
  plot$labels$colour <- ""
  plot$labels$fill <- ""
  
  return(plot)
}

# Sometimes these files have had userName taken out; if that's the case, assign userId to userName # nolint
assign_user_id <- function(df) {
  if (!"userName" %in% names(df)) {
    df$userName <- df$userId
  }
  return(df)
}


multiYearReciprocal <- function(metaTable, 
                                data) {
  #' Stats for the multi-year questions
  #'
  #' @description For the question sets that asked about a forecaster's own
  #' beliefs and what they thought the supers and the experts believed.
  #' This function builds out a folder structure and populates it with stats and graphs for these questions.
  #'
  #' @note All reciprocal questions were multi-year.
  #'
  #' @param metaTable - metadata for all the multi-year reciprocal question sets
  #' @param data - data for all users on all multi-year reciprocal question sets
  #'
  #' @export
  
  newAdd <- newAddInit()
  
  # For each of the risk categories (genetically engineered pathogen risk, AI catastrophe, etc.)
  for (i in 1:length(unique(metaTable$setName))) {
    
    currentSetName <- unique(metaTable$setName)[i]
    
    # checks if currentSetName has been selected for the analysis before proceeding
    if (currentSetName %in% selectSets) {
      print(currentSetName)
      if (dir.exists(currentSetName)) {
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName))
      } else {
        setwd(paste0(yourHome, "data/Summary Data"))
        dir.create(currentSetName)
        setwd(currentSetName)
      }
      
      # PHASE DATA
      
      if (dir.exists("Phase Data")) {
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data"))
      } else {
        dir.create("Phase Data")
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data"))
      }
      
      # Time horizons for the years selected for the analysis
      years <- metaTable[i, ] %>% select(all_of(selectYears))
      years <- as.character(years)
      years <- years[years != ""]
      
      # Belief sets: yours, other experts', other superforecasters'
      beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
      beliefSets <- as.character(beliefSets)
      beliefSets <- beliefSets[beliefSets != ""]
      
      # TRUE/FALSE numerate citizens flag
      numerateCitizens <- metaTable[i, ]$numerateCitizens
      
      # y-axis labels
      yLabel <- metaTable[i, ]$yLabels
      
      for (j in 1:length(years)) {
        if (!dir.exists(years[j])) {
          dir.create(years[j])
          setwd(years[j])
        } else {
          setwd(years[j])
        }
        for (k in 1:length(beliefSets)) {
          if (!dir.exists(beliefSets[k])) {
            dir.create(beliefSets[k])
          }
        }
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data"))
      }
      
      defaultForecast <- metaTable[i, ]$defaultForecast50
      
      specialty <- metaTable[i, ]$specialty
      
      expectedRisk <- metaTable[i, ]$expectedRisk
      forecastMin <- metaTable[i, ]$forecastMin
      forecastMax <- metaTable[i, ]$forecastMax
      
      # For each (year, belief set, user) combination, calculate the reciprocal score.
      for (j in 1:length(unique(metaTable$stage))) {
        
        currentStage <- unique(metaTable$stage)[j]
        
        # check if currentStage was selected for analysis
        if (currentStage %in% selectStages) {
          print(paste("Stage:", currentStage))
          
          for (k in 1:length(beliefSets)) {
            print(beliefSets[k])
            for (l in 1:length(years)) {
              print(years[l])
              currentQuestionName <- paste(years[l], beliefSets[k])
              
              setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[l], "/", beliefSets[k]))
              
              questionDataRaw <- data %>%
                filter(setName == currentSetName) %>%
                filter(questionName == currentQuestionName)
              users <- unique(questionDataRaw$userName)
              users <- users[(users %in% c(supers, expertsG1$userName))]
              
              # get most recent forecast for each respondent
              questionDataProcessed <- data.frame()
              for (m in 1:length(users)) {
                user <- users[m]
                userForecasts <- questionDataRaw %>%
                  filter(userName == user) %>%
                  filter(stage <= currentStage)
                mostRecentForecast <- userForecasts %>% filter(forecastId == max(forecastId))
                if (nrow(mostRecentForecast) > 1) {
                  mostRecentForecast <- mostRecentForecast %>% filter(timestampId == max(mostRecentForecast$timestampId))
                }
                questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
              }
              
              questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)
              
              write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, ".csv"), row.names = FALSE)
              
              setwd(yourHome)
              
              setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[l], "/", beliefSets[k]))
              
              # calculates all statistics for the given subset of forecasts selected
              newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName, answerText = "", stage = currentStage, specialty = metaTable[i, ]$specialty)
              newAdd <- rbind(newAdd, newRow)
            } 
          }
        }
      }
    }
  }
  return(newAdd)
}

multiYearReciprocal_RS <- function(metaTable, data, summaryTable) {
  #' Scores the multi-year reciprocal questions
  #'
  #' @description This scores everyone who answered the reciprocal questions
  #' (What do you believe? Ok, what do you think the [other supers, experts]
  #' believe?). They were asked about the MEDIAN belief of two groups.
  #'
  #' Scoring rule:
  #' `log(1 - abs(actual median - what you thought the median would be))`
  #'
  #' +------------+-----------------------+-------------------------------------+ # nolint
  #' |     x      |       A. Actual       | B. What _ believes Group X will say | # nolint
  #' +------------+-----------------------+-------------------------------------+ # nolint
  #' | 1. Yours   | Your true belief      | Your belief about Group X           | # nolint
  #' | 2. Group X | True Group X (median) | What Group X believes about itself  | # nolint
  #' +------------+-----------------------+-------------------------------------+ # nolint
  #'
  #' This produces two kinds of scores:
  #' score_unincentivized compares A2 to B1, and
  #' score_rs compares B2 to B1
  #'
  #' @param metaTable Metadata on the multi-year reciprocal questions
  #' @param data Data on the multi-year reciprocal questions (forecasts)
  #' @param summaryTable TODO
  #'
  #' @export
  
  # For each of the question sets (1. Genetically Engineered Pathogens, etc.)
  for (i in 1:length(unique(metaTable$setName))) {
    
    currentSetName <- unique(metaTable$setName)[i]
    
    # checks if currentSetName has been selected for the analysis before proceeding
    if (currentSetName %in% selectSets) {
      print(currentSetName)
      
      # Create the directory for the question set if it doesn't exist
      if (dir.exists(currentSetName)) {
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName))
      } else {
        setwd(paste0(yourHome, "data/Summary Data"))
        dir.create(currentSetName)
        setwd(currentSetName)
      }
      
      # PHASE DATA
      
      if (dir.exists("Phase Data")) {
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data"))
      } else {
        dir.create("Phase Data")
        setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data"))
      }
      
      # Time horizons for the years selected for the analysis
      years <- metaTable[i, ] %>% select(all_of(selectYears))
      years <- as.character(years)
      years <- years[years != ""]
      
      beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs,
                                              superBeliefs)
      beliefSets <- as.character(beliefSets)
      beliefSets <- beliefSets[beliefSets != ""]
      
      defaultForecast <- metaTable[i, ]$defaultForecast50
      
      specialty <- metaTable[i, ]$specialty
      
      expectedRisk <- metaTable[i, ]$expectedRisk
      forecastMin <- metaTable[i, ]$forecastMin
      forecastMax <- metaTable[i, ]$forecastMax
      
      for (j in 1:length(unique(metaTable$stage))) {
        
        # check if the stage was selected for analysis
        if (unique(metaTable$stage)[j] %in% selectStages) {
          print(paste("Stage:", (unique(metaTable$stage)[j])))
          currentStage <- unique(metaTable$stage)[j]
          for (k in 1:length(years)) {
            print(years[k])
            
            if (dir.exists(years[k])) {
              setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[k]))
            } else {
              dir.create(years[k])
              setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[k]))
            }
            
            sn <- currentSetName
            
            # extract median beliefs for each group
            otherBeliefSets <- beliefSets[!grepl("Your", beliefSets)]
            
            medianBeliefs <- summaryTable %>%
              filter(currentSetName == sn) %>%
              filter(currentQuestionName == paste(years[k], beliefSets[grepl(
                "Your",
                beliefSets
              )])) %>%
              filter(stage == j) %>%
              select(
                currentSetName, currentQuestionName, answerText, stage,
                specialty, supersMedian, expertsG1Median, domainExpertsMedian
              )
            
            if (specialty == "") {
              medianBeliefs$domainExpertsMedian <- medianBeliefs$expertsG1Median
            }
            
            for (l in 1:length(otherBeliefSets)) {
              print(otherBeliefSets[l])
              setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[k], "/", otherBeliefSets[l]))
              files <- list.files()
              phase_csv <- read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)])
              
              # calculate unincentivized scores
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
              
              RSBeliefs <- summaryTable %>%
                filter(currentSetName == sn) %>%
                filter(currentQuestionName == paste(years[k], otherBeliefSets[l])) %>%
                filter(stage == j) %>%
                select(currentSetName, currentQuestionName, answerText, stage,
                       specialty, supersMedian, expertsG1Median,
                       domainExpertsMedian)
              
              if (specialty == "") {
                RSBeliefs$domainExpertsMedian <- RSBeliefs$expertsG1Median
              }
              
              # calculate reciprocal scores
              if (grepl("Experts", otherBeliefSets[l])) {
                phase_csv <- phase_csv %>%
                  mutate(score_rs = log_score(RSBeliefs$domainExpertsMedian, forecast))
              } else if (grepl("Superforecasters", otherBeliefSets[l])) {
                phase_csv <- phase_csv %>%
                  mutate(score_rs = log_score(RSBeliefs$supersMedian, forecast))
              } else {
                print("ERROR!!!")
              }
              
              if (nrow(phase_csv[is.nan(phase_csv$score_rs), ]) > 0) {
                phase_csv[is.nan(phase_csv$score_rs), ]$score_rs <- -Inf
              }
              
              if (dir.exists("Reciprocal Scoring")) {
                setwd("Reciprocal Scoring")
              } else {
                dir.create("Reciprocal Scoring")
                setwd("Reciprocal Scoring")
              }
              
              phase_csv <- mutate(phase_csv, group = "")
              
              for (m in 1:nrow(phase_csv)) {
                if (phase_csv$userName[m] %in% supers) {
                  phase_csv$group[m] <- "supers"
                } else if (phase_csv$userName[m] %in% expertsG1$userName) {
                  if (specialty != "" & specialty %in% unlist(expertsG1 %>% filter(userId == phase_csv$userId[m]) %>% select(specialty1, specialty2, specialty3))) {
                    phase_csv$group[m] <- "domain experts"
                  } else {
                    phase_csv$group[m] <- "non-domain experts"
                  }
                } else if (phase_csv$userName[m] %in% expertsG2) {
                  phase_csv$group[m] <- "experts (g2)"
                }
              }
              
              # rank scores
              phase_csv <- phase_csv %>%
                mutate(rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep"))
              phase_csv <- phase_csv %>%
                mutate(rank_rs = rank(-score_rs, ties.method = "min", na.last = "keep"))
              
              # This is the first csv we write: everyone's score on each question
              csv <- select(phase_csv, userId, teamId, setName, setId,
                            questionName, questionId, answerText, answerId,
                            score_unincentivized, score_rs, rank_unincentivized,
                            rank_rs, group)
              csv <- unique(csv)
              
              write.csv(csv, paste0(currentSetName, " - ", csv$questionName[1],
                                    " - Reciprocal Scores.csv"), row.names = FALSE)
              
              setwd(paste0(yourHome, "data/Summary Data"))
              
              # Group 1 (supers and "G1" experts)
              g1RS <- csv %>%
                filter(group %in% c("supers", "domain experts",
                                    "non-domain experts"))
              # Supers by themselves
              supersRS <- csv %>% filter(group == "supers")
              # G1 experts by themselves
              expertsG1RS <- csv %>%
                filter(group %in% c("domain experts", "non-domain experts"))
              # G2 experts by themselves
              expertsG2RS <- csv %>% filter(group == "experts (g2)")
              # Domain experts (varies by question)
              domainExpertsRS <- csv %>% filter(group == "domain experts")
              # Non-domain experts (varies by question)
              nonDomainExpertsRS <- csv %>% filter(group == "non-domain experts")
              
              RSTable <- read.csv("RSTable.csv")
              RSTable <- rbind(RSTable, data.frame(
                currentSetName = currentSetName,
                currentQuestionName = csv$questionName[1],
                answerText = csv$answerText[1],
                stage = j,
                specialty = specialty,
                g1Mean_unincentivized = mean(g1RS$score_unincentivized),
                supersMean_unincentivized = mean(supersRS$score_unincentivized),
                expertsG1Mean_unincentivized = mean(expertsG1RS$score_unincentivized),
                expertsG2Mean_unincentivized = mean(expertsG2RS$score_unincentivized),
                domainExpertsMean_unincentivized = mean(domainExpertsRS$score_unincentivized),
                nonDomainExpertsMean_unincentivized = mean(nonDomainExpertsRS$score_unincentivized),
                g1Mean_RS = mean(g1RS$score_rs),
                supersMean_RS = mean(supersRS$score_rs),
                expertsG1Mean_RS = mean(expertsG1RS$score_rs),
                expertsG2Mean_RS = mean(expertsG2RS$score_rs),
                domainExpertsMean_RS = mean(domainExpertsRS$score_rs),
                nonDomainExpertsMean_RS = mean(nonDomainExpertsRS$score_rs)
              ))
              
              # For each question, mean unincentivized and RS scores for each group
              write.csv(RSTable, "RSTable.csv", row.names = FALSE)
              
              RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")
              
              # rank scores
              g1RS <- g1RS %>%
                mutate(g1Rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep")) %>%
                mutate(g1Rank_RS = rank(-score_rs, ties.method = "min", na.last = "keep"))
              
              # We only want stage 1 (unincentivized: before seeing others' forecasts)
              if (j == 1) {
                for (m in 1:nrow(g1RS)) {
                  if (g1RS$userId[m] %in% RSRanking_unincentivized$userId) {
                    increment <- RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$numQuestions + 1
                    RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$rankSum + g1RS[g1RS$userId == g1RS$userId[m], ]$g1Rank_unincentivized
                    RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$numQuestions <- increment
                  } else { # initializing (this is the first time we've seen this user)
                    RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                      userId = g1RS$userId[m],
                      group = g1RS$group[m],
                      rankSum = g1RS$g1Rank_unincentivized[m],
                      numQuestions = 1
                    ))
                  }
                }
              }
              
              # Write the G1 rankings unincentivized RS rankings to CSV
              write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)
              
              setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[k], "/", otherBeliefSets[l]))
            }
            
            setwd(paste0(yourHome, "data/Summary Data/", currentSetName, "/Phase Data/", years[k]))
          }
        }
      }
    }
    setwd(paste0(yourHome, "data/Summary Data"))
  }
}

newAddInit <- function() {
  #' Initialize empty dataframe to hold the summary stats.
  #'
  #' @return Dataframe (wide) with one row per (question, stage) pair, gives aggregates
  #' with columns like:
  #' specialty - nuclear, biorisk, etc
  #' pct* - everyone
  #' g1* - group 1 (highest-paid experts & ALL of the supers)
  #' g2* - group 2 (lower tier, experts only)
  #' domainExperts - ONLY the g1 experts from the domain of the question
  #' nonDomainExperts - experts from not the domain of the question, and supers
  #' *_exc - filter out extreme outliers (based on standard deviation; different from trimmed mean)
  #'
  #' @export
  
  return(data.frame(
    setName = character(0),
    questionName = character(0),
    answerText = character(0),
    stage = numeric(0),
    specialty = character(0),
    mean = numeric(0),
    median = numeric(0),
    sd = numeric(0),
    n = numeric(0),
    trimmedMean = numeric(0),
    pct5th = numeric(0),
    pct25th = numeric(0),
    pct75th = numeric(0),
    pct95th = numeric(0),
    geoMean = numeric(0),
    hdTrim = numeric(0),
    neymanAgg = numeric(0),
    g1Mean = numeric(0),
    g1Median = numeric(0),
    g1Sd = numeric(0),
    g1N = numeric(0),
    g1TrimmedMean = numeric(0),
    g1Pct5th = numeric(0),
    g1Pct25th = numeric(0),
    g1Pct75th = numeric(0),
    g1Pct95th = numeric(0),
    g1GeoMean = numeric(0),
    g1HdTrim = numeric(0),
    g1NeymanAgg = numeric(0),
    supersMean = numeric(0),
    supersMedian = numeric(0),
    supersSd = numeric(0),
    supersN = numeric(0),
    supersTrimmedMean = numeric(0),
    supersPct5th = numeric(0),
    supersPct25th = numeric(0),
    supersPct75th = numeric(0),
    supersPct95th = numeric(0),
    supersGeoMean = numeric(0),
    supersHdTrim = numeric(0),
    supersNeymanAgg = numeric(0),
    expertsG1Mean = numeric(0),
    expertsG1Median = numeric(0),
    expertsG1Sd = numeric(0),
    expertsG1N = numeric(0),
    expertsG1TrimmedMean = numeric(0),
    expertsG1Pct5th = numeric(0),
    expertsG1Pct25th = numeric(0),
    expertsG1Pct75th = numeric(0),
    expertsG1Pct95th = numeric(0),
    expertsG1GeoMean = numeric(0),
    expertsG1HdTrim = numeric(0),
    expertsG1NeymanAgg = numeric(0),
    xRiskGeneralistsMean = numeric(0),
    xRiskGeneralistsMedian = numeric(0),
    xRiskGeneralistsSd = numeric(0),
    xRiskGeneralistsN = numeric(0),
    xRiskGeneralistsTrimmedMean = numeric(0),
    xRiskGeneralistsPct5th = numeric(0),
    xRiskGeneralistsPct25th = numeric(0),
    xRiskGeneralistsPct75th = numeric(0),
    xRiskGeneralistsPct95th = numeric(0),
    xRiskGeneralistsGeoMean = numeric(0),
    xRiskGeneralistsHdTrim = numeric(0),
    xRiskGeneralistsNeymanAgg = numeric(0),
    domainExpertsMean = numeric(0),
    domainExpertsMedian = numeric(0),
    domainExpertsSd = numeric(0),
    domainExpertsN = numeric(0),
    domainExpertsTrimmedMean = numeric(0),
    domainExpertsPct5th = numeric(0),
    domainExpertsPct25th = numeric(0),
    domainExpertsPct75th = numeric(0),
    domainExpertsPct95th = numeric(0),
    domainExpertsGeoMean = numeric(0),
    domainExpertsHdTrim = numeric(0),
    domainExpertsNeymanAgg = numeric(0),
    nonDomainExpertsMean = numeric(0),
    nonDomainExpertsMedian = numeric(0),
    nonDomainExpertsSd = numeric(0),
    nonDomainExpertsN = numeric(0),
    nonDomainExpertsTrimmedMean = numeric(0),
    nonDomainExpertsPct5th = numeric(0),
    nonDomainExpertsPct25th = numeric(0),
    nonDomainExpertsPct75th = numeric(0),
    nonDomainExpertsPct95th = numeric(0),
    nonDomainExpertsGeoMean = numeric(0),
    nonDomainExpertsHdTrim = numeric(0),
    nonDomainExpertsNeymanAgg = numeric(0),
    mean_exc = numeric(0),
    median_exc = numeric(0),
    sd_exc = numeric(0),
    n_exc = numeric(0),
    trimmedMean_exc = numeric(0),
    pct5th_exc = numeric(0),
    pct25th_exc = numeric(0),
    pct75th_exc = numeric(0),
    pct95th_exc = numeric(0),
    geoMean_exc = numeric(0),
    hdTrim_exc = numeric(0),
    neymanAgg_exc = numeric(0),
    g1Mean_exc = numeric(0),
    g1Median_exc = numeric(0),
    g1Sd_exc = numeric(0),
    g1N_exc = numeric(0),
    g1TrimmedMean_exc = numeric(0),
    g1Pct5th_exc = numeric(0),
    g1Pct25th_exc = numeric(0),
    g1Pct75th_exc = numeric(0),
    g1Pct95th_exc = numeric(0),
    g1GeoMean_exc = numeric(0),
    g1HdTrim_exc = numeric(0),
    g1NeymanAgg_exc = numeric(0),
    supersMean_exc = numeric(0),
    supersMedian_exc = numeric(0),
    supersSd_exc = numeric(0),
    supersN_exc = numeric(0),
    supersTrimmedMean_exc = numeric(0),
    supersPct5th_exc = numeric(0),
    supersPct25th_exc = numeric(0),
    supersPct75th_exc = numeric(0),
    supersPct95th_exc = numeric(0),
    supersGeoMean_exc = numeric(0),
    supersHdTrim_exc = numeric(0),
    supersNeymanAgg_exc = numeric(0),
    expertsG1Mean_exc = numeric(0),
    expertsG1Median_exc = numeric(0),
    expertsG1Sd_exc = numeric(0),
    expertsG1N_exc = numeric(0),
    expertsG1TrimmedMean_exc = numeric(0),
    expertsG1Pct5th_exc = numeric(0),
    expertsG1Pct25th_exc = numeric(0),
    expertsG1Pct75th_exc = numeric(0),
    expertsG1Pct95th_exc = numeric(0),
    expertsG1GeoMean_exc = numeric(0),
    expertsG1HdTrim_exc = numeric(0),
    expertsG1NeymanAgg_exc = numeric(0),
    xRiskGeneralistsMean_exc = numeric(0),
    xRiskGeneralistsMedian_exc = numeric(0),
    xRiskGeneralistsSd_exc = numeric(0),
    xRiskGeneralistsN_exc = numeric(0),
    xRiskGeneralistsTrimmedMean_exc = numeric(0),
    xRiskGeneralistsPct5th_exc = numeric(0),
    xRiskGeneralistsPct25th_exc = numeric(0),
    xRiskGeneralistsPct75th_exc = numeric(0),
    xRiskGeneralistsPct95th_exc = numeric(0),
    xRiskGeneralistsGeoMean_exc = numeric(0),
    xRiskGeneralistsHdTrim_exc = numeric(0),
    xRiskGeneralistsNeymanAgg_exc = numeric(0),
    domainExpertsMean_exc = numeric(0),
    domainExpertsMedian_exc = numeric(0),
    domainExpertsSd_exc = numeric(0),
    domainExpertsN_exc = numeric(0),
    domainExpertsTrimmedMean_exc = numeric(0),
    domainExpertsPct5th_exc = numeric(0),
    domainExpertsPct25th_exc = numeric(0),
    domainExpertsPct75th_exc = numeric(0),
    domainExpertsPct95th_exc = numeric(0),
    domainExpertsGeoMean_exc = numeric(0),
    domainExpertsHdTrim_exc = numeric(0),
    domainExpertsNeymanAgg_exc = numeric(0),
    nonDomainExpertsMean_exc = numeric(0),
    nonDomainExpertsMedian_exc = numeric(0),
    nonDomainExpertsSd_exc = numeric(0),
    nonDomainExpertsN_exc = numeric(0),
    nonDomainExpertsTrimmedMean_exc = numeric(0),
    nonDomainExpertsPct5th_exc = numeric(0),
    nonDomainExpertsPct25th_exc = numeric(0),
    nonDomainExpertsPct75th_exc = numeric(0),
    nonDomainExpertsPct95th_exc = numeric(0),
    nonDomainExpertsGeoMean_exc = numeric(0),
    nonDomainExpertsHdTrim_exc = numeric(0),
    nonDomainExpertsNeymanAgg_exc = numeric(0)
  ))
}

newRowInit <- function(metaTable, questionDataProcessed, currentSetName,
                       currentQuestionName, answerText, stage, specialty) {
  
  ## calculates summary statistics for the given questionDataProcessed

  mean <- mean(questionDataProcessed$forecast)
  median <- median(questionDataProcessed$forecast)
  sd <- sd(questionDataProcessed$forecast)
  n <- nrow(questionDataProcessed)
  trimmedMean <- aggutils::trim(questionDataProcessed$forecast)
  pct5th <- as.numeric(quantile(questionDataProcessed$forecast, 0.05))
  pct25th <- as.numeric(quantile(questionDataProcessed$forecast, 0.25))
  pct75th <- as.numeric(quantile(questionDataProcessed$forecast, 0.75))
  pct95th <- as.numeric(quantile(questionDataProcessed$forecast, 0.95))
  geoMean <- geoMeanCalc(questionDataProcessed$forecast)
  hdTrim <- hd_trim(questionDataProcessed$forecast)
  neymanAgg <- neymanAggCalc(questionDataProcessed$forecast)
  
  g1Processed <- questionDataProcessed %>% filter(userName %in% c(supers, expertsG1$userName))
  g1Mean <- mean(g1Processed$forecast)
  g1Median <- median(g1Processed$forecast)
  g1Sd <- sd(g1Processed$forecast)
  g1N <- nrow(g1Processed)
  g1TrimmedMean <- aggutils::trim(g1Processed$forecast)
  g1Pct5th <- as.numeric(quantile(g1Processed$forecast, 0.05))
  g1Pct25th <- as.numeric(quantile(g1Processed$forecast, 0.25))
  g1Pct75th <- as.numeric(quantile(g1Processed$forecast, 0.75))
  g1Pct95th <- as.numeric(quantile(g1Processed$forecast, 0.95))
  g1GeoMean <- geoMeanCalc(g1Processed$forecast)
  g1HdTrim <- hd_trim(g1Processed$forecast)
  g1NeymanAgg <- neymanAggCalc(g1Processed$forecast)
  
  supersProcessed <- questionDataProcessed %>% filter(userName %in% supers)
  supersMean <- mean(supersProcessed$forecast)
  supersMedian <- median(supersProcessed$forecast)
  supersSd <- sd(supersProcessed$forecast)
  supersN <- nrow(supersProcessed)
  supersTrimmedMean <- aggutils::trim(supersProcessed$forecast)
  supersPct5th <- as.numeric(quantile(supersProcessed$forecast, 0.05))
  supersPct25th <- as.numeric(quantile(supersProcessed$forecast, 0.25))
  supersPct75th <- as.numeric(quantile(supersProcessed$forecast, 0.75))
  supersPct95th <- as.numeric(quantile(supersProcessed$forecast, 0.95))
  supersGeoMean <- geoMeanCalc(supersProcessed$forecast)
  supersHdTrim <- hd_trim(supersProcessed$forecast)
  supersNeymanAgg <- neymanAggCalc(supersProcessed$forecast)
  
  expertsG1Processed <- questionDataProcessed %>% filter(userName %in% expertsG1$userName)
  expertsG1Mean <- mean(expertsG1Processed$forecast)
  expertsG1Median <- median(expertsG1Processed$forecast)
  expertsG1Sd <- sd(expertsG1Processed$forecast)
  expertsG1N <- nrow(expertsG1Processed)
  expertsG1TrimmedMean <- aggutils::trim(expertsG1Processed$forecast)
  expertsG1Pct5th <- as.numeric(quantile(expertsG1Processed$forecast, 0.05))
  expertsG1Pct25th <- as.numeric(quantile(expertsG1Processed$forecast, 0.25))
  expertsG1Pct75th <- as.numeric(quantile(expertsG1Processed$forecast, 0.75))
  expertsG1Pct95th <- as.numeric(quantile(expertsG1Processed$forecast, 0.95))
  expertsG1GeoMean <- geoMeanCalc(expertsG1Processed$forecast)
  expertsG1HdTrim <- hd_trim(expertsG1Processed$forecast)
  expertsG1NeymanAgg <- neymanAggCalc(expertsG1Processed$forecast)
  
  xRiskGeneralists <- (expertsG1 %>% filter(specialty1 == "General" | specialty2 == "General" | specialty3 == "General"))$userName
  xRiskGeneralistsProcessed <- questionDataProcessed %>% filter(userName %in% xRiskGeneralists)
  xRiskGeneralistsMean <- mean(xRiskGeneralistsProcessed$forecast)
  xRiskGeneralistsMedian <- median(xRiskGeneralistsProcessed$forecast)
  xRiskGeneralistsSd <- sd(xRiskGeneralistsProcessed$forecast)
  xRiskGeneralistsN <- nrow(xRiskGeneralistsProcessed)
  xRiskGeneralistsTrimmedMean <- aggutils::trim(xRiskGeneralistsProcessed$forecast)
  xRiskGeneralistsPct5th <- as.numeric(quantile(xRiskGeneralistsProcessed$forecast, 0.05))
  xRiskGeneralistsPct25th <- as.numeric(quantile(xRiskGeneralistsProcessed$forecast, 0.25))
  xRiskGeneralistsPct75th <- as.numeric(quantile(xRiskGeneralistsProcessed$forecast, 0.75))
  xRiskGeneralistsPct95th <- as.numeric(quantile(xRiskGeneralistsProcessed$forecast, 0.95))
  xRiskGeneralistsGeoMean <- geoMeanCalc(xRiskGeneralistsProcessed$forecast)
  if (nrow(xRiskGeneralistsProcessed) > 0) {
    xRiskGeneralistsHdTrim <- hd_trim(xRiskGeneralistsProcessed$forecast)
  } else {
    xRiskGeneralistsHdTrim <- NA
  }
  xRiskGeneralistsNeymanAgg <- neymanAggCalc(xRiskGeneralistsProcessed$forecast)
  
  
  if (specialty != "") {
    currentSpecialty <- specialty
    specialists <- (expertsG1 %>% filter(specialty1 == specialty | specialty2 == specialty | specialty3 == specialty))$userName
    domainExpertsProcessed <- questionDataProcessed %>% filter(userName %in% specialists)
    domainExpertsMean <- mean(domainExpertsProcessed$forecast)
    domainExpertsMedian <- median(domainExpertsProcessed$forecast)
    domainExpertsSd <- sd(domainExpertsProcessed$forecast)
    domainExpertsN <- nrow(domainExpertsProcessed)
    domainExpertsTrimmedMean <- aggutils::trim(domainExpertsProcessed$forecast)
    domainExpertsPct5th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.05))
    domainExpertsPct25th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.25))
    domainExpertsPct75th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.75))
    domainExpertsPct95th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.95))
    domainExpertsGeoMean <- geoMeanCalc(domainExpertsProcessed$forecast)
    if (nrow(domainExpertsProcessed) > 0) {
      domainExpertsHdTrim <- hd_trim(domainExpertsProcessed$forecast)
    } else {
      domainExpertsHdTrim <- NA
    }
    domainExpertsNeymanAgg <- neymanAggCalc(domainExpertsProcessed$forecast)
  } else {
    domainExpertsMean <- NA
    domainExpertsMedian <- NA
    domainExpertsSd <- NA
    domainExpertsN <- NA
    domainExpertsTrimmedMean <- NA
    domainExpertsPct5th <- NA
    domainExpertsPct25th <- NA
    domainExpertsPct75th <- NA
    domainExpertsPct95th <- NA
    domainExpertsGeoMean <- NA
    domainExpertsHdTrim <- NA
    domainExpertsNeymanAgg <- NA
  }
  
  if (specialty != "") {
    nonDomainExpertsProcessed <- questionDataProcessed %>%
      filter(!(userName %in% specialists)) %>%
      filter(!(userName %in% xRiskGeneralists)) %>%
      filter(userName %in% expertsG1$userName)
    nonDomainExpertsMean <- mean(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsMedian <- median(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsSd <- sd(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsN <- nrow(nonDomainExpertsProcessed)
    nonDomainExpertsTrimmedMean <- aggutils::trim(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsPct5th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.05))
    nonDomainExpertsPct25th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.25))
    nonDomainExpertsPct75th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.75))
    nonDomainExpertsPct95th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.95))
    nonDomainExpertsGeoMean <- geoMeanCalc(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsHdTrim <- hd_trim(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsNeymanAgg <- neymanAggCalc(nonDomainExpertsProcessed$forecast)
  } else {
    nonDomainExpertsMean <- NA
    nonDomainExpertsMedian <- NA
    nonDomainExpertsSd <- NA
    nonDomainExpertsN <- NA
    nonDomainExpertsTrimmedMean <- NA
    nonDomainExpertsPct5th <- NA
    nonDomainExpertsPct25th <- NA
    nonDomainExpertsPct75th <- NA
    nonDomainExpertsPct95th <- NA
    nonDomainExpertsGeoMean <- NA
    nonDomainExpertsHdTrim <- NA
    nonDomainExpertsNeymanAgg <- NA
  }
  
  # Filter out extreme outliers
  questionDataProcessed_exc <- questionDataProcessed %>%
    filter(forecast > median - (10 * sd)) %>%
    filter(forecast < median + (10 * sd))
  mean_exc <- mean(questionDataProcessed_exc$forecast)
  median_exc <- median(questionDataProcessed_exc$forecast)
  sd_exc <- sd(questionDataProcessed_exc$forecast)
  n_exc <- nrow(questionDataProcessed_exc)
  trimmedMean_exc <- aggutils::trim(questionDataProcessed_exc$forecast)
  pct5th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.05))
  pct25th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.25))
  pct75th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.75))
  pct95th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.95))
  geoMean_exc <- geoMeanCalc(questionDataProcessed_exc$forecast)
  hdTrim_exc <- hd_trim(questionDataProcessed_exc$forecast)
  neymanAgg_exc <- neymanAggCalc(questionDataProcessed_exc$forecast)
  
  g1Processed_exc <- questionDataProcessed %>%
    filter(userName %in% c(supers, expertsG1$userName)) %>%
    filter(forecast > g1Median - (10 * g1Sd)) %>%
    filter(forecast < g1Median + (10 * g1Sd))
  g1Mean_exc <- mean(g1Processed_exc$forecast)
  g1Median_exc <- median(g1Processed_exc$forecast)
  g1Sd_exc <- sd(g1Processed_exc$forecast)
  g1N_exc <- nrow(g1Processed_exc)
  g1TrimmedMean_exc <- aggutils::trim(g1Processed_exc$forecast)
  g1Pct5th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.05))
  g1Pct25th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.25))
  g1Pct75th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.75))
  g1Pct95th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.95))
  g1GeoMean_exc <- geoMeanCalc(g1Processed_exc$forecast)
  g1HdTrim_exc <- hd_trim(g1Processed_exc$forecast)
  g1NeymanAgg_exc <- neymanAggCalc(g1Processed_exc$forecast)
  
  supersProcessed_exc <- questionDataProcessed %>%
    filter(userName %in% supers) %>%
    filter(forecast > supersMedian - (10 * supersSd)) %>%
    filter(forecast < supersMedian + (10 * supersSd))
  supersMean_exc <- mean(supersProcessed_exc$forecast)
  supersMedian_exc <- median(supersProcessed_exc$forecast)
  supersSd_exc <- sd(supersProcessed_exc$forecast)
  supersN_exc <- nrow(supersProcessed_exc)
  supersTrimmedMean_exc <- aggutils::trim(supersProcessed_exc$forecast)
  supersPct5th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.05))
  supersPct25th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.25))
  supersPct75th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.75))
  supersPct95th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.95))
  supersGeoMean_exc <- geoMeanCalc(supersProcessed_exc$forecast)
  supersHdTrim_exc <- hd_trim(supersProcessed_exc$forecast)
  supersNeymanAgg_exc <- neymanAggCalc(supersProcessed_exc$forecast)
  
  expertsG1Processed_exc <- questionDataProcessed %>%
    filter(userName %in% expertsG1$userName) %>%
    filter(forecast > expertsG1Median - (10 * expertsG1Sd)) %>%
    filter(forecast < expertsG1Median + (10 * expertsG1Sd))
  expertsG1Mean_exc <- mean(expertsG1Processed_exc$forecast)
  expertsG1Median_exc <- median(expertsG1Processed_exc$forecast)
  expertsG1Sd_exc <- sd(expertsG1Processed_exc$forecast)
  expertsG1N_exc <- nrow(expertsG1Processed_exc)
  expertsG1TrimmedMean_exc <- aggutils::trim(expertsG1Processed_exc$forecast)
  expertsG1Pct5th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.05))
  expertsG1Pct25th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.25))
  expertsG1Pct75th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.75))
  expertsG1Pct95th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.95))
  expertsG1GeoMean_exc <- geoMeanCalc(expertsG1Processed_exc$forecast)
  if (length(expertsG1Processed_exc$forecast) > 0) {
    expertsG1HdTrim_exc <- hd_trim(expertsG1Processed_exc$forecast)
  } else {
    expertsG1HdTrim_exc <- NA
  }
  expertsG1NeymanAgg_exc <- neymanAggCalc(expertsG1Processed_exc$forecast)
  
  xRiskGeneralists <- (expertsG1 %>% filter(specialty1 == "General" | specialty2 == "General" | specialty3 == "General"))$userName
  xRiskGeneralistsProcessed_exc <- questionDataProcessed %>%
    filter(userName %in% xRiskGeneralists) %>%
    filter(forecast > xRiskGeneralistsMedian - (10 * xRiskGeneralistsSd)) %>%
    filter(forecast < xRiskGeneralistsMedian + (10 * xRiskGeneralistsSd))
  xRiskGeneralistsMean_exc <- mean(xRiskGeneralistsProcessed_exc$forecast)
  xRiskGeneralistsMedian_exc <- median(xRiskGeneralistsProcessed_exc$forecast)
  xRiskGeneralistsSd_exc <- sd(xRiskGeneralistsProcessed_exc$forecast)
  xRiskGeneralistsN_exc <- nrow(xRiskGeneralistsProcessed_exc)
  xRiskGeneralistsTrimmedMean_exc <- aggutils::trim(xRiskGeneralistsProcessed_exc$forecast)
  xRiskGeneralistsPct5th_exc <- as.numeric(quantile(xRiskGeneralistsProcessed_exc$forecast, 0.05))
  xRiskGeneralistsPct25th_exc <- as.numeric(quantile(xRiskGeneralistsProcessed_exc$forecast, 0.25))
  xRiskGeneralistsPct75th_exc <- as.numeric(quantile(xRiskGeneralistsProcessed_exc$forecast, 0.75))
  xRiskGeneralistsPct95th_exc <- as.numeric(quantile(xRiskGeneralistsProcessed_exc$forecast, 0.95))
  xRiskGeneralistsGeoMean_exc <- geoMeanCalc(xRiskGeneralistsProcessed_exc$forecast)
  if (nrow(xRiskGeneralistsProcessed_exc) > 0) {
    xRiskGeneralistsHdTrim_exc <- hd_trim(xRiskGeneralistsProcessed_exc$forecast)
  } else {
    xRiskGeneralistsHdTrim_exc <- NA
  }
  xRiskGeneralistsNeymanAgg_exc <- neymanAggCalc(xRiskGeneralistsProcessed_exc$forecast)
  
  if (specialty != "") {
    currentSpecialty <- specialty
    specialists <- (expertsG1 %>% filter(specialty1 == specialty | specialty2 == specialty | specialty3 == specialty))$userName
    domainExpertsProcessed_exc <- questionDataProcessed %>%
      filter(userName %in% specialists) %>%
      filter(forecast > domainExpertsMedian - (10 * domainExpertsSd)) %>%
      filter(forecast < domainExpertsMedian + (10 * domainExpertsSd))
    domainExpertsMean_exc <- mean(domainExpertsProcessed_exc$forecast)
    domainExpertsMedian_exc <- median(domainExpertsProcessed_exc$forecast)
    domainExpertsSd_exc <- sd(domainExpertsProcessed_exc$forecast)
    domainExpertsN_exc <- nrow(domainExpertsProcessed_exc)
    domainExpertsTrimmedMean_exc <- aggutils::trim(domainExpertsProcessed_exc$forecast)
    domainExpertsPct5th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.05))
    domainExpertsPct25th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.25))
    domainExpertsPct75th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.75))
    domainExpertsPct95th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.95))
    domainExpertsGeoMean_exc <- geoMeanCalc(domainExpertsProcessed_exc$forecast)
    if (nrow(domainExpertsProcessed_exc) > 0) {
      domainExpertsHdTrim_exc <- hd_trim(domainExpertsProcessed_exc$forecast)
    } else {
      domainExpertsHdTrim_exc <- NA
    }
    domainExpertsNeymanAgg_exc <- neymanAggCalc(domainExpertsProcessed_exc$forecast)
  } else {
    domainExpertsMean_exc <- NA
    domainExpertsMedian_exc <- NA
    domainExpertsSd_exc <- NA
    domainExpertsN_exc <- NA
    domainExpertsTrimmedMean_exc <- NA
    domainExpertsPct5th_exc <- NA
    domainExpertsPct25th_exc <- NA
    domainExpertsPct75th_exc <- NA
    domainExpertsPct95th_exc <- NA
    domainExpertsGeoMean_exc <- NA
    domainExpertsHdTrim_exc <- NA
    domainExpertsNeymanAgg_exc <- NA
  }
  
  if (specialty != "") {
    nonDomainExpertsProcessed_exc <- questionDataProcessed %>%
      filter(!(userName %in% specialists)) %>%
      filter(!(userName %in% xRiskGeneralists)) %>%
      filter(userName %in% expertsG1$userName) %>%
      filter(forecast > nonDomainExpertsMedian - (10 * nonDomainExpertsSd)) %>%
      filter(forecast < nonDomainExpertsMedian + (10 * nonDomainExpertsSd))
    nonDomainExpertsMean_exc <- mean(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsMedian_exc <- median(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsSd_exc <- sd(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsN_exc <- nrow(nonDomainExpertsProcessed_exc)
    nonDomainExpertsTrimmedMean_exc <- aggutils::trim(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsPct5th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.05))
    nonDomainExpertsPct25th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.25))
    nonDomainExpertsPct75th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.75))
    nonDomainExpertsPct95th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.95))
    nonDomainExpertsGeoMean_exc <- geoMeanCalc(nonDomainExpertsProcessed_exc$forecast)
    if (nrow(nonDomainExpertsProcessed_exc) > 0) {
      nonDomainExpertsHdTrim_exc <- hd_trim(nonDomainExpertsProcessed_exc$forecast)
    } else {
      nonDomainExpertsHdTrim_exc <- NA
    }
    nonDomainExpertsNeymanAgg_exc <- neymanAggCalc(nonDomainExpertsProcessed_exc$forecast)
  } else {
    nonDomainExpertsMean_exc <- NA
    nonDomainExpertsMedian_exc <- NA
    nonDomainExpertsSd_exc <- NA
    nonDomainExpertsN_exc <- NA
    nonDomainExpertsTrimmedMean_exc <- NA
    nonDomainExpertsPct5th_exc <- NA
    nonDomainExpertsPct25th_exc <- NA
    nonDomainExpertsPct75th_exc <- NA
    nonDomainExpertsPct95th_exc <- NA
    nonDomainExpertsGeoMean_exc <- NA
    nonDomainExpertsHdTrim_exc <- NA
    nonDomainExpertsNeymanAgg_exc <- NA
  }
  
  newRow <- data.frame(
    currentSetName,
    currentQuestionName,
    answerText,
    stage,
    specialty,
    mean,
    median,
    sd,
    n,
    trimmedMean,
    pct5th,
    pct25th,
    pct75th,
    pct95th,
    geoMean,
    hdTrim,
    neymanAgg,
    g1Mean,
    g1Median,
    g1Sd,
    g1N,
    g1TrimmedMean,
    g1Pct5th,
    g1Pct25th,
    g1Pct75th,
    g1Pct95th,
    g1GeoMean,
    g1HdTrim,
    g1NeymanAgg,
    supersMean,
    supersMedian,
    supersSd,
    supersN,
    supersTrimmedMean,
    supersPct5th,
    supersPct25th,
    supersPct75th,
    supersPct95th,
    supersGeoMean,
    supersHdTrim,
    supersNeymanAgg,
    expertsG1Mean,
    expertsG1Median,
    expertsG1Sd,
    expertsG1N,
    expertsG1TrimmedMean,
    expertsG1Pct5th,
    expertsG1Pct25th,
    expertsG1Pct75th,
    expertsG1Pct95th,
    expertsG1GeoMean,
    expertsG1HdTrim,
    expertsG1NeymanAgg,
    xRiskGeneralistsMean,
    xRiskGeneralistsMedian,
    xRiskGeneralistsSd,
    xRiskGeneralistsN,
    xRiskGeneralistsTrimmedMean,
    xRiskGeneralistsPct5th,
    xRiskGeneralistsPct25th,
    xRiskGeneralistsPct75th,
    xRiskGeneralistsPct95th,
    xRiskGeneralistsGeoMean,
    xRiskGeneralistsHdTrim,
    xRiskGeneralistsNeymanAgg,
    domainExpertsMean,
    domainExpertsMedian,
    domainExpertsSd,
    domainExpertsN,
    domainExpertsTrimmedMean,
    domainExpertsPct5th,
    domainExpertsPct25th,
    domainExpertsPct75th,
    domainExpertsPct95th,
    domainExpertsGeoMean,
    domainExpertsHdTrim,
    domainExpertsNeymanAgg,
    nonDomainExpertsMean,
    nonDomainExpertsMedian,
    nonDomainExpertsSd,
    nonDomainExpertsN,
    nonDomainExpertsTrimmedMean,
    nonDomainExpertsPct5th,
    nonDomainExpertsPct25th,
    nonDomainExpertsPct75th,
    nonDomainExpertsPct95th,
    nonDomainExpertsGeoMean,
    nonDomainExpertsHdTrim,
    nonDomainExpertsNeymanAgg,
    mean_exc,
    median_exc,
    sd_exc,
    n_exc,
    trimmedMean_exc,
    pct5th_exc,
    pct25th_exc,
    pct75th_exc,
    pct95th_exc,
    geoMean_exc,
    hdTrim_exc,
    neymanAgg_exc,
    g1Mean_exc,
    g1Median_exc,
    g1Sd_exc,
    g1N_exc,
    g1TrimmedMean_exc,
    g1Pct5th_exc,
    g1Pct25th_exc,
    g1Pct75th_exc,
    g1Pct95th_exc,
    g1GeoMean_exc,
    g1HdTrim_exc,
    g1NeymanAgg_exc,
    supersMean_exc,
    supersMedian_exc,
    supersSd_exc,
    supersN_exc,
    supersTrimmedMean_exc,
    supersPct5th_exc,
    supersPct25th_exc,
    supersPct75th_exc,
    supersPct95th_exc,
    supersGeoMean_exc,
    supersHdTrim_exc,
    supersNeymanAgg_exc,
    expertsG1Mean_exc,
    expertsG1Median_exc,
    expertsG1Sd_exc,
    expertsG1N_exc,
    expertsG1TrimmedMean_exc,
    expertsG1Pct5th_exc,
    expertsG1Pct25th_exc,
    expertsG1Pct75th_exc,
    expertsG1Pct95th_exc,
    expertsG1GeoMean_exc,
    expertsG1HdTrim_exc,
    expertsG1NeymanAgg_exc,
    xRiskGeneralistsMean_exc,
    xRiskGeneralistsMedian_exc,
    xRiskGeneralistsSd_exc,
    xRiskGeneralistsN_exc,
    xRiskGeneralistsTrimmedMean_exc,
    xRiskGeneralistsPct5th_exc,
    xRiskGeneralistsPct25th_exc,
    xRiskGeneralistsPct75th_exc,
    xRiskGeneralistsPct95th_exc,
    xRiskGeneralistsGeoMean_exc,
    xRiskGeneralistsHdTrim_exc,
    xRiskGeneralistsNeymanAgg_exc,
    domainExpertsMean_exc,
    domainExpertsMedian_exc,
    domainExpertsSd_exc,
    domainExpertsN_exc,
    domainExpertsTrimmedMean_exc,
    domainExpertsPct5th_exc,
    domainExpertsPct25th_exc,
    domainExpertsPct75th_exc,
    domainExpertsPct95th_exc,
    domainExpertsGeoMean_exc,
    domainExpertsHdTrim_exc,
    domainExpertsNeymanAgg_exc,
    nonDomainExpertsMean_exc,
    nonDomainExpertsMedian_exc,
    nonDomainExpertsSd_exc,
    nonDomainExpertsN_exc,
    nonDomainExpertsTrimmedMean_exc,
    nonDomainExpertsPct5th_exc,
    nonDomainExpertsPct25th_exc,
    nonDomainExpertsPct75th_exc,
    nonDomainExpertsPct95th_exc,
    nonDomainExpertsGeoMean_exc,
    nonDomainExpertsHdTrim_exc,
    nonDomainExpertsNeymanAgg_exc
  )
  return(newRow)
}

RSInit <- function() {
  #' Initialize reciprocal scoring dataframe.
  #'
  #' @return An empty dataframe with the reciprocal scoring columns.
  #'
  #' @export
  
  return(data.frame(
    currentSetName = character(0),
    currentQuestionName = character(0),
    answerText = character(0),
    stage = character(0),
    specialty = character(0),
    g1Mean_unincentivized = character(0),
    supersMean_unincentivized = character(0),
    expertsG1Mean_unincentivized = character(0),
    expertsG2Mean_unincentivized = character(0),
    domainExpertsMean_unincentivized = character(0),
    nonDomainExpertsMean_unincentivized = character(0),
    g1Mean_RS = character(0),
    supersMean_RS = character(0),
    expertsG1Mean_RS = character(0),
    expertsG2Mean_RS = character(0),
    domainExpertsMean_RS = character(0),
    nonDomainExpertsMean_RS = character(0)
  ))
}

RSRankingInit <- function() {
  #' Initialize reciprocal scoring ranking dataframe.
  #'
  #' @return An empty dataframe that will hold the reciprocal scoring ranking
  #' for each user.
  #'
  #' @export
  
  return(data.frame(
    userId = numeric(0),
    group = character(0),
    rankSum = numeric(0),
    numQuestions = numeric(0)
  ))
}

log_score <- function(actual, forecast) {
  #' @export
  
  return(log(1 - abs(actual / 100 - forecast / 100)))
}