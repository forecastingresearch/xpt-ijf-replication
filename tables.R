library(dplyr)
library(boot)
setwd("data")

supers <- read.csv("supers.csv")
supers <- supers$x
experts <- read.csv("experts.csv")
survey <- read.csv("survey.csv") %>%
  rename("Engineered pathogen catastrophe" = "Engineered.pathogen.catastrophe") %>%
  rename("Engineered pathogen extinction" = "Engineered.pathogen.extinction") %>%
  rename("Natural pathogen catastrophe" = "Natural.pathogen.catastrophe") %>%
  rename("Natural pathogen extinction" = "Natural.pathogen.extinction")

forecasts <- read.csv("forecasts.csv")
public_survey <- read.csv("public_survey.csv")
names(public_survey) <- c("3. AI Catastrophic Risk", "4. AI Extinction Risk", "5. Nuclear Catastrophic Risk", "6. Nuclear Extinction Risk", "7. Non-Anthropogenic Catastrophic Risk", "8. Non-Anthropogenic Extinction Risk", "9. Total Catastrophic Risk", "10. Total Extinction Risk")

#Create table
boot_results <- function(plotTable, statistic = "median", width = 0.95) {
  
  ## bootstrapping confidence intervals for the given calculated statistic 
  ## of the data set plotTable
  
  set.seed(123)
  
  stat_fun <- match.fun(statistic)
  
  # If plotTable is NOT a dataframe, make it one
  if (!is.data.frame(plotTable)) {
    plotTable <- data.frame(plotTable) %>%
      rename(forecast = plotTable)
  }
  
  # If plotTable is empty, return NA's
  if (nrow(plotTable) == 0) {
    return(data.frame(confint_lower = NA, confint_upper = NA))
  }
  
  interval <- plotTable %>%
    do({
      x <- .$forecast
      res <- boot(x, statistic = function(x, i) stat_fun(x[i]), R = 1000)
      if (all(res$t == res$t[1], na.rm = TRUE)) {
        data.frame(confint_lower = NA, confint_upper = NA)
      } else {
        a <- boot.ci(res, conf = width, type = "perc")
        data.frame(confint_lower = a$percent[4], confint_upper = a$percent[5])
      }
    })
  return(interval)
}

assign_groups <- function(participant_data, domain){
  for(j in 1:nrow(participant_data)){
    if(participant_data$userId[j] %in% supers){
      participant_data$Group[j] <- "Superforecasters"
    } else if(participant_data$userId[j] %in% experts$userId){
      expert_details <- experts %>%
        filter(userId == participant_data$userId[j])
      if(!is.na(domain)){
        if(domain %in% (expert_details %>%
                        select(specialty1, specialty2, specialty3) %>%
                        unlist)){
          participant_data$Group[j] <- "Domain Experts"
        } else if("General" %in% (expert_details %>%
                                  select(specialty1, specialty2, specialty3) %>%
                                  unlist)){
          participant_data$Group[j] <- "General X-risk Experts"
        } else{
          participant_data$Group[j] <- "Non-Domain Experts"
        }
      } else if("General" %in% (expert_details %>%
                                select(specialty1, specialty2, specialty3) %>%
                                unlist)){
        participant_data$Group[j] <- "General X-risk Experts"
      } else{
        participant_data$Group[j] <- "Experts"
      }
    }
  }
  return(participant_data)
}

create_forecast_table <- function(vars){
  ## creates table with median forecasts and confidence interval for selected
  ## question variables
  forecast_table <- data.frame()
  for(i in 1:nrow(vars)){
    domain <- vars$domain[i]
    if(grepl("^\\d+\\.", vars$title[i])){
      participant_data <- forecasts %>%
        filter(setName == vars$title[i]) %>%
        filter(grepl("2100", questionName)) %>%
        filter(grepl("Your", questionName)) %>%
        filter(isCurrent == TRUE) %>%
        #exclude default forecast
        filter(forecast != 50) %>%
        mutate(Group = NA)
      participant_data <- assign_groups(participant_data, domain)
      if(vars$title[i] %in% names(public_survey)){
        public_data <- public_survey %>%
          select(vars$title[i])
        names(public_data) <- c("forecast")
        public_data <- public_data %>%
          mutate(Group = "Public survey") %>%
          # exclude out of range forecasts
          filter(forecast >= 0) %>%
          filter(forecast <= 100)
      } else{
        public_data <- NA
      }
      participant_data <- participant_data %>%
        select(forecast, Group)
      if(length(public_data) > 1){
        tbl_data <- rbind(participant_data, public_data)
      } else{
        tbl_data <- participant_data
      }
      tbl <- tbl_data %>%
        group_by(Group) %>%
        summarize(
          n_ids = n(),
          median = median(forecast),
          median_confint = boot_results(forecast, statistic = "median")
        ) %>%
        mutate(Forecast = vars$title[i])
    } else {
      participant_data <- survey %>%
        select(userId, all_of(vars$title[i]))
      participant_data <- assign_groups(participant_data, domain)
      participant_data <- participant_data %>%
        select(-userId)
      names(participant_data) <- c("forecast", "Group")
      participant_data <- participant_data %>%
        filter(!is.na(forecast))
      tbl <- participant_data %>%
        group_by(Group) %>%
        summarize(
          n_ids = n(),
          median = median(forecast, na.rm=TRUE),
          median_confint = boot_results(forecast, statistic = "median")
        ) %>%
        mutate(Forecast = vars$title[i])
    }
    forecast_table <- rbind(forecast_table, tbl)
  }
  return(forecast_table)
}

#Table 1 - catastropic risks
t1_vars <- data.frame(title=c("3. AI Catastrophic Risk", "Engineered pathogen catastrophe", "Natural pathogen catastrophe", "5. Nuclear Catastrophic Risk", "7. Non-Anthropogenic Catastrophic Risk", "9. Total Catastrophic Risk"),
                      domain=c("AI", "Biorisk", "Biorisk", "Nuclear", NA, NA))
t1 <- create_forecast_table(t1_vars)


#Table 2 - extinction risks
t2_vars <- data.frame(title=c("4. AI Extinction Risk", "Engineered pathogen extinction", "Natural pathogen extinction", "6. Nuclear Extinction Risk", "8. Non-Anthropogenic Extinction Risk", "10. Total Extinction Risk"),
                      domain=c("AI", "Biorisk", "Biorisk", "Nuclear", NA, NA))
t2 <- create_forecast_table(t2_vars)

setwd("../tables")

write.csv(t1, "t1.csv", row.names=FALSE)
write.csv(t2, "t2.csv", row.names=FALSE)
