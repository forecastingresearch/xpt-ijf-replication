setwd("data")

library(lubridate)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(data.table)
library(caret)
library(boot)
library(scales)
library(tidyr)

#### Loading data ####

supers <- read.csv("supers.csv")
supers <- supers$x
experts <- read.csv("experts.csv")

generalists <- experts %>%
  rowwise() %>%
  filter("General" %in% c(specialty1, specialty2, specialty3))

forecasts <- read.csv("forecasts.csv")

meta <- read.csv("metadata.csv")
meta <- meta %>% filter(stage == 4)

#### Processing ####

# extracting individual forecasts related to existential risks by 2100
all_ext_risk <- forecasts %>%
  filter(setName == "10. Total Extinction Risk") %>%
  filter(grepl("2100", questionName)) %>%
  filter(grepl("Your", questionName))
all_ext_risk <- unique(all_ext_risk)
all_ext_risk$timestamp <- mdy(all_ext_risk$timestamp)
# Remove default forecasts
all_ext_risk <- all_ext_risk %>%
  filter(forecast != 50)

# setting duration of tournament
startDate <- as.Date("2022-06-16 UTC")
phaseTwoMedian <- as.Date("2022-07-16")
endDate <- ymd("2022 10 31")

timeline <- seq(startDate, endDate, 1)

ext_risk_timeseries <- data.frame()

groupDateData <- function(group_data, group) {
  ## creates dataframe with
  ## summary statistics
  date_data <- group_data %>%
    summarize(
      median = median(forecast),
      sd = sd(forecast),
      n = n()
    ) %>%
    mutate(
      date = currentDate,
      Group = group
    ) %>%
    select(date, Group, n, median, sd)
  return(date_data)
}

# loop through all days of tournament
for (i in 1:length(timeline)) {
  currentDate <- timeline[i]
  if (i == 1 | (i %% 30) == 0) {
    print(currentDate)
  }
  
  # extracting only forecasts submitted by selected date
  dateDataRaw <- all_ext_risk %>% filter(timestamp < currentDate + 1)
  if (i == length(timeline)) {
    dateDataRaw <- all_ext_risk %>% filter(timestamp < currentDate + 2)
  }
  users <- unique(dateDataRaw$userId)
  users <- users[users %in% c(supers, experts$userId)]  # only superforecasters and experts
  
  dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

  # For each day (row), get most recent forecast from each user
  for (j in 1:length(users)) {
    user <- users[j]
    userForecasts <- dateDataRaw %>% filter(userId == user)

    mostRecentForecast <- filter(userForecasts, forecastId == max(userForecasts$forecastId))
    if (nrow(mostRecentForecast) > 1) {
      mostRecentForecast <- mostRecentForecast %>% filter(timestampId == max(mostRecentForecast$timestampId))
    }

    dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
  }
  supersData <- dateDataProcessed %>%
    filter(userId %in% supers)
  generalistData <- dateDataProcessed %>%
    filter(userId %in% generalists$userId)
  expertsData <- dateDataProcessed %>%
    filter(userId %in% experts$userId) %>%
    filter(!(userId %in% generalists$userId))
  rownames(ext_risk_timeseries) <- NULL
  date_data <- rbind(
    groupDateData(supersData, "Superforecasters"),
    groupDateData(expertsData, "Experts"),
    groupDateData(generalistData, "General X-risk Experts")
  )
  ext_risk_timeseries <- rbind(ext_risk_timeseries, date_data)
}



#### Plotting specifications ####
# Generate a colorblind-friendly palette with six colors
cb_pal <- colorblind_pal()(8)

# Exclude black from the palette
cb_pal <- tail(cb_pal, -1)

group_colors <- list(
  "Superforecasters" = cb_pal[1],
  "Domain Experts" = cb_pal[2],
  "Other Experts" = cb_pal[2],
  "General X-risk Experts" = cb_pal[3],
  "Non-domain Experts" = cb_pal[4],
  "Public Survey" = cb_pal[5],
  "Biorisk Experts" = cb_pal[2],
  "AI Experts" = cb_pal[2],
  "Climate Experts" = cb_pal[2],
  "Nuclear Experts" = cb_pal[2],
  "Experts" = cb_pal[2]
)


#### Figures 2-3 ####

#Figure 2
#Prep for chart, exclude unreliable (low n) median forecasts
ext_risk_timeseries <- ext_risk_timeseries %>%
  mutate(
    Group = factor(Group, levels = c("Experts", "General X-risk Experts", "Superforecasters"), ordered = TRUE),
    median = replace(median, n < 10 | (Group == "Non-domain Experts" & n < 4), NA)
  )

# Table for legend labels
group_counts <- ext_risk_timeseries %>%
  group_by(Group) %>%
  summarise(n = max(n))
legend_labels <- paste0(group_counts$Group, " (n = ", group_counts$n, ")")

plot <- ggplot(ext_risk_timeseries, aes(x = date, y = median, group = Group, color = Group, fill = Group)) +
  geom_line() +
  ylab("Median") +
  xlab("Date") +
  labs(title = "Total Extinction Risk by 2100", subtitle = "Forecasts over Time") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = unlist(group_colors), labels = legend_labels) +
  scale_fill_manual(values = unlist(group_colors)) +
  geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
  geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
  xlim(phaseTwoMedian, NA)
plot$labels$color <- ""

setwd("../figures")

ggsave("f2.png", plot, width = 9.18, height = 5.78, units = c("in"))

# Figure 3

# Filter and re-instate the levels now that the group names are correct
ext_risk_timeseries <- ext_risk_timeseries %>%
  mutate(
    sd = replace(sd, n < 10 | (Group == "Non-domain Experts" & n < 4), NA)
  ) %>%
  filter(date > ymd("2022 07 14"))

# Create percent variance column
ext_risk_timeseries <- ext_risk_timeseries %>%
  group_by(Group) %>%
  mutate(
    first_sd = first(sd),
    percent_variance = 100 * (sd / first_sd)
  )

plot <- ggplot(ext_risk_timeseries, aes(x = date, y = percent_variance, group = Group, color = Group)) +
  geom_line() +
  ylab("% of Initial SD") +
  xlab("Date") +
  labs(title = "Total Extinction Risk by 2100", subtitle = "Variance over Time") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
  geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
  scale_color_manual(values = unlist(group_colors), labels = legend_labels)
plot$labels$color <- ""

ggsave("f3.png", plot, width = 9.18, height = 5.78, units = c("in"))

#### Separating forecasts by question type ####

check_monotonicity <- function(q_forecasts){
  # require forecasts over quantiles to be monotonic
  users <- unique(q_forecasts$userId)
  exclude <- c()
  for (i in 1:length(users)) {
    user_forecasts <- q_forecasts %>%
      filter(userId == users[i]) %>%
      filter(isCurrent == TRUE)
    f_5th <- user_forecasts %>%
      filter(answerText == "5th %") %>%
      select(forecast) %>%
      unlist()
    f_25th <- user_forecasts %>%
      filter(answerText == "25th %") %>%
      select(forecast) %>%
      unlist()
    f_50th <- user_forecasts %>%
      filter(answerText == "50th %") %>%
      select(forecast) %>%
      unlist()
    f_75th <- user_forecasts %>%
      filter(answerText == "75th %") %>%
      select(forecast) %>%
      unlist()
    f_95th <- user_forecasts %>%
      filter(answerText == "95th %") %>%
      select(forecast) %>%
      unlist()
    if (!((f_5th <= f_25th) & (f_25th <= f_50th) & (f_50th <= f_75th) & (f_75th <= f_95th))) {
      exclude <- c(exclude, users[i])
    }
  }
  q_forecasts <- q_forecasts %>%
    filter(!(userId %in% exclude))
}

all_forecasts <- data.table(user = c(supers, experts$userId))

# getting only forecasts for questions of the multiYearReciprocal type
mYRTbl = data.table(meta %>% filter(questionType == "multiYearReciprocal"))
for(i in 1:nrow(mYRTbl)){
  print(mYRTbl$setName[i])
  years <- unlist(mYRTbl[i] %>% select(year1, year2, year3))
  years <- years[years != ""]
  for (j in 1:length(years)) {
    print(years[j])
    q_forecasts <- forecasts %>%
      filter(setName == mYRTbl$setName[i]) %>%
      filter(grepl("Your", questionName)) %>%
      filter(grepl(years[j], questionName)) %>%
      filter(isCurrent == TRUE) %>%
      filter(forecast != mYRTbl$defaultForecast50[i])
    shortName <- paste0("n", mYRTbl$setName[i], "_", years[j])
    shortName <- gsub(" ", "", shortName)
    shortName <- gsub("\\.", "_", shortName)
    all_forecasts[, shortName] <- as.numeric(NA)
    for (k in 1:nrow(all_forecasts)) {
      userForecast <- unlist(q_forecasts %>%
        filter(userId == all_forecasts$user[k]) %>%
        select(forecast))
      if (length(userForecast) > 0) {
        all_forecasts[k, shortName] <- unique(userForecast)
      }
    }
  }
}

# pointDistrib type
pDTbl <- meta %>% filter(questionType == "pointDistrib")
for (i in 1:nrow(pDTbl)) {
  print(pDTbl$setName[i])
  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")
  defaultForecasts <- c(pDTbl$defaultForecast5[i], pDTbl$defaultForecast25[i], pDTbl$defaultForecast50[i], pDTbl$defaultForecast75[i], pDTbl$defaultForecast95[i])
  q_forecasts_all <- forecasts %>%
    filter(setName == pDTbl$setName[i]) %>%
    filter(isCurrent == TRUE)
  q_forecasts_all <- check_monotonicity(q_forecasts_all)
  for (j in 1:length(distrib)) {
    print(distrib[j])
    q_forecasts <- q_forecasts_all %>%
      filter(answerText == distrib[j]) %>%
      filter(forecast != defaultForecasts[j])
    shortName <- paste0("n", pDTbl$setName[i], "_", distrib[j])
    shortName <- gsub(" ", "", shortName)
    shortName <- gsub("\\.", "_", shortName)
    shortName <- gsub("\\%", "", shortName)
    all_forecasts[, shortName] <- as.numeric(NA)
    for (k in 1:nrow(all_forecasts)) {
      userForecast <- unlist(q_forecasts %>%
        filter(userId == all_forecasts$user[k]) %>%
        select(forecast))
      if (length(userForecast) > 0) {
        all_forecasts[k, shortName] <- unique(userForecast)
      }
    }
  }
}

# multiYearDistrib type
mYDTbl <- meta %>% filter(questionType == "multiYearDistrib")
for (i in 1:nrow(mYDTbl)) {
  print(mYDTbl$setName[i])
  years <- c(mYDTbl$year1[i], mYDTbl$year2[i], mYDTbl$year3[i])
  years <- years[years != ""]
  for (j in 1:length(years)) {
    print(years[j])
    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")
    defaultForecasts <- c(mYDTbl$defaultForecast5[i], mYDTbl$defaultForecast25[i], mYDTbl$defaultForecast50[i], mYDTbl$defaultForecast75[i], mYDTbl$defaultForecast95[i])
    q_forecasts_all <- forecasts %>%
      filter(setName == mYDTbl$setName[i]) %>%
      filter(questionName == years[j]) %>%
      filter(isCurrent == TRUE)
    q_forecasts_all <- check_monotonicity(q_forecasts_all)
    for (k in 1:length(distrib)) {
      print(distrib[k])
      q_forecasts <- q_forecasts_all %>%
        filter(answerText == distrib[k]) %>%
        filter(forecast != defaultForecasts[k])
      shortName <- paste0("n", mYDTbl$setName[i], "_", years[j], "_", distrib[k])
      shortName <- gsub(" ", "", shortName)
      shortName <- gsub("\\.", "_", shortName)
      shortName <- gsub("\\%", "", shortName)
      shortName <- gsub("\\-", "", shortName)
      all_forecasts[, shortName] <- as.numeric(NA)
      for (l in 1:nrow(all_forecasts)) {
        userForecast <- unlist(q_forecasts %>%
          filter(userId == all_forecasts$user[l]) %>%
          select(forecast))
        if (length(userForecast) > 0) {
          all_forecasts[l, shortName] <- unique(userForecast)
        }
      }
    }
  }
}

# multiYearBinary type
mYBTbl <- meta %>% filter(questionType == "multiYearBinary")
for (i in 1:nrow(mYBTbl)) {
  print(mYBTbl$setName[i])
  years <- c(mYBTbl$year1[i], mYBTbl$year2[i], mYBTbl$year3[i])
  years <- years[years != ""]
  for (j in 1:length(years)) {
    print(years[j])
    q_forecasts <- forecasts %>%
      filter(setName == mYBTbl$setName[i]) %>%
      filter(grepl(years[j], questionName)) %>%
      filter(isCurrent == TRUE) %>%
      filter(forecast != mYBTbl$defaultForecast50[i])
    shortName <- paste0("n", mYBTbl$setName[i], "_", years[j])
    shortName <- gsub(" ", "", shortName)
    shortName <- gsub("\\.", "_", shortName)
    all_forecasts[, shortName] <- as.numeric(NA)
    for (k in 1:nrow(all_forecasts)) {
      userForecast <- unlist(q_forecasts %>%
        filter(userId == all_forecasts$user[k]) %>%
        select(forecast))
      if (length(userForecast) > 0) {
        all_forecasts[k, shortName] <- unique(userForecast)
      }
    }
  }
}

# multiYearCountryDistrib type
mYCDTbl <- meta %>% filter(questionType == "multiYearCountryDistrib")
for (i in 1:nrow(mYCDTbl)) {
  print(mYCDTbl$setName[i])
  years <- c(mYCDTbl$year1[i], mYCDTbl$year2[i], mYCDTbl$year3[i])
  years <- years[years != ""]
  for (j in 1:length(years)) {
    print(years[j])
    countries <- c(mYCDTbl$country1, mYCDTbl$country2, mYCDTbl$country3, mYCDTbl$country4, mYCDTbl$country5, mYCDTbl$country6, mYCDTbl$country7, mYCDTbl$country8, mYCDTbl$country9, mYCDTbl$country10, mYCDTbl$country11, mYCDTbl$country12)
    countries <- countries[countries != ""]
    for (k in 1:length(countries)) {
      print(countries[k])
      q_forecasts <- forecasts %>%
        filter(setName == mYCDTbl$setName[i]) %>%
        filter(grepl(years[j], questionName)) %>%
        filter(grepl(countries[k], questionName)) %>%
        filter(isCurrent == TRUE) %>%
        filter(forecast != mYCDTbl$defaultForecast50[i])
      shortName <- paste0("n", mYCDTbl$setName[i], "_", years[j], "_", countries[k])
      shortName <- gsub(" ", "", shortName)
      shortName <- gsub("\\.", "_", shortName)
      all_forecasts[, shortName] <- as.numeric(NA)
      for (l in 1:nrow(all_forecasts)) {
        userForecast <- unlist(q_forecasts %>%
          filter(userId == all_forecasts$user[l]) %>%
          select(forecast))
        if (length(userForecast) > 0) {
          all_forecasts[l, shortName] <- unique(userForecast)
        }
      }
    }
  }
}

# multiCountryBinary type
mCBTbl <- meta %>% filter(questionType == "multiCountryBinary")
for (i in 1:nrow(mCBTbl)) {
  print(mCBTbl$setName[i])
  countries <- c(mCBTbl$country1, mCBTbl$country2, mCBTbl$country3, mCBTbl$country4, mCBTbl$country5, mCBTbl$country6, mCBTbl$country7, mCBTbl$country8, mCBTbl$country9, mCBTbl$country10, mCBTbl$country11, mCBTbl$country12)
  countries <- countries[countries != ""]
  for (j in 1:length(countries)) {
    print(countries[j])
    q_forecasts <- forecasts %>%
      filter(setName == mCBTbl$setName[i]) %>%
      filter(answerText == countries[j]) %>%
      filter(isCurrent == TRUE) %>%
      filter(forecast != mCBTbl$defaultForecast50[i])
    shortName <- paste0("n", mCBTbl$setName[i], "_", countries[j])
    shortName <- gsub(" ", "", shortName)
    shortName <- gsub("\\.", "_", shortName)
    shortName <- gsub("\\-", "", shortName)
    all_forecasts[, shortName] <- as.numeric(NA)
    for (k in 1:nrow(all_forecasts)) {
      userForecast <- unlist(q_forecasts %>%
        filter(userId == all_forecasts$user[k]) %>%
        select(forecast))
      if (length(userForecast) > 0) {
        all_forecasts[k, shortName] <- unique(userForecast)
      }
    }
  }
}

# pointBinary type
pBTbl <- meta %>% filter(questionType == "pointBinary")
for (i in 1:nrow(pBTbl)) {
  print(pBTbl$setName[i])
  q_forecasts <- forecasts %>%
    filter(setName == pBTbl$setName[i]) %>%
    filter(isCurrent == TRUE) %>%
    filter(forecast != pBTbl$defaultForecast50[i])
  shortName <- paste0("n", pBTbl$setName[i])
  shortName <- gsub(" ", "", shortName)
  shortName <- gsub("\\.", "_", shortName)
  shortName <- gsub("\\%", "", shortName)
  all_forecasts[, shortName] <- as.numeric(NA)
  for (j in 1:nrow(all_forecasts)) {
    userForecast <- unlist(q_forecasts %>%
      filter(userId == all_forecasts$user[j]) %>%
      select(forecast))
    if (length(userForecast) > 0) {
      all_forecasts[j, shortName] <- unique(userForecast)
    }
  }
}


#### imputation ####

impute_tbl <- all_forecasts %>% select(-user)
impute_tbl <- as.data.frame(impute_tbl)

# find and impute columns with infinite standard deviations

sds = apply(impute_tbl, 2, sd, na.rm=TRUE)
sd_inf = sds[sds == Inf]

# iteratively topcode questions with infinite standard deviation

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.95, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.95, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.9, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.9, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.85, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.85, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.8, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.8, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.75, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.75, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.7, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.7, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.65, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.65, na.rm = TRUE))
}

sds <- apply(impute_tbl, 2, sd, na.rm = TRUE)
sd_inf <- sds[sds == Inf]

for (i in 1:length(sd_inf)) {
  colName <- names(sd_inf[i])
  impute_tbl[, colName][impute_tbl[, colName] > quantile(impute_tbl[, colName], 0.6, na.rm = TRUE) & !is.na(impute_tbl[, colName])] <- as.numeric(quantile(impute_tbl[, colName], 0.6, na.rm = TRUE))
}

sds = apply(impute_tbl, 2, sd, na.rm=TRUE)
sd_inf = sds[sds == Inf]

# dealing with individual columns/rows of extreme and missing data

impute_tbl <- cbind(data.frame(user = all_forecasts$user), impute_tbl)

near_complete <- apply(impute_tbl, 1, function(x) sum(is.na(x)))
close_indices <- which(near_complete == min(near_complete))

colnames(impute_tbl[73, ])[is.na(impute_tbl[73, ])]

tbl_for_distances <- impute_tbl %>% select(-user)

distances <- c()

for (i in 1:nrow(tbl_for_distances)) {
  currRow <- tbl_for_distances[i, ]
  distance <- (sqrt(sum((tbl_for_distances[73, ] - currRow)^2, na.rm = TRUE)))
  distances <- c(distances, distance)
}

indices_for_shortest_nuclear <- which(distances %in% sort(distances)[2:31])
shortest_distances_data_nuclear <- tbl_for_distances[indices_for_shortest_nuclear, ]

impute_tbl[73, ]$n27_NuclearFusionEnergy_5th <- median(shortest_distances_data_nuclear$n27_NuclearFusionEnergy_5th, na.rm = TRUE)

indices_for_shortest_autocracy <- which(distances %in% sort(distances)[2:43])
shortest_distances_data_autocracy <- tbl_for_distances[indices_for_shortest_autocracy, ]

impute_tbl[73, ]$n57_PrevalenceofAutocracies_2050_75th <- median(shortest_distances_data_autocracy$n57_PrevalenceofAutocracies_2050_75th, na.rm = TRUE)

indices_for_shortest_nkbiowar <- which(distances %in% sort(distances)[2:43])
shortest_distances_data_nkbiowar <- tbl_for_distances[indices_for_shortest_nkbiowar, ]

impute_tbl[73, ]$n20_IndividualCountrieswithBiologicalWeaponsPrograms_2030_NorthKorea <- median(shortest_distances_data_nkbiowar$n20_IndividualCountrieswithBiologicalWeaponsPrograms_2030_NorthKorea, na.rm = TRUE)

#log weird cols
impute_tbl$n11_YearofExtinction_25th <- log(impute_tbl$n11_YearofExtinction_25th)
impute_tbl$n11_YearofExtinction_50th <- log(impute_tbl$n11_YearofExtinction_50th)
impute_tbl$n12_FutureHumanBirths_75th <- log(impute_tbl$n12_FutureHumanBirths_75th)
impute_tbl$n12_FutureHumanBirths_95th <- log(impute_tbl$n12_FutureHumanBirths_95th)
impute_tbl$n43_NYTBestsellersWrittenbyAI_95th <- log(impute_tbl$n43_NYTBestsellersWrittenbyAI_95th)
# impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_5th <- log(impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_5th)
# impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_25th <- log(impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_25th)
# impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_50th <- log(impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_50th)
impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_75th <- log(impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_75th)
impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_95th <- log(impute_tbl$n13_NonCoronavirusmRNAVaccine_2030_95th)
impute_tbl$`n37_USComputerR&DSpending_2024_5th` <- log(impute_tbl$`n37_USComputerR&DSpending_2024_5th`)
impute_tbl$`n37_USComputerR&DSpending_2024_25th` <- log(impute_tbl$`n37_USComputerR&DSpending_2024_25th`)
impute_tbl$`n37_USComputerR&DSpending_2024_50th` <- log(impute_tbl$`n37_USComputerR&DSpending_2024_50th`)
impute_tbl$`n37_USComputerR&DSpending_2024_75th` <- log(impute_tbl$`n37_USComputerR&DSpending_2024_75th`)
impute_tbl$`n37_USComputerR&DSpending_2024_95th` <- log(impute_tbl$`n37_USComputerR&DSpending_2024_95th`)
impute_tbl$`n37_USComputerR&DSpending_2030_5th` <- log(impute_tbl$`n37_USComputerR&DSpending_2030_5th`)
impute_tbl$`n37_USComputerR&DSpending_2030_25th` <- log(impute_tbl$`n37_USComputerR&DSpending_2030_25th`)
impute_tbl$`n37_USComputerR&DSpending_2030_50th` <- log(impute_tbl$`n37_USComputerR&DSpending_2030_50th`)
impute_tbl$`n37_USComputerR&DSpending_2030_75th` <- log(impute_tbl$`n37_USComputerR&DSpending_2030_75th`)
impute_tbl$`n37_USComputerR&DSpending_2030_95th` <- log(impute_tbl$`n37_USComputerR&DSpending_2030_95th`)
impute_tbl$`n37_USComputerR&DSpending_2050_5th` <- log(impute_tbl$`n37_USComputerR&DSpending_2050_5th`)
impute_tbl$`n37_USComputerR&DSpending_2050_25th` <- log(impute_tbl$`n37_USComputerR&DSpending_2050_25th`)
impute_tbl$`n37_USComputerR&DSpending_2050_50th` <- log(impute_tbl$`n37_USComputerR&DSpending_2050_50th`)
impute_tbl$`n37_USComputerR&DSpending_2050_75th` <- log(impute_tbl$`n37_USComputerR&DSpending_2050_75th`)
impute_tbl$`n37_USComputerR&DSpending_2050_95th` <- log(impute_tbl$`n37_USComputerR&DSpending_2050_95th`)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_5th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_5th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_25th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_25th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_50th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_50th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_75th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_75th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_95th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2024_95th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_5th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_5th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_25th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_25th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_50th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_50th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_75th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_75th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_95th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2030_95th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_5th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_5th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_25th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_25th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_50th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_50th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_75th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_75th)
impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_95th <- log(impute_tbl$n45_MaximumComputeUsedinanAIExperiment_2050_95th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2024_5th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2024_5th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2024_25th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2024_25th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2024_50th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2024_50th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2024_75th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2024_75th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2024_95th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2024_95th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2030_5th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2030_5th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2030_25th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2030_25th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2030_50th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2030_50th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2030_75th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2030_75th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2030_95th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2030_95th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2050_5th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2050_5th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2050_25th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2050_25th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2050_50th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2050_50th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2050_75th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2050_75th)
impute_tbl$n46_LargestAIExperimentCostofCompute_2050_95th <- log(impute_tbl$n46_LargestAIExperimentCostofCompute_2050_95th)
impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_5th <- log(impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_5th)
impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_25th <- log(impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_25th)
impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_50th <- log(impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_50th)
impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_75th <- log(impute_tbl$n49_LargestNumberofParametersinaMachineLearningModel_2024_75th)

complete <- impute_tbl[complete.cases(impute_tbl), ]
incomplete <- impute_tbl[!complete.cases(impute_tbl), ]

incomplete <- cbind(incomplete, data.frame(NAs = as.numeric(rowSums(is.na(incomplete)))))
incomplete <- arrange(incomplete, NAs)
incomplete <- select(incomplete, -NAs)

userOrder <- c(complete$user, incomplete$user)

complete <- select(complete, -user)
incomplete <- select(incomplete, -user)

imputed <- complete

set.seed(1)
for (i in 1:nrow(incomplete)) {
  imputed <- rbind(imputed, incomplete[i, ])
  if (i > 3) {
    k <- 3
  } else {
    k <- i
  }
  preProcValues <- preProcess(imputed,
    method = c("knnImpute"),
    k = k,
    knnSummary = mean
  )
  impute_k1 <- predict(preProcValues, newdata = imputed, na.action = na.pass)

  procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
  for (j in procNames$col) {
    impute_k1[j] <- impute_k1[j] * preProcValues$std[j] + preProcValues$mean[j]
  }
  imputed <- impute_k1
}

imputed <- cbind(data.frame(user = userOrder), imputed)

imputed[imputed < 0] <- 0
imputed <- imputed[match(impute_tbl$user, imputed$user), ]

# exp weird cols
imputed$n11_YearofExtinction_25th <- exp(imputed$n11_YearofExtinction_25th)
imputed$n11_YearofExtinction_50th <- exp(imputed$n11_YearofExtinction_50th)
imputed$n12_FutureHumanBirths_75th <- exp(imputed$n12_FutureHumanBirths_75th)
imputed$n12_FutureHumanBirths_95th <- exp(imputed$n12_FutureHumanBirths_95th)
imputed$n43_NYTBestsellersWrittenbyAI_95th <- exp(imputed$n43_NYTBestsellersWrittenbyAI_95th)
# imputed$n13_NonCoronavirusmRNAVaccine_2030_5th <- exp(imputed$n13_NonCoronavirusmRNAVaccine_2030_5th)
# imputed$n13_NonCoronavirusmRNAVaccine_2030_25th <- exp(imputed$n13_NonCoronavirusmRNAVaccine_2030_25th)
# imputed$n13_NonCoronavirusmRNAVaccine_2030_50th <- exp(imputed$n13_NonCoronavirusmRNAVaccine_2030_50th)
imputed$n13_NonCoronavirusmRNAVaccine_2030_75th <- exp(imputed$n13_NonCoronavirusmRNAVaccine_2030_75th)
imputed$n13_NonCoronavirusmRNAVaccine_2030_95th <- exp(imputed$n13_NonCoronavirusmRNAVaccine_2030_95th)
imputed$`n37_USComputerR&DSpending_2024_5th` <- exp(imputed$`n37_USComputerR&DSpending_2024_5th`)
imputed$`n37_USComputerR&DSpending_2024_25th` <- exp(imputed$`n37_USComputerR&DSpending_2024_25th`)
imputed$`n37_USComputerR&DSpending_2024_50th` <- exp(imputed$`n37_USComputerR&DSpending_2024_50th`)
imputed$`n37_USComputerR&DSpending_2024_75th` <- exp(imputed$`n37_USComputerR&DSpending_2024_75th`)
imputed$`n37_USComputerR&DSpending_2024_95th` <- exp(imputed$`n37_USComputerR&DSpending_2024_95th`)
imputed$`n37_USComputerR&DSpending_2030_5th` <- exp(imputed$`n37_USComputerR&DSpending_2030_5th`)
imputed$`n37_USComputerR&DSpending_2030_25th` <- exp(imputed$`n37_USComputerR&DSpending_2030_25th`)
imputed$`n37_USComputerR&DSpending_2030_50th` <- exp(imputed$`n37_USComputerR&DSpending_2030_50th`)
imputed$`n37_USComputerR&DSpending_2030_75th` <- exp(imputed$`n37_USComputerR&DSpending_2030_75th`)
imputed$`n37_USComputerR&DSpending_2030_95th` <- exp(imputed$`n37_USComputerR&DSpending_2030_95th`)
imputed$`n37_USComputerR&DSpending_2050_5th` <- exp(imputed$`n37_USComputerR&DSpending_2050_5th`)
imputed$`n37_USComputerR&DSpending_2050_25th` <- exp(imputed$`n37_USComputerR&DSpending_2050_25th`)
imputed$`n37_USComputerR&DSpending_2050_50th` <- exp(imputed$`n37_USComputerR&DSpending_2050_50th`)
imputed$`n37_USComputerR&DSpending_2050_75th` <- exp(imputed$`n37_USComputerR&DSpending_2050_75th`)
imputed$`n37_USComputerR&DSpending_2050_95th` <- exp(imputed$`n37_USComputerR&DSpending_2050_95th`)
imputed$n45_MaximumComputeUsedinanAIExperiment_2024_5th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2024_5th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2024_25th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2024_25th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2024_50th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2024_50th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2024_75th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2024_75th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2024_95th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2024_95th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2030_5th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2030_5th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2030_25th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2030_25th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2030_50th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2030_50th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2030_75th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2030_75th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2030_95th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2030_95th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2050_5th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2050_5th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2050_25th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2050_25th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2050_50th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2050_50th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2050_75th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2050_75th)
imputed$n45_MaximumComputeUsedinanAIExperiment_2050_95th <- exp(imputed$n45_MaximumComputeUsedinanAIExperiment_2050_95th)
imputed$n46_LargestAIExperimentCostofCompute_2024_5th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2024_5th)
imputed$n46_LargestAIExperimentCostofCompute_2024_25th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2024_25th)
imputed$n46_LargestAIExperimentCostofCompute_2024_50th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2024_50th)
imputed$n46_LargestAIExperimentCostofCompute_2024_75th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2024_75th)
imputed$n46_LargestAIExperimentCostofCompute_2024_95th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2024_95th)
imputed$n46_LargestAIExperimentCostofCompute_2030_5th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2030_5th)
imputed$n46_LargestAIExperimentCostofCompute_2030_25th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2030_25th)
imputed$n46_LargestAIExperimentCostofCompute_2030_50th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2030_50th)
imputed$n46_LargestAIExperimentCostofCompute_2030_75th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2030_75th)
imputed$n46_LargestAIExperimentCostofCompute_2030_95th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2030_95th)
imputed$n46_LargestAIExperimentCostofCompute_2050_5th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2050_5th)
imputed$n46_LargestAIExperimentCostofCompute_2050_25th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2050_25th)
imputed$n46_LargestAIExperimentCostofCompute_2050_50th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2050_50th)
imputed$n46_LargestAIExperimentCostofCompute_2050_75th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2050_75th)
imputed$n46_LargestAIExperimentCostofCompute_2050_95th <- exp(imputed$n46_LargestAIExperimentCostofCompute_2050_95th)
imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_5th <- exp(imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_5th)
imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_25th <- exp(imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_25th)
imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_50th <- exp(imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_50th)
imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_75th <- exp(imputed$n49_LargestNumberofParametersinaMachineLearningModel_2024_75th)

imputed_data <- imputed


#### Figures 4, 5, A4 ####

prefixes <- c("4. AI Extinction Risk", "6. Nuclear Extinction Risk")
risks <- gsub("^(\\d|\\d{2})\\.\ ", "", prefixes)

# getting Stage 1 individual forecasts for questions 4 and 6 by 2100
for(i in 1:length(prefixes)){
  risk_table <- forecasts %>%
    filter(setName == prefixes[i]) %>%
    filter(grepl("Your", questionName)) %>%
    filter(grepl("2100", questionName)) %>%
    filter(stage == 1) %>%
    filter(forecast != 50)
  unique_ids <- unique(risk_table$userId)
  tbl_update <- data.frame()
  for (j in 1:length(unique_ids)) {
    tbl_update <- rbind(
      tbl_update,
      user_forecasts <- risk_table %>%
        filter(userId == unique_ids[j]) %>%
        filter(timestampId == max(timestampId))
    )
  }
  # sorting table by forecasts
  risk_table <- tbl_update
  risk_table <- risk_table %>%
    arrange(-forecast) %>%
    filter(userId %in% imputed_data$user)

  imputed_data <- imputed_data %>% filter(user %in% risk_table$userId)
  
  # categorizing forecasts into top, middle, bottom 1/3 forecasts
  top3rd_AIrisk <- risk_table$userId[1:round((1/3)*nrow(risk_table))]
  middle3rd_AIrisk <- risk_table$userId[(round((1/3)*nrow(risk_table))+1):round((2/3)*nrow(risk_table))]
  bottom3rd_AIrisk <- risk_table$userId[(round((2/3)*nrow(risk_table))+1):nrow(risk_table)]
  
  # extracting specific data for each category of analysis
  data_for_analysis <- imputed_data %>%
    select(-contains("25th")) %>%
    select(-contains("75th")) %>%
    select(-contains("95th")) %>%
    select(-contains("5th")) %>%
    select(-n11_YearofExtinction_50th) %>%
    select(-n12_FutureHumanBirths_50th) %>%
    select(-n27_NuclearFusionEnergy_50th) %>%
    select(-n42_AIWinsInternationalMathematicalOlympiad_50th) %>%
    select(-n43_NYTBestsellersWrittenbyAI_50th) %>%
    select(-n44_DateofAdvancedAI_50th) %>%
    select(-n53_YearofGDPGrowthover15_50th) %>%
    select(-n58_FutureWorriesandChildren_50th) %>%
    select(-n59_GenerationAttitudes_50th)

  data_for_analysis_AI <- data_for_analysis %>%
    select(
      user,
      contains("AICatastrophicRisk") |
        contains("AIExtinctionRisk") |
        contains("GPTRevenue") |
        contains("USGDPFromSoftware") |
        contains("USComputerR.DSpending") |
        contains("LaborForceParticipationRateinOECD") |
        contains("MATHDatasetBenchmark") |
        contains("MassiveMultitaskLanguageUnderstanding") |
        contains("QuALITYDatasetBenchmark") |
        contains("MaximumComputeUsedinanAIExperiment") |
        contains("LargestAIExperimentCostofCompute") |
        contains("LowestPriceofGFLOPS") |
        contains("ImageNetClassificationTrainingEfficiency") |
        contains("LargestNumberofParametersinaMachineLearningModel") |
        contains("NegativePublicOpinionofAI") |
        contains("NickBostromAffirmsExistenceofAGI") |
        contains("ProbabilityofGDPGrowthOver15")
    )

  data_for_analysis_nonAI <- data_for_analysis %>%
    select(
      user,
      !contains("AICatastrophicRisk") &
        !contains("AIExtinctionRisk") &
        !contains("GPTRevenue") &
        !contains("USGDPFromSoftware") &
        !contains("USComputerR.DSpending") &
        !contains("LaborForceParticipationRateinOECD") &
        !contains("MATHDatasetBenchmark") &
        !contains("MassiveMultitaskLanguageUnderstanding") &
        !contains("QuALITYDatasetBenchmark") &
        !contains("MaximumComputeUsedinanAIExperiment") &
        !contains("LargestAIExperimentCostofCompute") &
        !contains("LowestPriceofGFLOPS") &
        !contains("ImageNetClassificationTrainingEfficiency") &
        !contains("LargestNumberofParametersinaMachineLearningModel") &
        !contains("NegativePublicOpinionofAI") &
        !contains("NickBostromAffirmsExistenceofAGI") &
        !contains("ProbabilityofGDPGrowthOver15")
    )

  y2024 <- data_for_analysis %>%
    select(
      user,
      contains("2024"),
      n35_GPTRevenue
    )
  y2030 <- data_for_analysis %>%
    select(
      user,
      contains("2030"),
      contains("n34_CountrybyCountryNuclearUse")
    )
  y2050 <- data_for_analysis %>%
    select(
      user,
      contains("2050")
    )
  y2100 <- data_for_analysis %>%
    select(
      user,
      contains("2100"),
      n52_ProbabilityofGDPGrowthOver15
    )

  y2024_AI <- data_for_analysis_AI %>%
    select(
      user,
      contains("2024"),
      n35_GPTRevenue
    )
  y2030_AI <- data_for_analysis_AI %>%
    select(
      user,
      contains("2030"),
      contains("n34_CountrybyCountryNuclearUse")
    )
  y2050_AI <- data_for_analysis_AI %>%
    select(
      user,
      contains("2050")
    )
  y2100_AI <- data_for_analysis_AI %>%
    select(
      user,
      contains("2100"),
      n52_ProbabilityofGDPGrowthOver15
    )

  y2024_nonAI <- data_for_analysis_nonAI %>%
    select(
      user,
      contains("2024")
    )
  y2030_nonAI <- data_for_analysis_nonAI %>%
    select(
      user,
      contains("2030"),
      contains("n34_CountrybyCountryNuclearUse")
    )
  y2050_nonAI <- data_for_analysis_nonAI %>%
    select(
      user,
      contains("2050")
    )
  y2100_nonAI <- data_for_analysis_nonAI %>%
    select(
      user,
      contains("2100")
    )


  create_tbl <- function(user, year, year_table, group) {
    # creates table with distance from median forecast in SD units
    # averaged over all forecasts for each user
    distance_tbl <- data.table(
      user = user,
      distance = NA,
      year = year,
      group = group,
      forecast = NA
    )
    for (i in 1:nrow(distance_tbl)) {
      print(paste(i, "/", nrow(distance_tbl)))
      forecasts <- year_table %>%
        filter(user == distance_tbl$user[i])
      distances <- c()
      for (j in 2:ncol(forecasts)) {
        median <- median(year_table[, j])
        sd <- sd(year_table[, j])
        user_forecast <- forecasts[, j]
        distance_from_median <- abs(user_forecast - median) / sd
        distances <- c(distances, distance_from_median)
      }
      distances <- distances[!is.nan(distances)]
      avg_distance <- mean(distances)
      distance_tbl$distance[i] <- avg_distance
      distance_tbl$forecast[i] <- risk_table[risk_table$userId == distance_tbl$user[i], ]$forecast
    }
    return(distance_tbl)
  }

  # create average forecating distance table with each concern category/year
  graphTbl <- create_tbl(
    user = top3rd_AIrisk,
    year = 2024,
    year_table = y2024,
    group = "most concerned"
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = top3rd_AIrisk,
      year = 2030,
      year_table = y2030,
      group = "most concerned"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = top3rd_AIrisk,
      year = 2050,
      year_table = y2050,
      group = "most concerned"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = top3rd_AIrisk,
      year = 2100,
      year_table = y2100,
      group = "most concerned"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = middle3rd_AIrisk,
      year = 2024,
      year_table = y2024,
      group = "middle"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = middle3rd_AIrisk,
      year = 2030,
      year_table = y2030,
      group = "middle"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = middle3rd_AIrisk,
      year = 2050,
      year_table = y2050,
      group = "middle"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = middle3rd_AIrisk,
      year = 2100,
      year_table = y2100,
      group = "middle"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = bottom3rd_AIrisk,
      year = 2024,
      year_table = y2024,
      group = "least concerned"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = bottom3rd_AIrisk,
      year = 2030,
      year_table = y2030,
      group = "least concerned"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = bottom3rd_AIrisk,
      year = 2050,
      year_table = y2050,
      group = "least concerned"
    )
  )
  graphTbl <- bind_rows(
    graphTbl,
    create_tbl(
      user = bottom3rd_AIrisk,
      year = 2100,
      year_table = y2100,
      group = "least concerned"
    )
  )

  # getting median distance statistic for each group
  medians <- graphTbl %>%
    group_by(group, year) %>%
    summarize(rounded_median = round(median(distance), 2))

  names(medians) <- c("group", "year", "distance")

  median_b <- function(x, i) {
    median(x[i], na.rm = TRUE)
  }

  bootstrap_ci <- function(x) {
    # bootstrapped confidence intervals
    b <- boot(data = x, statistic = median_b, R = 1000)
    ci <- boot.ci(b, type = c("norm"))
    ci <- as.numeric(ci$normal[2:3])
    if (ci[1] < 0) {
      ci[1] <- 0
    }
    if (ci[2] > 100) {
      ci[2] <- 100
    }
    return(data.frame(ymin = ci[1], ymax = ci[2], y = mean(x)))
  }


  set.seed(1)
  # figure 4-5
  p <- ggplot(graphTbl, aes(x = group, y = distance, fill = factor(year))) +
    stat_summary(fun.data = bootstrap_ci, geom = "errorbar", aes(color = factor(year)), position = position_dodge(width = 0.75), width = 0.5) +
    labs(x = "Group", y = "Absolute Distance of Forecast from Median (in SD)") +
    scale_fill_discrete(name = "Resolution Date") +
    labs(title = paste0("Extremity of Forecast by ", risks[i], " Concern vs. Resolution Date")) +
    geom_label(
      data = medians, aes(label = distance),
      position = position_dodge2(width = 0.75, preserve = "single"),
      size = 3
    ) +
    guides(color = guide_legend("Resolution Date"), fill = "none") +
    theme_bw()
  
  filenames_f45 <- c(
    "Nuclear Extinction Risk" = "f5.png",
    "AI Extinction Risk" = "f4.png"
  )
  
  # Default if risks[i] is not in filenames_f45
  filename <- filenames_f45[[risks[i]]] %||%
    paste0("Extremity of Forecast by ", risks[i], " Concern vs. Resolution Date.png")
  
  ggsave(filename, p, units = "px", width = 2466, height = 1642)

  # figure A4
  p_byforecast_loess <- ggplot(graphTbl, aes(x = forecast, y = distance, color = factor(year))) +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE) + # Add regression lines
    labs(
      title = paste0("Extremity of Forecast by ", risks[i], " Concern vs. Resolution Date"),
      x = "Forecast",
      y = "Absolute Distance of Forecast from Median (in SD)"
    ) +
    scale_color_manual(values = c("2024" = "red", "2030" = "blue", "2050" = "green", "2100" = "purple"), name = "Resolution Date") +
    # theme_minimal() +
    scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100)) +
    theme_bw()

  if (risks[i] == "AI Extinction Risk") {
    ggsave(paste0("fa4.png"), p_byforecast_loess, units = "px", width = 2466, height = 1642)
  }
}


#### Figure 6, A5, A8 ####

setwd("../figures")
# Generate summary table w/ supers median and domain experts median
source("summary-table.R")
# Get stage 1 RS ranks
source("rs_ranks.R")
# Quintile plots
source("rs_quintiles_plots.R")

#### Figures A1-A3 ####

setwd("../figures")
source("boxplots.R")

#### Figures A6-A7

setwd("../figures")
source("correlation-plots.R")