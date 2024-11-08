# does reciprocal scoring analysis
# need to run probability-analysis.R first
yourHome <<- "/path/to/xpt-ijf-replication/"
rm(list = ls())

library(data.table)
library(dplyr)

setwd(yourHome)

source("functions.R")

# selecting the setNames to include in analysis
selectSets <- c("10. Total Extinction Risk",
                "9. Total Catastrophic Risk",
                "4. AI Extinction Risk")

# selecting the years to include in analysis
selectYears <- c("year3")

# selecting the stages of the tournament to include in analysis
selectStages <- c(1, 4)

#### Loading data ####

setwd(paste0(yourHome, "data"))

meta <- fread("metadata.csv")
questions <- unique(meta$setName)

data <- fread("forecasts.csv")
data <- assign_user_id(data)
data$forecast <- as.numeric(data$forecast)
data$setName[grep("40.", data$setName)] <- unique(meta$setName[grep("40.", meta$setName)])

supers <- fread("supers.csv")
supers <- supers$x

expertsG1 <- fread("experts.csv")
expertsG1 <- assign_user_id(expertsG1)

#### Setting up files to hold results ####

setwd(paste0(yourHome, "data/Summary Data"))

if ("RSTable.csv" %in% list.files()) {
  RSTable <- read.csv("RSTable.csv")
} else {
  RSTable <- RSInit()
  write.csv(RSTable, "RSTable.csv", row.names = FALSE)
}

if ("RSRanking_unincentivized.csv" %in% list.files()) {
  RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")
} else {
  RSRanking_unincentivized <- RSRankingInit()
  write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)
}

# sumaryTable.csv contains summary statistics calculated in probability-analysis.R
summaryTable <- read.csv("summaryTable.csv")

#### Reciprocal scoring analysis ####

metaTable <- meta %>% filter(questionType == "multiYearReciprocal")
multiYearReciprocal_RS(metaTable, data, summaryTable)

RSTable <- read.csv("RSTable.csv")
RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")
RSRanking_unincentivized <- RSRanking_unincentivized %>% mutate(avgRank = rankSum / numQuestions)
write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

setwd(yourHome)

