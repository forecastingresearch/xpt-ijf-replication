# creates box plots from the paper

yourHome <<- "/path/to/xpt-ijf-replication/"

#### Set-up ####
rm(list = ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(aggutils)

setwd(yourHome)

source("functions.R")

if (!dir.exists(paste0(yourHome, "data/Summary Data"))) {
   dir.create(paste0(yourHome, "data/Summary Data"))
 }

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

#### Analysis choices ####

selectSets <- c("10. Total Extinction Risk",
                "9. Total Catastrophic Risk",
                "4. AI Extinction Risk")

selectYears <- c("year3")

selectStages <- c(1, 4)

#### Analysis for each question ####

setwd(paste0(yourHome, "data/Summary Data"))

if ("summaryTable.csv" %in% list.files()) {
  summaryTable <- read.csv("summaryTable.csv")
} else {
  summaryTable <- newAddInit()
}
write.csv(summaryTable, "summaryTable.csv", row.names = FALSE)

# computes summary statistics of data in each of the selected question sets
for (i in 1:length(questions)) {
  if (questions[i] %in% selectSets) {
    metaTable <- meta %>% filter(setName == questions[i])
    newAdd <- multiYearReciprocal(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
}

setwd(paste0(yourHome, "data/Summary Data"))
write.csv(summaryTable, "summaryTable.csv", row.names = FALSE)

setwd(yourHome)


