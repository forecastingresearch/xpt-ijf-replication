library(dplyr)
setwd("data")

# load all the survey and participant data
data <- read.csv("survey.csv")
supers <- read.csv("supers.csv")
experts <- read.csv("experts.csv")

# classify participants
data <- data %>%
  mutate(Group = ifelse(userId %in% supers$x, "Superforecasters", "Experts"))

# estimate proportion of participants spending x hours on existential risk by group
ordered_levels <- c("0-50 hours", "50-200 hours", "200-1,000 hours", "1,000+ hours", "Did not answer")
data$Hours.on.X.risk <- factor(data$Hours.on.X.risk, levels = ordered_levels)

time_spent_on_xrisk <- data %>%
  group_by(Group, Hours.on.X.risk) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(Group) %>%
  mutate(percentage = count / sum(count)) %>%
  select(Group, Hours.on.X.risk, percentage)

setwd("../misc")
write.csv(time_spent_on_xrisk, "time_spent_on_xrisk.csv", row.names=FALSE)

# estimate proportion of participants who attended an EA meetup by group
ordered_levels <- c("Attended an EA meetup", "Has not attended an EA meetup", "Did not answer")
data$EAMeetup <- factor(data$EAMeetup, levels = ordered_levels)

EA_meetup <- data %>%
  group_by(Group, EAMeetup) %>%
  summarize(count=n()) %>%
  ungroup() %>%
  group_by(Group) %>%
  mutate(percentage = count / sum(count)) %>%
  select(Group, EAMeetup, percentage)

write.csv(EA_meetup, "EA_meetup.csv", row.names=FALSE)
