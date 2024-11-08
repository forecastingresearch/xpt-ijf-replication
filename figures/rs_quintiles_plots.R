library(ncar, include.only = "Round")

rs_quintile_plot <- function(tbl, title, subtitle) {
  #' Boxplot for RS quintiles
  #'
  #' @importFrom ncar Round
  #' @export

  # plot <- ggplot(tbl, aes(x = quintile, y = forecast, group = quintile)) +
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

#### Plotting preferences and selections ####

# Generate a colorblind-friendly palette with six colors
cb_pal <- colorblind_pal()(8)

# Exclude black from the palette
cb_pal <- tail(cb_pal, -1)

# assign colors to the groups
group_colors <- list(
  "Superforecasters" = cb_pal[1],
  "Domain Experts" = cb_pal[2],
  "Other Experts" = cb_pal[2],
  "Risk Experts" = cb_pal[3],
  "Non-domain Experts" = cb_pal[4],
  "Public Survey" = cb_pal[5],
  "Biorisk Experts" = cb_pal[2],
  "AI Experts" = cb_pal[2],
  "Climate Experts" = cb_pal[2],
  "Nuclear Experts" = cb_pal[2],
  "Experts" = cb_pal[2]
)

# selecting the setNames to include in analysis
selectSets <- c(
  "10. Total Extinction Risk",
  "9. Total Catastrophic Risk",
  "4. AI Extinction Risk"
)

# selecting the years to include in analysis
selectYears <- c("year3")

# selecting the stages of the tournament to include in analysis
selectStages <- c(4)

#### Loading in analysis results ####

rs_rank <- RSRanking_unincentivized

# Keep only those who have forecast on at least 30 questions and are G1 experts or supers
rs_rank <- rs_rank %>%
  filter(numQuestions >= 30) %>%
  filter(group %in% c("supers", "experts"))

# Merge experts with rs_rank by userId
rs_rank <- merge(rs_rank, experts, by = "userId", all.x = TRUE)

# Give the experts who have "General" as their specialty a new group: "X-risk generalists"
rs_rank <- rs_rank %>%
  mutate(
    specialty1 = ifelse(is.na(specialty1), "", specialty1),
    specialty2 = ifelse(is.na(specialty2), "", specialty2),
    specialty3 = ifelse(is.na(specialty3), "", specialty3)
  ) %>%
  mutate(group = ifelse(specialty1 == "General" | specialty2 == "General" | specialty3 == "General", "Risk Experts", group)) %>%
  mutate(
    group = ifelse(group == "experts", "Other Experts", group),
    group = ifelse(group == "supers", "Superforecasters", group)
  )

rs_rank <- arrange(rs_rank, avgRank)

# We use some of these to construct the quintile groups in a moment
d_20th <- round(0.2 * nrow(rs_rank))
d_25th <- round(0.25 * nrow(rs_rank))
d_40th <- round(0.4 * nrow(rs_rank))
d_50th <- round(0.5 * nrow(rs_rank))
d_60th <- round(0.6 * nrow(rs_rank))
d_75th <- round(0.75 * nrow(rs_rank))
d_80th <- round(0.8 * nrow(rs_rank))

q1 <- rs_rank[1:d_20th, ]
q2 <- rs_rank[(d_20th + 1):d_40th, ]
q3 <- rs_rank[(d_40th + 1):d_60th, ]
q4 <- rs_rank[(d_60th + 1):d_80th, ]
q5 <- rs_rank[(d_80th + 1):nrow(rs_rank), ]

# loop through each question set of interest
for (i in 1:length(unique(sets_for_summary$setName))) {
  currentSetName <- unique(sets_for_summary$setName)[i]
  if (currentSetName %in% selectSets) {
    print(currentSetName)
    years <- sets_for_summary[i, ] %>% select(all_of(selectYears))
    years <- as.character(years)
    years <- years[years != ""]
    beliefSets <- sets_for_summary[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]
    # loop through each stage of interest
    for (j in 1:4) {
      currentStage <- j
      if (currentStage %in% selectStages) {
        print(paste("Stage:", currentStage))
        for (k in 1:length(years)) {
          print(years[k])
          for (l in 1:length(beliefSets)) {
            print(beliefSets[l])
            csv <- forecasts %>%
              filter(setName == currentSetName) %>%
              filter(grepl(years[k], questionName)) %>%
              filter(grepl("Your", questionName)) %>%
              filter(isCurrent == TRUE) %>%
              filter(forecast != 50)
            # This filters out those people we filtered out before for having too few forecasts
            csv <- csv %>% filter(userId %in% rs_rank$userId)

            tbl <- data.frame(
              userType = character(0),
              forecast = numeric(0),
              quintile = character(0)
            )

            # Loop over the people in the first quintile
            for (m in 1:nrow(q1)) {
              currentId <- q1$userId[m]
              forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast

              if (length(forecast) > 0) {
                tbl <- rbind(tbl, data.frame(
                  userType = q1[m, ]$group,
                  forecast = forecast,
                  quintile = "Q1"
                ))
              }
            }

            # Loop over the people in the second quintile
            for (m in 1:nrow(q2)) {
              currentId <- q2$userId[m]
              forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast

              if (length(forecast) > 0) {
                tbl <- rbind(tbl, data.frame(
                  userType = q2[m, ]$group,
                  forecast = forecast,
                  quintile = "Q2"
                ))
              }
            }

            # Loop over the people in the third quintile
            for (m in 1:nrow(q3)) {
              currentId <- q3$userId[m]
              forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast

              if (length(forecast) > 0) {
                tbl <- rbind(tbl, data.frame(
                  userType = q3[m, ]$group,
                  forecast = forecast,
                  quintile = "Q3"
                ))
              }
            }

            # Loop over the people in the fourth quintile
            for (m in 1:nrow(q4)) {
              currentId <- q4$userId[m]
              forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast

              if (length(forecast) > 0) {
                tbl <- rbind(tbl, data.frame(
                  userType = q4[m, ]$group,
                  forecast = forecast,
                  quintile = "Q4"
                ))
              }
            }

            # Loop over the people in the fifth quintile
            for (m in 1:nrow(q5)) {
              currentId <- q5$userId[m]
              forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast

              if (length(forecast) > 0) {
                tbl <- rbind(tbl, data.frame(
                  userType = q5[m, ]$group,
                  forecast = forecast,
                  quintile = "Q5"
                ))
              }
            }

            # Get the name of the value in tbl$group that contains "uper"
            superstr <- tbl$userType[grepl("uper", tbl$userType)][1]
            # Ditto "eneral" (for generalists)
            generalstr <- tbl$userType[grepl("isk Experts", tbl$userType)][1]
            # Get the name of the value in tbl$group that contains "xpert" BUT NOT "eneral"
            expertstr <- tbl$userType[grepl("xpert", tbl$userType) & !grepl("eneral", tbl$userType)][1]

            # We turn into factor so that graph orders quintiles correctly
            tbl$quintile <- factor(tbl$quintile, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"), ordered = TRUE)

            # Ditto for userType
            tbl$userType <- factor(tbl$userType, levels = c(superstr, generalstr, expertstr), ordered = TRUE)

            # Split title at the period (after the question number) and remove the space at the beginning
            title <- gsub("^\\d+\\.\\s", "", paste(currentSetName, "by", years[k]))

            subtitle <- paste("Stage", j, "-", "Reciprocal Scoring Accuracy")

            plot <- rs_quintile_plot(tbl, title, subtitle)

            if (currentSetName == "4. AI Extinction Risk") {
              filename <- "fa5.png"
            } else if (currentSetName == "9. Total Catastrophic Risk") {
              filename <- "fa8.png"
            } else {
              filename <- "f6.png"
            }

            ggsave(filename, plot, width = 3600, height = 2000, units = c("px"))
          }
        }
      }
    }
  }
}
