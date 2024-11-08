library(ggthemes)
setwd("../data")

# Import public survey
survey <- read.csv("public_survey.csv")
names(survey) <- gsub("\\.", " ", names(survey))
survey <- survey %>%
  rename(
    "Total Extinction Risk" = `Total extinction risk`,
    "Total Catastrophic Risk" = `Total catastrophic risk`,
    "AI Extinction Risk" = `AI extinction`
  )

# Get AI experts
AI_experts <- experts %>%
  rowwise() %>%
  filter("AI" %in% c(specialty1, specialty2, specialty3))

get_final_forecasts <- function(set) {
  # Get relevant forecasts from set
  set_forecasts <- forecasts %>%
    filter(grepl(set, setName)) %>%
    filter(grepl("2100", questionName)) %>%
    filter(grepl("Your", questionName)) %>%
    # Exclude default forecasts
    filter(forecast != 50) %>%
    # Make sure only current forecasts are getting in (if final forecast is default)
    filter(isCurrent == TRUE)
  # Get unique users from set forecasts
  users <- unique(set_forecasts$userId)
  final_forecasts <- data.frame()
  # Loop through users to get final forecast from each
  for (i in 1:length(users)) {
    user_forecast <- set_forecasts %>%
      filter(userId == users[i]) %>%
      filter(timestampId == max(timestampId))
    final_forecasts <- rbind(final_forecasts, user_forecast)
  }
  return(final_forecasts)
}

get_colors <- function() {
  # Generate a colorblind-friendly palette with six colors
  cb_pal <- colorblind_pal()(8)
  # Exclude black from the palette
  cb_pal <- tail(cb_pal, -1)
  # Assign colors to groups
  group_colors <- list(
    "Superforecasters" = cb_pal[1],
    "Experts" = cb_pal[2],
    "Risk Experts" = cb_pal[3],
    "Non-domain Experts" = cb_pal[4],
    "Public Survey" = cb_pal[5],
    "AI Experts" = cb_pal[2]
  )
  return(group_colors)
}

create_box_plot <- function(set, filename) {
  # Get final forecasts
  tbl <- get_final_forecasts(set) %>%
    # Add super/expert/risk experts
    mutate(group = case_when(
      userId %in% supers ~ "Superforecasters",
      userId %in% generalists$userId ~ "Risk Experts",
      userId %in% experts$userId ~ "Experts"
    ))
  # Add in AI experts if AI set
  if (grepl("AI", set)) {
    tbl <- tbl %>%
      mutate(group = case_when(userId %in% AI_experts$userId ~ "AI Experts",
        group == "Experts" ~ "Non-domain Experts",
        .default = group
      ))
  }
  tbl <- tbl %>%
    select(forecast, group)
  # Get public survey data
  public_forecasts <- survey %>%
    select(all_of(set)) %>%
    rename(forecast = all_of(set)) %>%
    mutate(group = "Public Survey") %>%
    # Get rid of NAs and non-probabilities
    filter(!is.na(forecast)) %>%
    filter(forecast >= 0 & forecast <= 100)
  tbl <- rbind(tbl, public_forecasts)
  # Order groups and handle AI cases
  # Add in AI experts if AI set
  if (!grepl("AI", set)) {
    group_order <- c(
      "Superforecasters",
      "Experts",
      "Risk Experts",
      "Public Survey"
    )
  } else {
    group_order <- c(
      "Superforecasters",
      "AI Experts",
      "Non-domain Experts",
      "Risk Experts",
      "Public Survey"
    )
  }
  tbl$group <- factor(tbl$group, levels = group_order, ordered = TRUE)
  # Get medians
  rounded_medians <- aggregate(forecast ~ group, median, data = tbl)
  # Assign colors
  group_colors <- get_colors()
  # Plot
  boxPlot <- ggplot(tbl, aes(x = group, y = forecast, color = group)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Probability") +
    xlab("Group") +
    labs(title = set, subtitle = "2100") +
    theme_bw() +
    scale_color_manual(values = unlist(group_colors)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none",
      axis.title.x = element_blank()
    ) +
    geom_point(position = position_jitterdodge())
  # Add (n=numrows) for the x-axis labels
  boxPlot <- boxPlot +
    scale_x_discrete(
      labels = function(x) {
        x <- as.character(x)
        paste0(x, " (n=", table(tbl$group)[x], ")")
      },
      guide = guide_axis(n.dodge = 2)
    )
  # Resize scale
  boxPlot <- boxPlot +
    scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
  # Add medians
  boxPlot <- boxPlot +
    stat_summary(
      fun.y = median, geom = "label",
      data = rounded_medians,
      aes(label = round(..y.., 2)),
      position = position_dodge2(width = 0.75, preserve = "single"),
      vjust = 0.5,
      size = 3,
      fill = "white",
      show.legend = FALSE
    )
  # Ensure that y-axis fits into 95th percentile of tournament participant cases
  tournamentParticipants_95thpctile <- tbl %>%
    filter(group != "Public Survey") %>%
    group_by(group) %>%
    summarize(percentile_95 = quantile(forecast, 0.95))
  boxPlot <- boxPlot +
    coord_cartesian(ylim = c(NA, max(tournamentParticipants_95thpctile$percentile_95)))
  # Save plot with XPT specifications
  setwd("../figures")
  ggsave(filename, boxPlot, width = 9.18, height = 5.78, units = c("in"))
  setwd("../data")
}

needed_sets <- c("Total Extinction Risk", "Total Catastrophic Risk", "AI Extinction Risk")
filenames <- c("fa1.png", "fa2.png", "fa3.png")

for (i in 1:length(needed_sets)) {
  create_box_plot(
    set = needed_sets[i],
    filename = filenames[i]
  )
}
