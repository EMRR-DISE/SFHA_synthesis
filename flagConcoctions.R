# full remainders, no
# only repeats, no
# remainder min, no
# remainder min/mean, no
# diff3 itself, no
# diff2, diff3, no
# remainder max/mean, no
# diff2, no


# Works
# value, diff, diff2, diff3, outlierMax ; 23
# value, diff, diff2, diff3, outlierMin, outlierMax, outlierMean ; 24
# value, diff2, outlierMax ; 40
# value, diff, diff2, outlierMax ; 41
# value, diff, diff2, diff3 ; 42
# value, diff2, diff3, outlierMin, outlierMax, outlierMean ; 30
# value, diff3, outlierMin, outlierMax, outlierMean ; 29
# value, outlierMin, outlierMax, outlierMean ; 14
# value, diff, diff2, diff3, outlierMax, outlierMean ; 18
# diff, diff2, diff3, outlierMax, outlierMean ; 38
# diff, diff2, diff3, outlierMin ; 17
# diff2, diff3, outlierMin ; 25 
# diff, diff3, outlierMax ; 27
# diff, diff2, diff3, outlierMax ; 28
# value, diff, diff2, diff3, outlierMin ; 43

# value, outlierMin, outlierMax, outlierMean
# value, diff2, diff3, outlierMin, outlierMax, outlierMean
# value, repeats, outlierMin, max, mean
# value, diff, diff2, diff3, max, mean
# value, diff, diff2, diff3, min, max, mean
# value, diff, diff2, diff3, min
# diff, diff2, diff3, min **, diff might need to be higher

# Check
# diff, diff2, diff3 ; 15
# value ; 39

# Not full
# diff, diff3, outlierMax
# diff, diff2, diff3, outlierMin



# interestedIndex <- c(23, 24, 40, 41, 42, 30, 29, 14, 18, 38, 17, 25, 27, 28, 43)
# i <- interestedIndex[[2]]
# 
# whatPlotDf <-
#   what[[1]] %>% 
#   filter(stationId == "ANH", sensorNumber == 4,
#          outString %in% differentGroups[c(1, interestedIndex), 1],
#          outlierSum > 0
#   ) %>% 
#   mutate(randomized = sample(nrow(.))) %>% 
#   arrange(randomized) %>% 
#   slice(1:10) %>% 
#   arrange(dateTime) %>% 
#   transmute(dateTime,
#             start = dateTime - lubridate::days(10),
#             end = dateTime + lubridate::days(10)) %>% 
#   group_split(dateTime) %>% 
#   lapply(., function(range) {
#     what[[1]] %>% 
#       filter(stationId == "ANH", sensorNumber == 4,
#              outString %in% differentGroups[c(1, interestedIndex), 1],
#              between(dateTime, range$start, range$end)
#       )
#   })
# 
# graphics.off()
# lapply(1:length(whatPlotDf), function(t) {
#   whatPlotDf[[t]] %>% 
#     {
#       ggplot() +
#         geom_point(data = filter(., outString == differentGroups[1, 1]), 
#                    aes(dateTime, valueOriginal), color = "grey", alpha = 0.5) +
#         geom_point(data = filter(., outString == differentGroups[i, 1]), 
#                    aes(dateTime, valueOriginal, color = differentGroups[i, 2]), size = 2) +
#         scale_color_manual(values = c("firebrick")) +
#         labs(color = "outlierGroup") +
#         theme(legend.position = "bottom")
#     }
# }
# )
# 
# what[[1]] %>% 
#   filter(stationId == "ANH", sensorNumber == 4,
#          outString %in% differentGroups[c(1, interestedIndex), 1],
#   ) %>% 
#   {
#     ggplot() +
#       # geom_point(data = filter(., outString == differentGroups[1, 1]), 
#       #            aes(dateTime, valueOriginal), color = "grey", alpha = 0.2) +
#       geom_ribbon(
#         data = filter(., outString == differentGroups[1, 1]) %>% 
#           group_by(., dateTime = floor_date(dateTime, "week")) %>% 
#           summarise(valueMin = min(valueOriginal, na.rm = T),
#                     valueMax = max(valueOriginal, na.rm = T)),
#         aes(dateTime, ymin = valueMin, ymax = valueMax),
#         fill = "skyblue", # Choose a fill color
#         alpha = 0.5      # Set transparency
#       ) +
#       geom_point(data = filter(., outString %in% differentGroups[interestedIndex, 1]),
#                  aes(dateTime, valueOriginal, color = outString), size = 2) +
#       # scale_color_manual(values = c("firebrick")) +
#       labs(color = "outlierGroup") +
#       theme(legend.position = "bottom")
#   }
# 
# p <- what[[1]] %>% 
#   filter(stationId == "ANH", sensorNumber == 4
#   ) %>% 
#   left_join(
#     differentGroups, by = "outString"
#   ) %>% 
#   {
#     ggplot() +
#       geom_ribbon(
#         data = filter(., outString == differentGroups[1, 1]) %>% 
#           group_by(., dateTime = floor_date(dateTime, "week")) %>% 
#           summarise(valueMin = min(valueOriginal, na.rm = T),
#                     valueMax = max(valueOriginal, na.rm = T)),
#         aes(dateTime, ymin = valueMin, ymax = valueMax),
#         fill = "skyblue", # Choose a fill color
#         alpha = 0.5      # Set transparency
#       ) +
#       geom_point(data = filter(., outString != differentGroups[1, 1]),
#                  aes(dateTime, valueOriginal, color = parsedString), size = 2) +
#       theme(legend.position = "none")
#   }

library(plotly)
i=1
d <- outlierData %>% 
  filter(stationId == keyGroup$stationId[i], sensorNumber == keyGroup$sensorNumber[i],
         # between(dateTime, as.POSIXct("2013-01-16 00:00:00"), as.POSIXct("2013-01-17 00:00:00"))
  ) %>% 
  left_join(
    differentGroups, by = "outString"
  ) %>% 
  mutate(wantedGroups = parsedString %in% concoction) %>% 
  # filter(parsedString %in% concoction) %>% 
  highlight_key(~wantedGroups)

p <- ggplot(data = d) +
  geom_ribbon(
    data = outlierData %>%
      filter(stationId == keyGroup$stationId[i], sensorNumber == keyGroup$sensorNumber[i]
      ) %>%
      left_join(
        differentGroups, by = "outString"
      ) %>%
      filter(., outString %in% outlierNoneGroup) %>%
      group_by(., dateTime = floor_date(dateTime, "week")) %>%
      summarise(valueMin = min(value, na.rm = T),
                valueMax = max(value, na.rm = T)),
    aes(dateTime, ymin = valueMin, ymax = valueMax),
    fill = "skyblue", # Choose a fill color
    alpha = 0.5      # Set transparency
  ) +
  geom_point(aes(dateTime, value, group = parsedString, color = wantedGroups)) +
  theme(legend.position = "none")+
  coord_cartesian(ylim = c(0, 50))
# p
gg <- ggplotly(
  p,
  # tooltip = "parsedString" # Tell ggplotly to primarily use the 'text' aesthetic for hover info
) %>%
  # Hide the potentially large and unreadable legend
  layout(showlegend = FALSE)
highlight(gg, on = "plotly_click", off = "plotly_deselect", color = "black")

what <- outlierData %>% 
  filter(stationId == "ANH", sensorNumber == 4
  ) %>% 
  left_join(
    differentGroups, by = "outString"
  ) %>% 
  mutate(weeklyDateTime = floor_date(dateTime, "week")) %>% 
  left_join(
    outlierData %>% 
      filter(stationId == "ANH", sensorNumber == 4
      ) %>% 
      left_join(
        differentGroups, by = "outString"
      ) %>% 
      filter(., outString %in% outlierNoneGroup) %>% 
      group_by(., dateTime = floor_date(dateTime, "week")) %>% 
      summarise(valueMin = min(value, na.rm = T),
                valueMax = max(value, na.rm = T), .groups = "drop"),
    by = c("weeklyDateTime" = "dateTime")
  ) %>% 
  mutate(beyondWeeklyHull = value < valueMin | value > valueMax)
