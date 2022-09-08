# Run Setup script
source("Setup.R")

# Emergency Department (Commissioner) Plot ----
# Dataframe
Emergency12aSomCommData <-
  Emergency12aSQLData |>
  CommPlotGroup()

# Graphic
Emergency12aSomCommPlot <-
  ggplot(data = Emergency12aSomCommData,
         aes(x = `MonthDate`,
             y = `Activity`,
             colour = factor(`CommissionerDescription`,
                             level = c('Somerset')),
             fill = factor(`CommissionerDescription`,
                           level = c('Somerset')))) +
  geom_area(alpha = 0.2) +
  geom_line(size = 1) +
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSBlue)) +
  scale_fill_manual("legend",
                    values = c("Somerset" = NHSBlue)) +
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 4))) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_x_date(breaks = c(seq(from = min(Emergency12aSomCommData$`MonthDate`),
                              to = max(Emergency12aSomCommData$`MonthDate`),
                              by = "6 months")),
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                15000),
                     expand = c(0,
                                0),
                     labels = comma) +
  labs(title = paste("Emergency Department - Commissioner Total"),
       subtitle = paste("Snapshot at month end of Somerset patients"),
       caption = paste("Source: SUS, updated on",
                       SystemTime)) +
  LinePlotTheme()