# RunSetup ----------------------------------------------------------------
# Run Setup script
source("SetupNew.R")
source("DiagnosticsData.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300

# Diagnostic restoration -------------------------------------------------------------

# Dataframe
DiagRestorationData <-
  Diagnostics |> 
  filter(`Metric` %in% c("Diagnostics Restoration"))

# Graphic
DiagRestorationPlot <-
  ggplot(data = DiagRestorationData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('SOM',
                                       'SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "SOM" = NHSBlue)) +
  geom_hline(yintercept = 1.2,
             size = 0.7,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.7,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.7,
                                1.5),
                     expand = c(0,
                                0),
                     labels = percent_format(accuracy = 1)) +
  scale_x_date(breaks= seq(min(DiagRestorationData$Date), max(DiagRestorationData$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Diagnostic Restoration"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Diagnostics dataset, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Diagnostics/DiagRestorationPlot.png"),
       plot = DiagRestorationPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Diagnostic restoration -------------------------------------------------------------

# Dataframe
DiagPerfData <-
  Diagnostics |> 
  filter(`Metric` %in% c("Diagnostics Less6"))

# Graphic
DiagPerfPlot <-
  ggplot(data = DiagPerfData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0.99,
             size = 0.7,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0.75,
             size = 0.7,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.7,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.4,
                                1),
                     expand = c(0,
                                0),
                     labels = percent_format(accuracy = 1)) +
  scale_x_date(breaks= seq(min(DiagPerfData$Date), max(DiagPerfData$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Diagnostic 6 Week Waits Performance"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("Data source: Diagnostics dataset, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Diagnostics/DiagPerfPlot.png"),
       plot = DiagPerfPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")
